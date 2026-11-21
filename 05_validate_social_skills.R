
# EMPATHY PREMIUM RESEARCH
# Script 05: Validate social skill measurements

library(tidyverse)
library(readxl)
library(here)

# 1. Define the six official O*NET social skills

core_social_skills <- c(
  "Coordination",
  "Instructing",
  "Negotiation",
  "Persuasion",
  "Service Orientation",
  "Social Perceptiveness"
)

# 2. Import Transferable Skills data

transferable <- read_excel(
  here("data", "raw", "onet", "Transferable Skills.xlsx")
)

# 3. Extract Importance ratings

social_im <- transferable %>%
  filter(
    `Element Name` %in% core_social_skills,
    `Scale ID` == "IM"
  ) %>%
  transmute(
    onet_soc = `O*NET-SOC Code`,
    occupation = Title,
    skill = `Element Name`,
    importance = as.numeric(`Data Value`),
    sample_n = as.numeric(N),
    standard_error = as.numeric(`Standard Error`),
    suppress_flag = `Recommend Suppress`,
    not_relevant_flag = `Not Relevant`
  )

# 4. Check the six skills are present

skills_found <- sort(unique(social_im$skill))

missing_skills <- setdiff(
  core_social_skills,
  skills_found
)

cat("\nMISSING SKILLS:\n")
print(missing_skills)

# 5. Check occupation coverage and missing values

coverage_audit <- social_im %>%
  group_by(skill) %>%
  summarise(
    occupations = n_distinct(onet_soc),
    observations = n(),
    missing_ratings = sum(is.na(importance)),
    suppressed = sum(suppress_flag == "Y", na.rm = TRUE),
    not_relevant = sum(not_relevant_flag == "Y", na.rm = TRUE),
    minimum = min(importance, na.rm = TRUE),
    maximum = max(importance, na.rm = TRUE),
    .groups = "drop"
  )

cat("\nCOVERAGE AUDIT:\n")
print(coverage_audit, n = Inf)

# 6. Check duplicate occupation-skill records

duplicates <- social_im %>%
  count(onet_soc, skill) %>%
  filter(n != 1)

cat("\nDUPLICATE RECORDS:\n")
print(duplicates, n = Inf)

# 7. Check completeness by occupation

occupation_audit <- social_im %>%
  group_by(onet_soc, occupation) %>%
  summarise(
    skills_available = n_distinct(skill[!is.na(importance)]),
    .groups = "drop"
  )

cat("\nOCCUPATION COMPLETENESS:\n")
print(count(occupation_audit, skills_available))

# 8. Save audit files

dir.create(
  here("outputs"),
  showWarnings = FALSE,
  recursive = TRUE
)

write_csv(
  coverage_audit,
  here("outputs", "social_skill_coverage_audit.csv")
)

write_csv(
  duplicates,
  here("outputs", "social_skill_duplicates.csv")
)

write_csv(
  occupation_audit,
  here("outputs", "social_skill_occupation_audit.csv")
)

message("Social skill validation completed.")
