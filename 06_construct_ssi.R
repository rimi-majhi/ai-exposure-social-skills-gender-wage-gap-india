
# EMPATHY PREMIUM RESEARCH
# SCRIPT 06: CONSTRUCT SOCIAL SKILLS INTENSITY INDEX

library(tidyverse)
library(readxl)
library(here)

# -------------------------------------------
# 1. Define the six core social skills
# -------------------------------------------

core_social_skills <- c(
  "Coordination",
  "Instructing",
  "Negotiation",
  "Persuasion",
  "Service Orientation",
  "Social Perceptiveness"
)

# -------------------------------------------
# 2. Import O*NET Transferable Skills
# -------------------------------------------

transferable <- read_excel(
  here("data", "raw", "onet", "Transferable Skills.xlsx")
)

# -------------------------------------------
# 3. Extract Importance ratings
# -------------------------------------------

social_data <- transferable %>%
  filter(
    `Element Name` %in% core_social_skills,
    `Scale ID` == "IM"
  ) %>%
  transmute(
    onet_soc = `O*NET-SOC Code`,
    occupation = Title,
    skill = `Element Name`,
    importance = as.numeric(`Data Value`)
  )

# -------------------------------------------
# 4. Verify data quality
# -------------------------------------------

if (anyNA(social_data$importance)) {
  stop("Missing Importance ratings detected.")
}

if (anyDuplicated(social_data[c("onet_soc", "skill")])) {
  stop("Duplicate occupation-skill records detected.")
}

if (any(!social_data$skill %in% core_social_skills)) {
  stop("Unexpected skill detected.")
}

occupation_counts <- social_data %>%
  count(onet_soc)

if (any(occupation_counts$n != 6)) {
  stop("Some occupations do not have all six skills.")
}

# -------------------------------------------
# 5. Convert long data into wide format
# -------------------------------------------

social_wide <- social_data %>%
  pivot_wider(
    names_from = skill,
    values_from = importance
  )

# -------------------------------------------
# 6. Calculate the raw SSI
# -------------------------------------------

social_index <- social_wide %>%
  mutate(
    SSI_raw = rowMeans(
      pick(all_of(core_social_skills))
    )
  )

# -------------------------------------------
# 7. Standardise the SSI
# -------------------------------------------

social_index <- social_index %>%
  mutate(
    SSI_z = as.numeric(scale(SSI_raw))
  ) %>%
  arrange(desc(SSI_raw))

# -------------------------------------------
# 8. Generate summary statistics
# -------------------------------------------

ssi_summary <- social_index %>%
  summarise(
    occupations = n(),
    mean_SSI = mean(SSI_raw),
    sd_SSI = sd(SSI_raw),
    minimum_SSI = min(SSI_raw),
    maximum_SSI = max(SSI_raw),
    mean_z = mean(SSI_z),
    sd_z = sd(SSI_z)
  )

# -------------------------------------------
# 9. Display results
# -------------------------------------------

cat("\n--- SSI SUMMARY ---\n")
print(ssi_summary)

cat("\n--- TOP 10 SOCIAL SKILL INTENSIVE OCCUPATIONS ---\n")

print(
  social_index %>%
    select(onet_soc, occupation, SSI_raw, SSI_z) %>%
    slice_head(n = 10),
  n = 10
)

cat("\n--- BOTTOM 10 OCCUPATIONS ---\n")

print(
  social_index %>%
    select(onet_soc, occupation, SSI_raw, SSI_z) %>%
    slice_tail(n = 10),
  n = 10
)

# -------------------------------------------
# 10. Export the research dataset
# -------------------------------------------

dir.create(
  here("data", "processed"),
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  here("outputs", "tables"),
  recursive = TRUE,
  showWarnings = FALSE
)

write_csv(
  social_index,
  here("data", "processed", "onet_social_skills_index.csv")
)

write_csv(
  ssi_summary,
  here("outputs", "tables", "ssi_summary_statistics.csv")
)

message("Social Skills Intensity Index successfully constructed.")

