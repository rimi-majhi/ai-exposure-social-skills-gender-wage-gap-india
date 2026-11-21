
# EMPATHY PREMIUM RESEARCH
# SCRIPT 08: AUDIT O*NET TRANSITION

library(tidyverse)
library(here)

# 1. Import transition crosswalk
transition <- read_csv(
  here(
    "data", "raw", "crosswalk",
    "2010_to_2019_Crosswalk.csv"
  ),
  show_col_types = FALSE
)

# 2. Import constructed SSI dataset
ssi <- read_csv(
  here(
    "data", "processed",
    "onet_social_skills_index.csv"
  ),
  show_col_types = FALSE
)

# 3. Standardise column names
transition_clean <- transition %>%
  transmute(
    onet_soc_2010 = `O*NET-SOC 2010 Code`,
    title_2010 = `O*NET-SOC 2010 Title`,
    onet_soc_2019 = `O*NET-SOC 2019 Code`,
    title_2019 = `O*NET-SOC 2019 Title`
  )

# 4. Identify 2010 occupations
# that map to multiple 2019 occupations

split_mapping <- transition_clean %>%
  group_by(onet_soc_2010, title_2010) %>%
  summarise(
    number_of_2019_codes =
      n_distinct(onet_soc_2019),
    .groups = "drop"
  ) %>%
  filter(number_of_2019_codes > 1)

# 5. Identify 2019 occupations
# that combine multiple 2010 occupations

rollup_mapping <- transition_clean %>%
  group_by(onet_soc_2019, title_2019) %>%
  summarise(
    number_of_2010_codes =
      n_distinct(onet_soc_2010),
    .groups = "drop"
  ) %>%
  filter(number_of_2010_codes > 1)

# 6. Check SSI occupational coverage

ssi_codes <- ssi %>%
  distinct(onet_soc)

crosswalk_codes <- transition_clean %>%
  distinct(onet_soc_2019)

unmatched_ssi <- anti_join(
  ssi_codes,
  crosswalk_codes,
  by = c("onet_soc" = "onet_soc_2019")
)

# 7. Display results

cat("\n--- TRANSITION SUMMARY ---\n")

cat(
  "Total records:",
  nrow(transition_clean), "\n"
)

cat(
  "Unique 2010 occupations:",
  n_distinct(transition_clean$onet_soc_2010), "\n"
)

cat(
  "Unique 2019 occupations:",
  n_distinct(transition_clean$onet_soc_2019), "\n"
)

cat("\n--- OCCUPATIONAL SPLITS ---\n")
cat("Number of 2010 codes splitting:",
    nrow(split_mapping), "\n")

print(head(split_mapping, 15), n = 15)

cat("\n--- OCCUPATIONAL ROLL-UPS ---\n")
cat("Number of 2019 codes with multiple predecessors:",
    nrow(rollup_mapping), "\n")

print(head(rollup_mapping, 15), n = 15)

cat("\n--- SSI COVERAGE ---\n")

cat(
  "Total SSI occupations:",
  nrow(ssi_codes), "\n"
)

cat(
  "Unmatched SSI occupations:",
  nrow(unmatched_ssi), "\n"
)

print(unmatched_ssi, n = Inf)

# 8. Save audit results

dir.create(
  here("outputs", "tables"),
  recursive = TRUE,
  showWarnings = FALSE
)

write_csv(
  split_mapping,
  here("outputs", "tables",
       "onet_occupational_splits.csv")
)

write_csv(
  rollup_mapping,
  here("outputs", "tables",
       "onet_occupational_rollups.csv")
)

write_csv(
  unmatched_ssi,
  here("outputs", "tables",
       "unmatched_ssi_occupations.csv")
)

message("O*NET transition audit completed.")
