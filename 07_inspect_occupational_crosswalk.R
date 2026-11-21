
# EMPATHY PREMIUM RESEARCH
# SCRIPT 07: INSPECT SOC-ISCO CROSSWALK

library(tidyverse)
library(haven)
library(here)

# 1. Locate the downloaded crosswalk
crosswalk_path <- here(
  "data", "raw", "crosswalk",
  "soc10_isco08.dta"
)

# 2. Verify file availability
if (!file.exists(crosswalk_path)) {
  stop("Crosswalk file not found. Check its folder and filename.")
}

# 3. Import Stata dataset
soc_isco <- read_dta(crosswalk_path)

# 4. Inspect variables
cat("\n--- COLUMN NAMES ---\n")
print(names(soc_isco))

cat("\n--- DATA STRUCTURE ---\n")
glimpse(soc_isco)

cat("\n--- FIRST 10 OBSERVATIONS ---\n")
print(head(soc_isco, 10))

# 5. Check mapping coverage
cat("\n--- MAPPING SUMMARY ---\n")

mapping_summary <- soc_isco %>%
  summarise(
    total_rows = n(),
    unique_soc_codes = n_distinct(soc10),
    unique_isco_codes = n_distinct(isco08),
    missing_soc = sum(is.na(soc10)),
    missing_isco = sum(is.na(isco08))
  )

print(mapping_summary)

# 6. Identify one-to-many mappings
mapping_structure <- soc_isco %>%
  group_by(soc10) %>%
  summarise(
    isco_matches = n_distinct(isco08),
    .groups = "drop"
  )

print(
  count(mapping_structure, isco_matches),
  n = Inf
)

message("Crosswalk inspection completed.")
