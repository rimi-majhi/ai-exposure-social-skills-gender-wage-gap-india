
# EMPATHY PREMIUM RESEARCH
# SCRIPT 09: PROVISIONAL OCCUPATIONAL MAPPING

library(tidyverse)
library(haven)
library(here)

# 1. Import the SSI dataset (O*NET-SOC 2019)

ssi <- read_csv(
  here("data", "processed",
       "onet_social_skills_index.csv"),
  show_col_types = FALSE
) %>%
  transmute(
    onet_soc_2019 = onet_soc,
    occupation_2019 = occupation,
    SSI_raw,
    SSI_z
  )

# 2. Import O*NET 2010-2019 transition

transition <- read_csv(
  here("data", "raw", "crosswalk",
       "2010_to_2019_Crosswalk.csv"),
  show_col_types = FALSE
) %>%
  transmute(
    onet_soc_2010 = `O*NET-SOC 2010 Code`,
    title_2010 = `O*NET-SOC 2010 Title`,
    onet_soc_2019 = `O*NET-SOC 2019 Code`
  )

# 3. Import SOC 2010 to ISCO-08 mapping

soc_isco <- read_dta(
  here("data", "raw", "crosswalk",
       "soc10_isco08.dta")
) %>%
  transmute(
    soc10 = str_pad(
      as.character(as.integer(soc10)),
      width = 6,
      side = "left",
      pad = "0"
    ),
    isco08 = as.character(as.integer(isco08))
  )

# 4. Link SSI occupations to their 2010 predecessors

ssi_to_2010 <- ssi %>%
  left_join(
    transition,
    by = "onet_soc_2019"
  )

# 5. Convert extended O*NET-SOC 2010
# codes to their six-digit SOC 2010 parent

ssi_to_soc <- ssi_to_2010 %>%
  mutate(
    soc10 = if_else(
      !is.na(onet_soc_2010),
      str_sub(
        str_remove_all(onet_soc_2010, "[^0-9]"),
        1, 6
      ),
      NA_character_
    )
  )

# 6. Link SOC 2010 to ISCO-08

provisional_mapping <- ssi_to_soc %>%
  left_join(
    soc_isco,
    by = "soc10"
  )

# 7. Audit coverage

coverage_summary <- tibble(
  SSI_occupations = n_distinct(ssi$onet_soc_2019),
  
  matched_to_2010 = n_distinct(
    provisional_mapping$onet_soc_2019[
      !is.na(provisional_mapping$onet_soc_2010)
    ]
  ),
  
  unmatched_2010 = n_distinct(
    provisional_mapping$onet_soc_2019[
      is.na(provisional_mapping$onet_soc_2010)
    ]
  ),
  
  matched_to_SOC2010 = n_distinct(
    provisional_mapping$onet_soc_2019[
      !is.na(provisional_mapping$soc10)
    ]
  ),
  
  matched_to_ISCO08 = n_distinct(
    provisional_mapping$onet_soc_2019[
      !is.na(provisional_mapping$isco08)
    ]
  )
)

cat("\n--- PROVISIONAL MAPPING COVERAGE ---\n")
print(coverage_summary)

# 8. Count mappings per occupation

mapping_complexity <- provisional_mapping %>%
  group_by(onet_soc_2019, occupation_2019) %>%
  summarise(
    number_of_2010_codes =
      n_distinct(onet_soc_2010, na.rm = TRUE),
    
    number_of_SOC_codes =
      n_distinct(soc10, na.rm = TRUE),
    
    number_of_ISCO_codes =
      n_distinct(isco08, na.rm = TRUE),
    
    .groups = "drop"
  )

cat("\n--- MAPPING COMPLEXITY ---\n")

print(
  mapping_complexity %>%
    count(number_of_ISCO_codes),
  n = Inf
)

# 9. Identify unmatched records

unmatched_transition <- provisional_mapping %>%
  filter(is.na(onet_soc_2010)) %>%
  distinct(onet_soc_2019, occupation_2019)

unmatched_isco <- provisional_mapping %>%
  filter(
    !is.na(onet_soc_2010),
    is.na(isco08)
  ) %>%
  distinct(
    onet_soc_2019,
    occupation_2019,
    onet_soc_2010,
    soc10
  )

# 10. Save provisional mapping and audits

dir.create(
  here("outputs", "tables"),
  recursive = TRUE,
  showWarnings = FALSE
)

write_csv(
  provisional_mapping,
  here("outputs", "tables",
       "provisional_onet_isco_mapping.csv")
)

write_csv(
  coverage_summary,
  here("outputs", "tables",
       "provisional_mapping_coverage.csv")
)

write_csv(
  mapping_complexity,
  here("outputs", "tables",
       "provisional_mapping_complexity.csv")
)

write_csv(
  unmatched_transition,
  here("outputs", "tables",
       "unmatched_transition_occupations.csv")
)

write_csv(
  unmatched_isco,
  here("outputs", "tables",
       "unmatched_isco_occupations.csv")
)

message("Provisional occupational mapping completed.")
