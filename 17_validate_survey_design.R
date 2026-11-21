
# ============================================================
# SCRIPT 17: VALIDATE PLFS SURVEY DESIGN
# ============================================================

packages <- c("dplyr", "readr", "survey", "here", "tibble")

missing_packages <- packages[
  !vapply(packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop("Install packages: ", paste(missing_packages, collapse = ", "))
}

library(dplyr)
library(readr)
library(survey)
library(here)
library(tibble)

# 1. LOAD FULL ANALYSIS DATA

input_file <- here(
  "data", "processed",
  "plfs_final_ai_ssi_cpi.csv"
)

if (!file.exists(input_file)) {
  stop("Full AI-SSI-CPI dataset not found.")
}

plfs <- read_csv(input_file, show_col_types = FALSE)

# 2. CHECK DESIGN VARIABLES

required <- c(
  "year", "state", "nss_region", "sector",
  "stratum", "substratum", "quarter", "fsu",
  "annual_weight"
)

if (!all(required %in% names(plfs))) {
  stop(
    "Missing survey fields: ",
    paste(setdiff(required, names(plfs)), collapse = ", ")
  )
}

# 3. REBUILD STRATUM AND PSU IDENTIFIERS

# Rural: retain substratum.
# Urban: no sub-stratification; use a common marker.

plfs <- plfs %>%
  mutate(
    design_substratum = case_when(
      sector == "1" ~ substratum,
      sector == "2" ~ "0",
      TRUE ~ NA_character_
    ),
    
    sampling_stratum_id = paste(
      year,
      state,
      nss_region,
      sector,
      stratum,
      design_substratum,
      sep = "_"
    ),
    
    # Quarter is included to distinguish annual FSU
    # sample occasions.
    psu_id = paste(
      sampling_stratum_id,
      quarter,
      fsu,
      sep = "_"
    )
  )

if (
  anyNA(plfs$sampling_stratum_id) ||
  anyNA(plfs$psu_id) ||
  anyNA(plfs$annual_weight) ||
  any(plfs$annual_weight <= 0)
) {
  stop("Invalid survey design identifiers or weights.")
}

# 4. STRATUM AND PSU AUDIT

psu_counts <- plfs %>%
  distinct(year, sampling_stratum_id, psu_id) %>%
  group_by(year, sampling_stratum_id) %>%
  summarise(
    psus = n_distinct(psu_id),
    .groups = "drop"
  )

design_audit <- psu_counts %>%
  group_by(year) %>%
  summarise(
    number_of_strata = n(),
    total_psus = sum(psus),
    single_psu_strata = sum(psus == 1),
    minimum_psus_per_stratum = min(psus),
    median_psus_per_stratum = median(psus),
    maximum_psus_per_stratum = max(psus),
    .groups = "drop"
  )

cat("\n--- CORRECTED SURVEY DESIGN AUDIT ---\n")
print(design_audit, width = Inf)

write_csv(
  design_audit,
  here(
    "outputs", "tables",
    "corrected_survey_design_audit.csv"
  )
)

# 5. CREATE FULL SURVEY DESIGN

# First-stage units are FSUs.
# No FPC is supplied: the design is treated as
# with-replacement for variance estimation.

design_full <- survey::svydesign(
  ids = ~psu_id,
  strata = ~sampling_stratum_id,
  weights = ~annual_weight,
  data = plfs,
  nest = TRUE
)

cat("\n--- FULL DESIGN ---\n")
print(design_full)

cat(
  "\nDesign degrees of freedom:",
  survey::degf(design_full),
  "\n"
)

# 6. DEFINE THE ECONOMETRIC DOMAIN

# IMPORTANT: create the design on the full PLFS dataset
# and then subset it. Do not construct the survey design
# directly from the salaried-worker sample.

model_domain <- with(
  plfs,
  cws_code == "31" &
    sex_code %in% c("1", "2") &
    !is.na(ln_real_wage) &
    !is.na(SSI_group_z) &
    !is.na(AI_exposure_mean) &
    !is.na(age) &
    !is.na(education_code) &
    !is.na(industry_code) &
    !is.na(state) &
    !is.na(annual_weight)
)

design_model <- subset(
  design_full,
  model_domain
)

cat("\n--- ECONOMETRIC DOMAIN SAMPLE ---\n")

cat(
  "Observations:",
  nrow(design_model$variables),
  "\n"
)

cat(
  "Survey design degrees of freedom:",
  survey::degf(design_model),
  "\n"
)

# 7. SAVE VALIDATED DESIGN OBJECTS

saveRDS(
  design_full,
  here(
    "data", "processed",
    "plfs_survey_design_full.rds"
  )
)

saveRDS(
  design_model,
  here(
    "data", "processed",
    "plfs_survey_design_model.rds"
  )
)

# Export corrected model dataset.

write_csv(
  plfs,
  here(
    "data", "processed",
    "plfs_final_ai_ssi_cpi_survey_ids.csv"
  )
)

cat("\n============================================\n")
cat("SURVEY DESIGN VALIDATION COMPLETED\n")
cat("============================================\n")
