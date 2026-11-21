
# ============================================================
# PROJECT: THE EMPATHY PREMIUM IN INDIA
# SCRIPT: 16_build_final_model_dataset.R
#
# PURPOSE:
# Construct the final worker-level econometric dataset.
#
# INPUTS:
# 1. CPI-adjusted PLFS data
# 2. O*NET SSI aggregated to ISCO-08 minor groups
# 3. ILO GenAI exposure aggregated to ISCO-08 minor groups
#
# MAIN RESEARCH VARIABLES:
# Female
# Social skill intensity
# GenAI exposure
# Female x SSI x GenAI exposure
#
# SAMPLE:
# Regular salaried workers with positive monthly earnings.
#
# ============================================================


# 0. PACKAGES -----------------------------------------------

packages <- c(
  "dplyr",
  "readr",
  "here",
  "tibble",
  "stringr"
)

missing_packages <- packages[
  !vapply(packages, requireNamespace,
          logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "Install missing packages: ",
    paste(missing_packages, collapse = ", ")
  )
}

library(dplyr)
library(readr)
library(here)
library(tibble)
library(stringr)


# 1. INPUT FILES --------------------------------------------

plfs_file <- here(
  "data", "processed",
  "plfs_analysis_cpi.csv"
)

ai_file <- here(
  "data", "processed",
  "ilo_genai_isco3.csv"
)

if (!file.exists(plfs_file) || !file.exists(ai_file)) {
  stop("Required input dataset not found.")
}


# 2. LOAD DATA ----------------------------------------------

cat("\n============================================\n")
cat("FINAL ECONOMETRIC DATASET CONSTRUCTION\n")
cat("============================================\n")

plfs <- read_csv(
  plfs_file,
  show_col_types = FALSE
)

ai <- read_csv(
  ai_file,
  show_col_types = FALSE
)

cat("PLFS observations:", nrow(plfs), "\n")
cat("AI occupation groups:", nrow(ai), "\n")


# 3. VALIDATE INPUTS ----------------------------------------

required_plfs <- c(
  "year", "nco_code", "gender", "sex_code",
  "female", "age", "education_code",
  "cws_code", "industry_code", "state",
  "sector", "quarter", "monthly_earnings",
  "real_monthly_earnings",
  "ln_real_monthly_earnings",
  "annual_weight", "SSI_raw_mean",
  "SSI_group_z", "sampling_stratum_id",
  "psu_id"
)

required_ai <- c(
  "isco_minor_code",
  "AI_exposure_mean",
  "AI_exposure_sd_between_occupations",
  "n_isco4_occupations"
)

if (!all(required_plfs %in% names(plfs))) {
  stop(
    "Missing PLFS fields: ",
    paste(setdiff(required_plfs, names(plfs)), collapse = ", ")
  )
}

if (!all(required_ai %in% names(ai))) {
  stop(
    "Missing AI fields: ",
    paste(setdiff(required_ai, names(ai)), collapse = ", ")
  )
}

if (anyDuplicated(ai$isco_minor_code) > 0) {
  stop("Duplicate AI occupation groups detected.")
}


# 4. STANDARDISE AI EXPOSURE -------------------------------

cat("\n[1/6] Standardising AI exposure...\n")

ai_clean <- ai %>%
  mutate(
    isco_minor_code = str_trim(
      as.character(isco_minor_code)
    ),
    
    AI_exposure_z = as.numeric(
      scale(AI_exposure_mean)
    )
  ) %>%
  select(
    isco_minor_code,
    AI_exposure_mean,
    AI_exposure_z,
    AI_exposure_sd_between_occupations,
    n_isco4_occupations
  )

if (
  any(ai_clean$AI_exposure_mean < 0 |
      ai_clean$AI_exposure_mean > 1)
) {
  stop("AI scores must lie between zero and one.")
}


# 5. MERGE AI INTO PLFS -------------------------------------

cat("\n[2/6] Merging occupational AI exposure...\n")

n_before <- nrow(plfs)

plfs_final <- plfs %>%
  mutate(
    nco_code = str_trim(as.character(nco_code))
  ) %>%
  left_join(
    ai_clean,
    by = c("nco_code" = "isco_minor_code"),
    relationship = "many-to-one"
  )

if (nrow(plfs_final) != n_before) {
  stop("PLFS observations changed after AI merge.")
}


# 6. CREATE ECONOMETRIC VARIABLES ---------------------------

cat("\n[3/6] Constructing regression variables...\n")

plfs_final <- plfs_final %>%
  mutate(
    female = case_when(
      sex_code == "2" ~ 1,
      sex_code == "1" ~ 0,
      TRUE ~ NA_real_
    ),
    
    age_squared = age^2,
    
    SSI = SSI_group_z,
    
    AI = AI_exposure_z,
    
    ln_real_wage = if_else(
      real_monthly_earnings > 0,
      log(real_monthly_earnings),
      NA_real_
    ),
    
    # Two-way interactions
    female_SSI = female * SSI,
    
    female_AI = female * AI,
    
    SSI_AI = SSI * AI,
    
    # Main research interaction
    female_SSI_AI = female * SSI * AI,
    
    # Categorical controls
    state_factor = factor(state),
    
    education_factor = factor(education_code),
    
    industry_factor = factor(industry_code),
    
    sector_factor = factor(sector),
    
    year_factor = factor(year),
    
    occupation_factor = factor(nco_code)
  )


# 7. DEFINE THE MAIN REGRESSION SAMPLE ----------------------

cat("\n[4/6] Preparing regression sample...\n")

model_sample <- plfs_final %>%
  filter(
    cws_code == "31",
    sex_code %in% c("1", "2"),
    !is.na(ln_real_wage),
    !is.na(female),
    !is.na(SSI),
    !is.na(AI),
    !is.na(age),
    !is.na(education_code),
    !is.na(industry_code),
    !is.na(state),
    !is.na(annual_weight),
    annual_weight > 0
  ) %>%
  mutate(
    age_squared = age^2
  )

# This is the complete-case sample for the initially
# specified controls. Survey design still requires validation.


# 8. COVERAGE AUDIT -----------------------------------------

cat("\n[5/6] Preparing coverage audit...\n")

coverage_audit <- plfs_final %>%
  filter(
    cws_code == "31",
    sex_code %in% c("1", "2"),
    real_monthly_earnings > 0
  ) %>%
  group_by(year) %>%
  summarise(
    total_workers = n(),
    
    AI_available = sum(!is.na(AI)),
    
    SSI_available = sum(!is.na(SSI)),
    
    both_available = sum(
      !is.na(AI) & !is.na(SSI)
    ),
    
    missing_both_or_either = sum(
      is.na(AI) | is.na(SSI)
    ),
    
    final_model_sample = sum(
      !is.na(AI) &
        !is.na(SSI) &
        !is.na(age) &
        !is.na(education_code) &
        !is.na(industry_code) &
        !is.na(state) &
        !is.na(annual_weight)
    ),
    
    joint_coverage_percentage = round(
      100 * both_available / total_workers,
      2
    ),
    
    .groups = "drop"
  )

print(coverage_audit, width = Inf)

write_csv(
  coverage_audit,
  here(
    "outputs", "tables",
    "final_model_coverage_audit.csv"
  )
)


# 9. SURVEY DESIGN AUDIT ------------------------------------

cat("\n[6/6] Auditing survey identifiers...\n")

design_audit <- model_sample %>%
  distinct(
    year,
    sampling_stratum_id,
    psu_id
  ) %>%
  group_by(
    year,
    sampling_stratum_id
  ) %>%
  summarise(
    number_of_psus = n_distinct(psu_id),
    .groups = "drop"
  ) %>%
  group_by(year) %>%
  summarise(
    number_of_strata = n(),
    total_psus = sum(number_of_psus),
    single_psu_strata = sum(number_of_psus == 1),
    maximum_psus_per_stratum = max(number_of_psus),
    .groups = "drop"
  )

cat("\n--- SURVEY DESIGN AUDIT ---\n")
print(design_audit, width = Inf)

write_csv(
  design_audit,
  here(
    "outputs", "tables",
    "final_model_survey_design_audit.csv"
  )
)


# 10. EXPORT FINAL DATASETS -------------------------------

cat("\nSaving final model datasets...\n")

write_csv(
  plfs_final,
  here(
    "data", "processed",
    "plfs_final_ai_ssi_cpi.csv"
  )
)

write_csv(
  model_sample,
  here(
    "data", "processed",
    "plfs_final_model_sample.csv"
  )
)

saveRDS(
  model_sample,
  here(
    "data", "processed",
    "plfs_final_model_sample.rds"
  )
)


# 11. FINAL RESULTS -----------------------------------------

cat("\n============================================\n")
cat("FINAL MODEL DATASET CREATED\n")
cat("============================================\n")

cat(
  "\nFull PLFS observations:",
  nrow(plfs_final),
  "\n"
)

cat(
  "Final regression observations:",
  nrow(model_sample),
  "\n"
)

cat(
  "Unique occupations in model sample:",
  n_distinct(model_sample$nco_code),
  "\n"
)

cat(
  "SSI coverage:",
  round(mean(!is.na(plfs_final$SSI)) * 100, 2),
  "%\n"
)

cat(
  "AI coverage:",
  round(mean(!is.na(plfs_final$AI)) * 100, 2),
  "%\n"
)

cat(
  "\nMain output:\n",
  "data/processed/plfs_final_model_sample.csv\n"
)

cat(
  "\nNext: validate survey design and estimate models.\n"
)
