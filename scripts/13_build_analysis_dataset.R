
# ============================================================
# PROJECT: THE EMPATHY PREMIUM IN INDIA
# SCRIPT: 13_build_analysis_dataset.R
#
# PURPOSE:
# Construct the consolidated worker-level PLFS dataset.
#
# DATA:
# PLFS 2021-22 and PLFS 2023-24
# O*NET Social Skills Index aggregated to ISCO minor groups
#
# SAMPLE:
# First-visit person records.
#
# KEY OUTPUTS:
# 1. plfs_analysis_all.csv
# 2. plfs_regular_salaried.csv
# 3. plfs_positive_wage_sample.csv
# 4. plfs_analysis_sample_audit.csv
# 5. plfs_annual_weight_summary.csv
#
# IMPORTANT:
# - SSI is an occupation-level measure.
# - Occupation 951 remains unmatched (SSI = NA).
# - Monthly earnings are nominal at this stage.
# - CPI deflation is a separate stage.
# - No causal interpretation is implied by this data build.
# ============================================================


# 0. PACKAGES -----------------------------------------------

packages <- c(
  "dplyr",
  "readr",
  "stringr",
  "here",
  "tibble"
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
library(stringr)
library(here)
library(tibble)


# 1. PROJECT PATHS ------------------------------------------

plfs_2022_zip <- here(
  "data", "raw", "plfs",
  "PLFS_Data_2021-22_CSV.zip"
)

plfs_2024_zip <- here(
  "data", "raw", "plfs",
  "CSV_data_PLFS_2023_2024.zip"
)

ssi_file <- here(
  "data", "processed",
  "onet_ssi_isco_minor.csv"
)

output_dir <- here("outputs", "tables")
processed_dir <- here("data", "processed")

required_files <- c(
  plfs_2022_zip,
  plfs_2024_zip,
  ssi_file
)

missing_files <- required_files[
  !file.exists(required_files)
]

if (length(missing_files) > 0) {
  stop(
    "Missing input files:\n",
    paste(missing_files, collapse = "\n")
  )
}


# 2. LOAD SSI DATA ------------------------------------------

cat("\n============================================\n")
cat("BUILDING PLFS ANALYSIS DATASET\n")
cat("============================================\n")

cat("\n[1/8] Loading SSI dataset...\n")

ssi <- read_csv(
  ssi_file,
  show_col_types = FALSE
) %>%
  mutate(
    isco_minor_code = str_trim(
      as.character(isco_minor_code)
    )
  )

if (anyDuplicated(ssi$isco_minor_code) > 0) {
  stop("Duplicate ISCO minor groups in SSI dataset.")
}

required_ssi <- c(
  "isco_minor_code",
  "n_onet_occupations",
  "SSI_raw_mean",
  "SSI_z_mean",
  "SSI_group_z"
)

if (!all(required_ssi %in% names(ssi))) {
  stop("SSI dataset does not have the expected columns.")
}

cat("SSI occupation groups:", nrow(ssi), "\n")


# 3. LOAD PLFS DATA -----------------------------------------

cat("\n[2/8] Loading PLFS person files...\n")

plfs21 <- read_csv(
  unz(
    plfs_2022_zip,
    "PLFS_Data_2021-22_CSV/perv1.csv"
  ),
  show_col_types = FALSE
)

plfs23 <- read_csv(
  unz(
    plfs_2024_zip,
    "CSV_data_PLFS_2023_2024/perv1.csv"
  ),
  show_col_types = FALSE
)

cat("PLFS 2021-22:", nrow(plfs21), "\n")
cat("PLFS 2023-24:", nrow(plfs23), "\n")


# 4. EXTRACT AND STANDARDISE VARIABLES ----------------------

cat("\n[3/8] Standardising PLFS variables...\n")

# Extract the same set of variables from both years.
# Casting categorical fields to character prevents
# bind_rows() errors caused by inconsistent data types.

prepare_plfs <- function(data, survey_year) {
  
  required_vars <- c(
    "qtr_perv1",
    "visit_perv1",
    "b1q1_perv1",
    "b1q3_perv1",
    "state_perv1",
    "distcode_perv1",
    "nss_region_perv1",
    "b1q5_perv1",
    "b1q6_perv1",
    "b1q11_perv1",
    "b1q13_perv1",
    "b1q14_perv1",
    "b1q15_perv1",
    "b4q1_perv1",
    "b4q5_perv1",
    "b4q6_perv1",
    "b4q8_perv1",
    "b6q5_perv1",
    "b6q6_perv1",
    "b6q7_perv1",
    "b6q9_perv1",
    "b6q10_perv1",
    "NSS_perv1",
    "NSC_perv1",
    "mult_perv1",
    "no_qtr_perv1"
  )
  
  missing_vars <- setdiff(
    required_vars,
    names(data)
  )
  
  if (length(missing_vars) > 0) {
    stop(
      "Required PLFS fields missing for ",
      survey_year,
      ": ",
      paste(missing_vars, collapse = ", ")
    )
  }
  
  data %>%
    transmute(
      
      year = survey_year,
      
      # Survey identifiers
      quarter = as.character(qtr_perv1),
      visit = as.character(visit_perv1),
      
      state = str_trim(
        as.character(state_perv1)
      ),
      
      district = str_trim(
        as.character(distcode_perv1)
      ),
      
      nss_region = str_trim(
        as.character(nss_region_perv1)
      ),
      
      sector = str_trim(
        as.character(b1q3_perv1)
      ),
      
      fsu = str_trim(
        as.character(b1q1_perv1)
      ),
      
      stratum = str_trim(
        as.character(b1q5_perv1)
      ),
      
      substratum = str_trim(
        as.character(b1q6_perv1)
      ),
      
      subsample = str_trim(
        as.character(b1q11_perv1)
      ),
      
      sample_subblock = str_trim(
        as.character(b1q13_perv1)
      ),
      
      second_stage_stratum = str_trim(
        as.character(b1q14_perv1)
      ),
      
      household_number = str_trim(
        as.character(b1q15_perv1)
      ),
      
      person_serial = str_trim(
        as.character(b4q1_perv1)
      ),
      
      # Individual characteristics
      sex_code = str_trim(
        as.character(b4q5_perv1)
      ),
      
      age = suppressWarnings(
        as.numeric(as.character(b4q6_perv1))
      ),
      
      education_code = str_trim(
        as.character(b4q8_perv1)
      ),
      
      # Current weekly status and occupation
      cws_code = str_trim(
        as.character(b6q5_perv1)
      ),
      
      industry_code = str_trim(
        as.character(b6q6_perv1)
      ),
      
      nco_code = str_trim(
        as.character(b6q7_perv1)
      ),
      
      # Earnings
      monthly_earnings = suppressWarnings(
        as.numeric(as.character(b6q9_perv1))
      ),
      
      self_employment_earnings = suppressWarnings(
        as.numeric(as.character(b6q10_perv1))
      ),
      
      # Multiplier components
      NSS = suppressWarnings(
        as.numeric(as.character(NSS_perv1))
      ),
      
      NSC = suppressWarnings(
        as.numeric(as.character(NSC_perv1))
      ),
      
      multiplier = suppressWarnings(
        as.numeric(as.character(mult_perv1))
      ),
      
      no_qtr = suppressWarnings(
        as.numeric(as.character(no_qtr_perv1))
      )
    )
}


plfs_2022 <- prepare_plfs(
  plfs21,
  2022
)

plfs_2024 <- prepare_plfs(
  plfs23,
  2024
)

# Combine only the selected, harmonised fields.

plfs_all <- bind_rows(
  plfs_2022,
  plfs_2024
)

rm(plfs21, plfs23, plfs_2022, plfs_2024)

cat("Combined PLFS observations:", nrow(plfs_all), "\n")


# 5. CONSTRUCT ANNUAL WEIGHTS -------------------------------

cat("\n[4/8] Constructing annual weights...\n")

plfs_all <- plfs_all %>%
  mutate(
    
    multiplier_base = case_when(
      
      !is.na(NSS) &
        !is.na(NSC) &
        NSS == NSC ~ multiplier / 100,
      
      !is.na(NSS) &
        !is.na(NSC) &
        NSS != NSC ~ multiplier / 200,
      
      TRUE ~ NA_real_
    ),
    
    annual_weight = case_when(
      
      !is.na(multiplier_base) &
        !is.na(no_qtr) &
        no_qtr > 0 ~ multiplier_base / no_qtr,
      
      TRUE ~ NA_real_
    )
  )

# Validate weights before proceeding.

weight_validation <- plfs_all %>%
  group_by(year) %>%
  summarise(
    
    observations = n(),
    
    missing_weights = sum(
      is.na(annual_weight)
    ),
    
    nonpositive_weights = sum(
      annual_weight <= 0,
      na.rm = TRUE
    ),
    
    minimum_weight = min(
      annual_weight,
      na.rm = TRUE
    ),
    
    median_weight = median(
      annual_weight,
      na.rm = TRUE
    ),
    
    mean_weight = mean(
      annual_weight,
      na.rm = TRUE
    ),
    
    maximum_weight = max(
      annual_weight,
      na.rm = TRUE
    ),
    
    estimated_population = sum(
      annual_weight,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  )

cat("\n--- ANNUAL WEIGHT VALIDATION ---\n")
print(weight_validation, width = Inf)

if (
  any(weight_validation$missing_weights > 0) ||
  any(weight_validation$nonpositive_weights > 0)
) {
  stop(
    "Weight validation failed. Inspect annual weight construction."
  )
}

write_csv(
  weight_validation,
  here(
    "outputs", "tables",
    "plfs_annual_weight_summary.csv"
  )
)


# 6. CREATE INDIVIDUAL ANALYSIS VARIABLES --------------------

cat("\n[5/8] Creating individual-level variables...\n")

plfs_all <- plfs_all %>%
  mutate(
    
    # Gender categories
    gender = case_when(
      sex_code == "1" ~ "Male",
      sex_code == "2" ~ "Female",
      sex_code == "3" ~ "Other",
      TRUE ~ NA_character_
    ),
    
    # Female dummy; other and missing categories
    # are intentionally not coded as male.
    female = case_when(
      sex_code == "1" ~ 0,
      sex_code == "2" ~ 1,
      TRUE ~ NA_real_
    ),
    
    # Age categories, retaining original age
    age_group = case_when(
      age >= 15 & age <= 24 ~ "15-24",
      age >= 25 & age <= 34 ~ "25-34",
      age >= 35 & age <= 44 ~ "35-44",
      age >= 45 & age <= 54 ~ "45-54",
      age >= 55 & age <= 64 ~ "55-64",
      age >= 65 ~ "65+",
      TRUE ~ NA_character_
    ),
    
    # Employment indicator
    regular_salaried = cws_code == "31",
    
    # Positive earnings indicator
    positive_monthly_earnings = (
      !is.na(monthly_earnings) &
        monthly_earnings > 0
    ),
    
    # Log monthly earnings is defined only for positive values.
    ln_monthly_earnings = if_else(
      positive_monthly_earnings,
      log(monthly_earnings),
      NA_real_
    ),
    
    # Sampling design identifiers
    # Stratification is represented by the geographic and
    # sampling strata fields; PSU is nested within stratum.
    sampling_stratum_id = paste(
      year,
      state,
      sector,
      stratum,
      substratum,
      sep = "_"
    ),
    
    psu_id = paste(
      sampling_stratum_id,
      quarter,
      fsu,
      sep = "_"
    ),
    
    household_id = paste(
      psu_id,
      sample_subblock,
      second_stage_stratum,
      household_number,
      sep = "_"
    ),
    
    person_id = paste(
      household_id,
      person_serial,
      sep = "_"
    )
  )


# 7. MERGE OCCUPATIONAL SSI ---------------------------------

cat("\n[6/8] Merging occupational social-skill scores...\n")

plfs_all <- plfs_all %>%
  left_join(
    ssi %>%
      select(
        isco_minor_code,
        n_onet_occupations,
        SSI_raw_mean,
        SSI_z_mean,
        SSI_group_z
      ),
    by = c("nco_code" = "isco_minor_code"),
    relationship = "many-to-one"
  ) %>%
  mutate(
    has_ssi = !is.na(SSI_raw_mean)
  )

# Confirm no worker rows were duplicated by the merge.

cat(
  "Rows before SSI merge:",
  nrow(plfs_all),
  "\n"
)

cat(
  "Rows after SSI merge:",
  nrow(plfs_all),
  "\n"
)

if (anyDuplicated(plfs_all$person_id) > 0) {
  warning(
    "Repeated person identifiers detected. ",
    "Review the survey identifiers before panel or pooled analysis."
  )
}


# 8. CREATE ANALYSIS SUBSAMPLES ------------------------------

cat("\n[7/8] Creating analysis samples...\n")

# Full first-visit person-level dataset.

plfs_analysis_all <- plfs_all


# Regular salaried workers, retaining zero and missing wages
# for transparent sample accounting.

plfs_regular_salaried <- plfs_all %>%
  filter(
    regular_salaried %in% TRUE
  )


# Positive wage sample restricted to male and female
# observations for the gender wage-gap analysis.
# SSI-missing records are retained at this stage and are
# removed only in models that require SSI.

plfs_positive_wage_sample <- plfs_regular_salaried %>%
  filter(
    sex_code %in% c("1", "2"),
    positive_monthly_earnings,
    !is.na(annual_weight)
  )


# Complete SSI-ready wage sample.

plfs_ssi_wage_sample <- plfs_positive_wage_sample %>%
  filter(
    !is.na(SSI_raw_mean)
  )


# 9. SAMPLE AUDIT -------------------------------------------

cat("\n[8/8] Creating sample and coverage audits...\n")

sample_audit <- bind_rows(
  
  plfs_analysis_all %>%
    group_by(year) %>%
    summarise(
      sample = "All first-visit persons",
      observations = n(),
      observations_with_ssi = sum(has_ssi),
      observations_without_ssi = sum(!has_ssi),
      .groups = "drop"
    ),
  
  plfs_regular_salaried %>%
    group_by(year) %>%
    summarise(
      sample = "Regular salaried",
      observations = n(),
      observations_with_ssi = sum(has_ssi),
      observations_without_ssi = sum(!has_ssi),
      .groups = "drop"
    ),
  
  plfs_positive_wage_sample %>%
    group_by(year) %>%
    summarise(
      sample = "Positive-wage male/female",
      observations = n(),
      observations_with_ssi = sum(has_ssi),
      observations_without_ssi = sum(!has_ssi),
      .groups = "drop"
    ),
  
  plfs_ssi_wage_sample %>%
    group_by(year) %>%
    summarise(
      sample = "Positive-wage male/female with SSI",
      observations = n(),
      observations_with_ssi = sum(has_ssi),
      observations_without_ssi = sum(!has_ssi),
      .groups = "drop"
    )
) %>%
  arrange(year, sample)

cat("\n--- ANALYSIS SAMPLE AUDIT ---\n")
print(sample_audit, n = Inf)

write_csv(
  sample_audit,
  here(
    "outputs", "tables",
    "plfs_analysis_sample_audit.csv"
  )
)


# Gender wage summary using annual weights.
# These are descriptive statistics, not survey-design
# standard-error estimates.

weighted_wage_summary <- plfs_positive_wage_sample %>%
  filter(
    !is.na(gender),
    !is.na(annual_weight),
    annual_weight > 0
  ) %>%
  group_by(year, gender) %>%
  summarise(
    observations = n(),
    
    weighted_mean_monthly_wage = weighted.mean(
      monthly_earnings,
      annual_weight
    ),
    
    weighted_median_monthly_wage = {
      ord <- order(monthly_earnings)
      x <- monthly_earnings[ord]
      w <- annual_weight[ord]
      x[which(cumsum(w) >= sum(w) / 2)[1]]
    },
    
    weighted_population = sum(annual_weight),
    
    .groups = "drop"
  )

cat("\n--- WEIGHTED NOMINAL WAGE SUMMARY ---\n")
print(weighted_wage_summary, n = Inf)

write_csv(
  weighted_wage_summary,
  here(
    "outputs", "tables",
    "plfs_weighted_nominal_wages.csv"
  )
)


# 10. SAVE ANALYTICAL DATASETS ------------------------------

cat("\nSaving analytical datasets...\n")

write_csv(
  plfs_analysis_all,
  here(
    "data", "processed",
    "plfs_analysis_all.csv"
  )
)

write_csv(
  plfs_regular_salaried,
  here(
    "data", "processed",
    "plfs_regular_salaried.csv"
  )
)

write_csv(
  plfs_positive_wage_sample,
  here(
    "data", "processed",
    "plfs_positive_wage_sample.csv"
  )
)

write_csv(
  plfs_ssi_wage_sample,
  here(
    "data", "processed",
    "plfs_ssi_wage_sample.csv"
  )
)


# Save compact R objects for subsequent scripts.

saveRDS(
  plfs_analysis_all,
  here(
    "data", "processed",
    "plfs_analysis_all.rds"
  )
)

saveRDS(
  plfs_positive_wage_sample,
  here(
    "data", "processed",
    "plfs_positive_wage_sample.rds"
  )
)


# 11. FINAL CHECKS ------------------------------------------

cat("\n============================================\n")
cat("ANALYSIS DATASET BUILD COMPLETED\n")
cat("============================================\n")

cat(
  "\nAll first-visit person observations:",
  nrow(plfs_analysis_all),
  "\n"
)

cat(
  "Regular salaried observations:",
  nrow(plfs_regular_salaried),
  "\n"
)

cat(
  "Positive-wage male/female observations:",
  nrow(plfs_positive_wage_sample),
  "\n"
)

cat(
  "Positive-wage observations with SSI:",
  nrow(plfs_ssi_wage_sample),
  "\n"
)

cat(
  "\nSSI-missing regular salaried records:",
  sum(is.na(plfs_regular_salaried$SSI_raw_mean)),
  "\n"
)

cat(
  "\nSaved files in data/processed:\n",
  "plfs_analysis_all.csv\n",
  "plfs_regular_salaried.csv\n",
  "plfs_positive_wage_sample.csv\n",
  "plfs_ssi_wage_sample.csv\n",
  "\n"
)

cat(
  "\nNext stage: CPI deflation and AI exposure integration.\n"
)
