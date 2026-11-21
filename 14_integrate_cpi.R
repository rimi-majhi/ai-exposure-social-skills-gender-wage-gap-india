
# ============================================================
# PROJECT: THE EMPATHY PREMIUM IN INDIA
# SCRIPT: 14_integrate_cpi.R
#
# PURPOSE:
# Deflate PLFS nominal monthly earnings using CPI.
#
# CPI BASE:
# 2012 = 100
#
# CPI REFERENCE PERIOD:
# July 2023 - June 2024
#
# CPI SECTORS:
# Rural and Urban, All India
#
# PLFS SURVEY PERIODS:
# 2021-22: July 2021 - June 2022
# 2023-24: July 2023 - June 2024
#
# OUTPUT:
# Monthly earnings in 2023-24 reference-period rupees.
#
# LIMITATION:
# CPI data are All India only, not state-specific.
# ============================================================


# 0. PACKAGES -----------------------------------------------

packages <- c(
  "dplyr",
  "readr",
  "readxl",
  "stringr",
  "tidyr",
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
library(readxl)
library(stringr)
library(tidyr)
library(here)
library(tibble)


# 1. FILE PATHS ---------------------------------------------

plfs_file <- here(
  "data", "processed",
  "plfs_analysis_all.csv"
)

cpi_file <- here(
  "data", "raw", "cpi",
  "cpi_2287.xlsx"
)

output_dir <- here("outputs", "tables")
processed_dir <- here("data", "processed")

if (!file.exists(plfs_file)) {
  stop("PLFS analysis dataset not found.")
}

if (!file.exists(cpi_file)) {
  stop("CPI workbook not found.")
}


# 2. LOAD DATA ----------------------------------------------

cat("\n============================================\n")
cat("CPI INTEGRATION AND REAL WAGE CONSTRUCTION\n")
cat("============================================\n")

cat("\n[1/8] Loading PLFS and CPI data...\n")

plfs <- read_csv(
  plfs_file,
  show_col_types = FALSE
)

cpi_raw <- read_excel(
  cpi_file,
  sheet = "Sheet1"
)

cat("PLFS records:", nrow(plfs), "\n")
cat("CPI records:", nrow(cpi_raw), "\n")


# 3. CLEAN GENERAL CPI --------------------------------------

cat("\n[2/8] Preparing CPI series...\n")

cpi_general <- cpi_raw %>%
  filter(
    baseyear == "2012",
    state == "All India",
    group == "General",
    sector %in% c("Rural", "Urban")
  ) %>%
  transmute(
    cpi_sector = as.character(sector),
    
    calendar_year = as.integer(year),
    
    month_code = as.integer(month_code),
    
    month_date = as.Date(
      sprintf(
        "%04d-%02d-01",
        as.integer(year),
        as.integer(month_code)
      )
    ),
    
    cpi_index = parse_number(as.character(index))
  ) %>%
  filter(
    !is.na(cpi_index),
    !is.na(month_date)
  ) %>%
  arrange(cpi_sector, month_date)

# Check monthly uniqueness.

cpi_duplicates <- cpi_general %>%
  count(cpi_sector, month_date) %>%
  filter(n > 1)

if (nrow(cpi_duplicates) > 0) {
  stop("Duplicate monthly CPI observations detected.")
}

# Check CPI period coverage.

cpi_coverage <- cpi_general %>%
  group_by(cpi_sector) %>%
  summarise(
    months_available = n_distinct(month_date),
    first_month = min(month_date),
    last_month = max(month_date),
    missing_indices = sum(is.na(cpi_index)),
    .groups = "drop"
  )

cat("\n--- CPI COVERAGE ---\n")
print(cpi_coverage, width = Inf)

if (any(cpi_coverage$months_available < 48)) {
  warning("Fewer than 48 monthly observations for a CPI sector.")
}


# 4. MAP PLFS QUARTERS TO CALENDAR MONTHS --------------------

cat("\n[3/8] Building PLFS quarter calendar...\n")

# PLFS year 2022 = July 2021 to June 2022.
# PLFS year 2024 = July 2023 to June 2024.

quarter_calendar <- expand_grid(
  survey_year = c(2022L, 2024L),
  quarter = paste0("Q", 1:4),
  month_in_quarter = 1:3
) %>%
  mutate(
    
    calendar_year = case_when(
      quarter %in% c("Q1", "Q2") ~ survey_year - 1L,
      quarter %in% c("Q3", "Q4") ~ survey_year,
      TRUE ~ NA_integer_
    ),
    
    calendar_month = case_when(
      quarter == "Q1" ~ 6L + month_in_quarter,
      quarter == "Q2" ~ 9L + month_in_quarter,
      quarter == "Q3" ~ month_in_quarter,
      quarter == "Q4" ~ 3L + month_in_quarter
    ),
    
    month_date = as.Date(
      sprintf(
        "%04d-%02d-01",
        calendar_year,
        calendar_month
      )
    )
  )

cat("\n--- QUARTER CALENDAR ---\n")
print(
  quarter_calendar %>%
    select(
      survey_year,
      quarter,
      calendar_year,
      calendar_month
    ),
  n = Inf
)


# 5. COMPUTE QUARTERLY CPI AVERAGES --------------------------

cat("\n[4/8] Calculating quarterly average CPI...\n")

quarterly_cpi <- quarter_calendar %>%
  inner_join(
    cpi_general,
    by = "month_date"
  ) %>%
  group_by(
    survey_year,
    quarter,
    cpi_sector
  ) %>%
  summarise(
    quarterly_cpi = mean(
      cpi_index,
      na.rm = TRUE
    ),
    
    months_available = n_distinct(month_date),
    
    .groups = "drop"
  )

cat("\n--- QUARTERLY CPI TABLE ---\n")

print(
  quarterly_cpi %>%
    arrange(survey_year, cpi_sector, quarter),
  n = Inf
)

# Each quarter must contain all three months.

if (any(quarterly_cpi$months_available != 3)) {
  stop(
    "A PLFS quarter does not have all three CPI months."
  )
}

write_csv(
  quarterly_cpi,
  here(
    "outputs", "tables",
    "plfs_quarterly_cpi.csv"
  )
)


# 6. CONSTRUCT REFERENCE-PERIOD CPI -------------------------

cat("\n[5/8] Calculating reference-period CPI...\n")

# Reference period: July 2023 - June 2024.
# This is the 2023-24 PLFS survey year.

reference_cpi <- cpi_general %>%
  filter(
    month_date >= as.Date("2023-07-01"),
    month_date <= as.Date("2024-06-01")
  ) %>%
  group_by(cpi_sector) %>%
  summarise(
    reference_cpi = mean(
      cpi_index,
      na.rm = TRUE
    ),
    
    reference_months = n_distinct(month_date),
    
    .groups = "drop"
  ) %>%
  mutate(
    reference_period = "2023-24",
    reference_base_year = "2012"
  )

cat("\n--- REFERENCE CPI ---\n")
print(reference_cpi, width = Inf)

if (any(reference_cpi$reference_months != 12)) {
  stop("Reference CPI must contain 12 months per sector.")
}

write_csv(
  reference_cpi,
  here(
    "outputs", "tables",
    "cpi_reference_period_2023_24.csv"
  )
)


# 7. CALCULATE CPI DEFLATORS -------------------------------

cat("\n[6/8] Constructing CPI deflators...\n")

cpi_deflators <- quarterly_cpi %>%
  left_join(
    reference_cpi,
    by = "cpi_sector"
  ) %>%
  mutate(
    cpi_deflator = reference_cpi / quarterly_cpi
  )

if (
  any(is.na(cpi_deflators$cpi_deflator)) ||
  any(cpi_deflators$cpi_deflator <= 0)
) {
  stop("Invalid CPI deflator detected.")
}

write_csv(
  cpi_deflators,
  here(
    "outputs", "tables",
    "plfs_cpi_deflators.csv"
  )
)


# 8. MERGE CPI INTO PLFS ------------------------------------

cat("\n[7/8] Merging CPI into PLFS records...\n")

# PLFS sector codes:
# 1 = Rural
# 2 = Urban

plfs_cpi <- plfs %>%
  mutate(
    
    year = as.integer(year),
    
    quarter = str_to_upper(
      str_trim(as.character(quarter))
    ),
    
    sector = str_trim(as.character(sector)),
    
    cpi_sector = case_when(
      sector == "1" ~ "Rural",
      sector == "2" ~ "Urban",
      TRUE ~ NA_character_
    )
  ) %>%
  left_join(
    cpi_deflators %>%
      select(
        survey_year,
        quarter,
        cpi_sector,
        quarterly_cpi,
        reference_cpi,
        cpi_deflator,
        reference_period
      ),
    by = c(
      "year" = "survey_year",
      "quarter",
      "cpi_sector"
    ),
    relationship = "many-to-one"
  ) %>%
  mutate(
    
    real_monthly_earnings = if_else(
      !is.na(monthly_earnings) &
        !is.na(cpi_deflator),
      monthly_earnings * cpi_deflator,
      NA_real_
    ),
    
    ln_real_monthly_earnings = if_else(
      !is.na(real_monthly_earnings) &
        real_monthly_earnings > 0,
      log(real_monthly_earnings),
      NA_real_
    )
  )

# Ensure no observations were duplicated.

if (nrow(plfs_cpi) != nrow(plfs)) {
  stop("PLFS rows changed during CPI merge.")
}

# CPI coverage.

cpi_merge_audit <- plfs_cpi %>%
  group_by(year, cpi_sector) %>%
  summarise(
    observations = n(),
    observations_with_cpi = sum(!is.na(cpi_deflator)),
    observations_without_cpi = sum(is.na(cpi_deflator)),
    coverage_percentage = round(
      100 * mean(!is.na(cpi_deflator)),
      2
    ),
    .groups = "drop"
  )

cat("\n--- CPI MERGE AUDIT ---\n")
print(cpi_merge_audit, n = Inf)

write_csv(
  cpi_merge_audit,
  here(
    "outputs", "tables",
    "plfs_cpi_merge_audit.csv"
  )
)

if (any(cpi_merge_audit$observations_without_cpi > 0)) {
  stop("Some PLFS observations did not receive a CPI deflator.")
} else {
  cat("\nAll PLFS observations successfully matched to CPI.\n")
}


# 9. BUILD REAL-WAGE ANALYSIS SAMPLES -----------------------

cat("\n[8/8] Creating real-wage samples...\n")

# All individuals.

plfs_real_all <- plfs_cpi


# Regular salaried workers.

plfs_real_salaried <- plfs_cpi %>%
  filter(
    cws_code == "31"
  )


# Positive wage male/female sample.

plfs_real_wage_sample <- plfs_real_salaried %>%
  filter(
    sex_code %in% c("1", "2"),
    !is.na(real_monthly_earnings),
    real_monthly_earnings > 0,
    !is.na(annual_weight),
    annual_weight > 0
  )


# Positive wage sample with SSI.

plfs_real_ssi_sample <- plfs_real_wage_sample %>%
  filter(
    !is.na(SSI_raw_mean)
  )


# 10. REAL-WAGE DESCRIPTIVE STATISTICS -----------------------

real_wage_summary <- plfs_real_wage_sample %>%
  mutate(
    gender = case_when(
      sex_code == "1" ~ "Male",
      sex_code == "2" ~ "Female",
      TRUE ~ NA_character_
    )
  ) %>%
  group_by(year, gender) %>%
  summarise(
    observations = n(),
    
    weighted_mean_real_wage = weighted.mean(
      real_monthly_earnings,
      annual_weight
    ),
    
    weighted_population = sum(annual_weight),
    
    .groups = "drop"
  )

cat("\n--- WEIGHTED REAL WAGE SUMMARY ---\n")
print(real_wage_summary, n = Inf, width = Inf)

write_csv(
  real_wage_summary,
  here(
    "outputs", "tables",
    "plfs_weighted_real_wage_summary.csv"
  )
)


# 11. SAMPLE AUDIT ------------------------------------------

real_sample_audit <- bind_rows(
  
  plfs_real_all %>%
    group_by(year) %>%
    summarise(
      sample = "All persons",
      observations = n(),
      with_cpi = sum(!is.na(cpi_deflator)),
      with_ssi = sum(!is.na(SSI_raw_mean)),
      .groups = "drop"
    ),
  
  plfs_real_salaried %>%
    group_by(year) %>%
    summarise(
      sample = "Regular salaried",
      observations = n(),
      with_cpi = sum(!is.na(cpi_deflator)),
      with_ssi = sum(!is.na(SSI_raw_mean)),
      .groups = "drop"
    ),
  
  plfs_real_wage_sample %>%
    group_by(year) %>%
    summarise(
      sample = "Positive-wage male/female",
      observations = n(),
      with_cpi = sum(!is.na(cpi_deflator)),
      with_ssi = sum(!is.na(SSI_raw_mean)),
      .groups = "drop"
    ),
  
  plfs_real_ssi_sample %>%
    group_by(year) %>%
    summarise(
      sample = "Positive-wage male/female with SSI",
      observations = n(),
      with_cpi = sum(!is.na(cpi_deflator)),
      with_ssi = sum(!is.na(SSI_raw_mean)),
      .groups = "drop"
    )
) %>%
  arrange(year, sample)

cat("\n--- REAL-WAGE SAMPLE AUDIT ---\n")
print(real_sample_audit, n = Inf)

write_csv(
  real_sample_audit,
  here(
    "outputs", "tables",
    "plfs_real_wage_sample_audit.csv"
  )
)


# 12. SAVE DATASETS -----------------------------------------

write_csv(
  plfs_real_all,
  here(
    "data", "processed",
    "plfs_analysis_cpi.csv"
  )
)

write_csv(
  plfs_real_salaried,
  here(
    "data", "processed",
    "plfs_regular_salaried_cpi.csv"
  )
)

write_csv(
  plfs_real_wage_sample,
  here(
    "data", "processed",
    "plfs_positive_wage_cpi.csv"
  )
)

write_csv(
  plfs_real_ssi_sample,
  here(
    "data", "processed",
    "plfs_ssi_wage_cpi.csv"
  )
)

saveRDS(
  plfs_real_all,
  here(
    "data", "processed",
    "plfs_analysis_cpi.rds"
  )
)

saveRDS(
  plfs_real_ssi_sample,
  here(
    "data", "processed",
    "plfs_ssi_wage_cpi.rds"
  )
)


# 13. FINAL STATUS ------------------------------------------

cat("\n============================================\n")
cat("CPI INTEGRATION COMPLETED\n")
cat("============================================\n")

cat(
  "\nPLFS observations:",
  nrow(plfs_real_all),
  "\n"
)

cat(
  "Regular salaried observations:",
  nrow(plfs_real_salaried),
  "\n"
)

cat(
  "Positive-wage observations:",
  nrow(plfs_real_wage_sample),
  "\n"
)

cat(
  "Positive-wage observations with SSI:",
  nrow(plfs_real_ssi_sample),
  "\n"
)

cat(
  "\nReference price period: 2023-24\n"
)

cat(
  "CPI base year: 2012\n"
)

cat(
  "\nMain output:\n",
  "data/processed/plfs_analysis_cpi.csv\n"
)

cat(
  "\nNext stage: occupational AI exposure integration.\n"
)
