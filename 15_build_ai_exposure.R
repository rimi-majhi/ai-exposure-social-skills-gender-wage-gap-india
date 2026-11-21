
# ============================================================
# PROJECT: THE EMPATHY PREMIUM IN INDIA
# SCRIPT: 15_build_ai_exposure.R
#
# PURPOSE:
# Construct ISCO-08 three-digit occupational GenAI exposure
# measures from the ILO Working Paper 140 dataset.
#
# SOURCE:
# Gmyrek et al. (2025), ILO Working Paper 140.
#
# INPUT:
# ILO_2025_GenAI_ISCO08.xlsx
#
# MAPPING:
# ISCO-08 four-digit occupation -> three-digit minor group.
#
# MAIN MEASURE:
# Equal-weighted mean of mean_score_2025 across
# four-digit occupations within each minor group.
#
# OUTPUT:
# data/processed/ilo_genai_isco4.csv
# data/processed/ilo_genai_isco3.csv
# data/processed/occupation_ai_ssi_master.csv
# ============================================================


# 0. PACKAGES -----------------------------------------------

packages <- c(
  "dplyr",
  "readr",
  "readxl",
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
library(readxl)
library(stringr)
library(here)
library(tibble)


# 1. FILE PATHS ---------------------------------------------

ai_file <- here(
  "data", "raw", "ai_exposure",
  "ILO_2025_GenAI_ISCO08.xlsx"
)

ssi_file <- here(
  "data", "processed",
  "onet_ssi_isco_minor.csv"
)

inventory_file <- here(
  "outputs", "tables",
  "plfs_verified_occupation_inventory.csv"
)

output_tables <- here("outputs", "tables")
processed_dir <- here("data", "processed")

required_files <- c(
  ai_file,
  ssi_file,
  inventory_file
)

missing_files <- required_files[!file.exists(required_files)]

if (length(missing_files) > 0) {
  stop(
    "Missing files:\n",
    paste(missing_files, collapse = "\n")
  )
}

cat("\n============================================\n")
cat("ILO GENERATIVE AI EXPOSURE PIPELINE\n")
cat("============================================\n")


# 2. LOAD DATA ----------------------------------------------

cat("\n[1/9] Loading source datasets...\n")

ai_raw <- read_excel(
  ai_file,
  sheet = "Sheet1"
)

ssi <- read_csv(
  ssi_file,
  show_col_types = FALSE
) %>%
  mutate(
    isco_minor_code = as.character(isco_minor_code)
  )

plfs_inventory <- read_csv(
  inventory_file,
  col_types = cols(
    year = col_double(),
    nco_code = col_character(),
    workers = col_integer()
  ),
  show_col_types = FALSE
)

cat("AI task records:", nrow(ai_raw), "\n")
cat("SSI occupation groups:", nrow(ssi), "\n")
cat(
  "PLFS unique occupation-year records:",
  nrow(plfs_inventory),
  "\n"
)


# 3. VALIDATE SOURCE COLUMNS -------------------------------

cat("\n[2/9] Validating AI exposure variables...\n")

required_ai <- c(
  "ISCO_08",
  "Title",
  "taskID",
  "score_2025",
  "mean_score_2025",
  "SD_2025",
  "potential25",
  "source"
)

if (!all(required_ai %in% names(ai_raw))) {
  stop(
    "Required AI exposure columns missing: ",
    paste(
      setdiff(required_ai, names(ai_raw)),
      collapse = ", "
    )
  )
}

# Standardise source fields.

ai_task <- ai_raw %>%
  transmute(
    isco4 = str_pad(
      str_trim(as.character(ISCO_08)),
      width = 4,
      side = "left",
      pad = "0"
    ),
    
    occupation_title = str_trim(
      as.character(Title)
    ),
    
    task_id = as.character(taskID),
    
    task_score = as.numeric(score_2025),
    
    occupation_mean = as.numeric(mean_score_2025),
    
    occupation_sd = as.numeric(SD_2025),
    
    exposure_category = str_trim(
      as.character(potential25)
    ),
    
    score_source = str_trim(
      as.character(source)
    )
  ) %>%
  filter(
    str_detect(isco4, "^[0-9]{4}$")
  )

if (nrow(ai_task) == 0) {
  stop("No valid four-digit ISCO occupation codes found.")
}

if (
  any(is.na(ai_task$occupation_mean)) ||
  any(is.na(ai_task$occupation_sd))
) {
  stop("Missing occupational exposure scores detected.")
}

if (
  any(ai_task$occupation_mean < 0 |
      ai_task$occupation_mean > 1)
) {
  stop("Occupation exposure scores must be between 0 and 1.")
}

cat("Valid task records:", nrow(ai_task), "\n")


# 4. VERIFY OCCUPATION-LEVEL SCORE CONSISTENCY -------------

cat("\n[3/9] Checking repeated occupation scores...\n")

occupation_consistency <- ai_task %>%
  group_by(isco4) %>%
  summarise(
    n_titles = n_distinct(occupation_title),
    n_mean_scores = n_distinct(occupation_mean),
    n_sd_scores = n_distinct(occupation_sd),
    n_categories = n_distinct(exposure_category),
    n_tasks = n_distinct(task_id),
    .groups = "drop"
  )

cat("\n--- CONSISTENCY CHECK ---\n")

print(
  occupation_consistency %>%
    summarise(
      four_digit_occupations = n(),
      inconsistent_titles = sum(n_titles > 1),
      inconsistent_mean_scores = sum(n_mean_scores > 1),
      inconsistent_sd_scores = sum(n_sd_scores > 1),
      inconsistent_categories = sum(n_categories > 1)
    ),
  width = Inf
)

if (
  any(occupation_consistency$n_mean_scores != 1) ||
  any(occupation_consistency$n_sd_scores != 1) ||
  any(occupation_consistency$n_categories != 1)
) {
  stop(
    "Occupation-level scores are inconsistent across task rows."
  )
}


# 5. COLLAPSE TO FOUR-DIGIT OCCUPATIONS ---------------------

cat("\n[4/9] Constructing occupation-level exposure...\n")

ai_isco4 <- ai_task %>%
  group_by(isco4) %>%
  summarise(
    occupation_title = first(occupation_title),
    
    AI_exposure_score = first(occupation_mean),
    
    AI_task_sd = first(occupation_sd),
    
    exposure_category = first(exposure_category),
    
    number_of_tasks = n_distinct(task_id),
    
    predicted_tasks = sum(
      score_source == "predicted",
      na.rm = TRUE
    ),
    
    reconciled_tasks = sum(
      score_source == "reconciled",
      na.rm = TRUE
    ),
    
    .groups = "drop"
  ) %>%
  mutate(
    isco3 = substr(isco4, 1, 3)
  ) %>%
  arrange(isco4)

cat("\n--- FOUR-DIGIT OCCUPATION SUMMARY ---\n")

print(
  ai_isco4 %>%
    summarise(
      occupations = n(),
      total_tasks = sum(number_of_tasks),
      mean_exposure = mean(AI_exposure_score),
      minimum_exposure = min(AI_exposure_score),
      maximum_exposure = max(AI_exposure_score)
    ),
  width = Inf
)

write_csv(
  ai_isco4,
  here(
    "data", "processed",
    "ilo_genai_isco4.csv"
  )
)


# 6. AGGREGATE TO THREE-DIGIT MINOR GROUPS ---------------

cat("\n[5/9] Aggregating to ISCO minor groups...\n")

# Each four-digit occupation receives equal weight.
# This avoids allowing occupations with more listed tasks
# to dominate the three-digit aggregate.

ai_isco3 <- ai_isco4 %>%
  group_by(isco3) %>%
  summarise(
    
    n_isco4_occupations = n_distinct(isco4),
    
    AI_exposure_mean = mean(
      AI_exposure_score,
      na.rm = TRUE
    ),
    
    AI_exposure_sd_between_occupations = sd(
      AI_exposure_score,
      na.rm = TRUE
    ),
    
    mean_within_occupation_task_sd = mean(
      AI_task_sd,
      na.rm = TRUE
    ),
    
    minimum_occupation_exposure = min(
      AI_exposure_score,
      na.rm = TRUE
    ),
    
    maximum_occupation_exposure = max(
      AI_exposure_score,
      na.rm = TRUE
    ),
    
    total_tasks = sum(number_of_tasks),
    
    .groups = "drop"
  ) %>%
  rename(
    isco_minor_code = isco3
  ) %>%
  arrange(isco_minor_code)

cat("\n--- THREE-DIGIT AI EXPOSURE SUMMARY ---\n")

print(
  ai_isco3 %>%
    summarise(
      minor_groups = n(),
      mean_AI_exposure = mean(AI_exposure_mean),
      sd_AI_exposure = sd(AI_exposure_mean),
      minimum_AI_exposure = min(AI_exposure_mean),
      maximum_AI_exposure = max(AI_exposure_mean)
    ),
  width = Inf
)

if (anyDuplicated(ai_isco3$isco_minor_code) > 0) {
  stop("Duplicate three-digit occupation groups detected.")
}

write_csv(
  ai_isco3,
  here(
    "data", "processed",
    "ilo_genai_isco3.csv"
  )
)


# 7. PRESERVE OCCUPATION EXPOSURE CATEGORIES ----------------

cat("\n[6/9] Building categorical exposure profiles...\n")

ai_category_profile <- ai_isco4 %>%
  count(
    isco3,
    exposure_category,
    name = "n_occupations"
  ) %>%
  group_by(isco3) %>%
  mutate(
    share_of_occupations = n_occupations / sum(n_occupations)
  ) %>%
  ungroup() %>%
  arrange(isco3, exposure_category)

write_csv(
  ai_category_profile,
  here(
    "outputs", "tables",
    "ilo_genai_isco3_category_profile.csv"
  )
)


# 8. STANDARDISE AI EXPOSURE FOR PLFS OCCUPATIONS ----------

cat("\n[7/9] Preparing PLFS-linked AI exposure...\n")

plfs_minor_codes <- plfs_inventory %>%
  distinct(nco_code) %>%
  pull(nco_code)

ai_plfs_groups <- ai_isco3 %>%
  filter(
    isco_minor_code %in% plfs_minor_codes
  )

if (nrow(ai_plfs_groups) < 2) {
  stop("Insufficient PLFS-linked AI occupation groups.")
}

# Standardisation uses the unweighted distribution of
# unique PLFS occupation groups, not worker counts.

ai_plfs_groups <- ai_plfs_groups %>%
  mutate(
    AI_exposure_z = as.numeric(
      scale(AI_exposure_mean)
    )
  )

# Combine AI exposure with the existing SSI index.

occupation_ai_ssi_master <- full_join(
  ai_isco3,
  ssi,
  by = "isco_minor_code"
)

write_csv(
  occupation_ai_ssi_master,
  here(
    "data", "processed",
    "occupation_ai_ssi_master.csv"
  )
)


# 9. PLFS COVERAGE AND MATCHING AUDIT ------------------------

cat("\n[8/9] Checking AI and SSI coverage in PLFS...\n")

plfs_ai_ssi <- plfs_inventory %>%
  left_join(
    ai_plfs_groups,
    by = c("nco_code" = "isco_minor_code"),
    relationship = "many-to-one"
  ) %>%
  left_join(
    ssi,
    by = c("nco_code" = "isco_minor_code"),
    relationship = "many-to-one"
  )

plfs_ai_ssi_coverage <- plfs_ai_ssi %>%
  group_by(year) %>%
  summarise(
    
    total_occupation_codes = n_distinct(nco_code),
    
    AI_matched_codes = n_distinct(
      nco_code[!is.na(AI_exposure_mean)]
    ),
    
    SSI_matched_codes = n_distinct(
      nco_code[!is.na(SSI_raw_mean)]
    ),
    
    AI_SSI_matched_codes = n_distinct(
      nco_code[
        !is.na(AI_exposure_mean) &
          !is.na(SSI_raw_mean)
      ]
    ),
    
    total_worker_records = sum(workers),
    
    AI_worker_coverage = round(
      100 * sum(
        workers[!is.na(AI_exposure_mean)]
      ) / sum(workers),
      2
    ),
    
    SSI_worker_coverage = round(
      100 * sum(
        workers[!is.na(SSI_raw_mean)]
      ) / sum(workers),
      2
    ),
    
    joint_worker_coverage = round(
      100 * sum(
        workers[
          !is.na(AI_exposure_mean) &
            !is.na(SSI_raw_mean)
        ]
      ) / sum(workers),
      2
    ),
    
    .groups = "drop"
  )

cat("\n--- PLFS AI AND SSI COVERAGE ---\n")

print(
  plfs_ai_ssi_coverage,
  width = Inf
)

write_csv(
  plfs_ai_ssi_coverage,
  here(
    "outputs", "tables",
    "plfs_ai_ssi_coverage.csv"
  )
)


# Unmatched PLFS occupation groups.

unmatched_plfs <- plfs_ai_ssi %>%
  filter(
    is.na(AI_exposure_mean) |
      is.na(SSI_raw_mean)
  ) %>%
  select(
    year,
    nco_code,
    workers,
    AI_exposure_mean,
    SSI_raw_mean
  )

write_csv(
  unmatched_plfs,
  here(
    "outputs", "tables",
    "plfs_ai_ssi_unmatched.csv"
  )
)


# 10. FINAL QUALITY CHECKS ----------------------------------

cat("\n[9/9] Final quality checks...\n")

if (
  any(ai_isco3$AI_exposure_mean < 0) ||
  any(ai_isco3$AI_exposure_mean > 1)
) {
  stop("Aggregated AI exposure falls outside [0,1].")
}

if (anyDuplicated(occupation_ai_ssi_master$isco_minor_code) > 0) {
  stop("Duplicate ISCO groups in final occupation master.")
}

cat("\n--- FOUR-DIGIT OCCUPATION COVERAGE ---\n")
print(
  ai_isco4 %>%
    summarise(
      occupations = n(),
      tasks = sum(number_of_tasks),
      predicted_tasks = sum(predicted_tasks),
      reconciled_tasks = sum(reconciled_tasks)
    ),
  width = Inf
)

cat("\n--- FINAL PLFS AI-SSI COVERAGE ---\n")
print(plfs_ai_ssi_coverage, width = Inf)

# Save R objects for further analysis.

saveRDS(
  ai_isco3,
  here(
    "data", "processed",
    "ilo_genai_isco3.rds"
  )
)

saveRDS(
  occupation_ai_ssi_master,
  here(
    "data", "processed",
    "occupation_ai_ssi_master.rds"
  )
)

saveRDS(
  plfs_ai_ssi,
  here(
    "data", "processed",
    "plfs_ai_ssi_coverage.rds"
  )
)

cat("\n============================================\n")
cat("AI EXPOSURE CONSTRUCTION COMPLETED\n")
cat("============================================\n")

cat(
  "\nFour-digit ISCO occupations:",
  nrow(ai_isco4),
  "\n"
)

cat(
  "Three-digit ISCO groups:",
  nrow(ai_isco3),
  "\n"
)

cat(
  "PLFS occupational groups:",
  length(plfs_minor_codes),
  "\n"
)

cat(
  "\nNext stage: merge AI exposure into the CPI-adjusted",
  "worker-level dataset and estimate empirical models.\n"
)
