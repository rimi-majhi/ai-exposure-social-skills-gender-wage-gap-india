
# ============================================================
# PROJECT: THE EMPATHY PREMIUM IN INDIA
# SCRIPT: 11_build_nco_isco_crosswalk.R
#
# PURPOSE:
# Construct and validate the NCO-2015 to ISCO-08
# three-digit minor-group correspondence.
#
# MAPPING BASIS:
# NCO-2015 first three digits correspond to the
# ISCO-08 minor group.
#
# SOURCES:
# DGE, National Classification of Occupations 2015, Volume I.
# PLFS occupation code: NCO-2015, three-digit level.
#
# IMPORTANT:
# This script validates the classification-level mapping.
# It does not yet map O*NET occupations or SSI scores.
# ============================================================


# 0. PACKAGES -----------------------------------------------

packages <- c("dplyr", "readr", "stringr", "here", "tibble")

missing_packages <- packages[
  !vapply(packages, requireNamespace, logical(1), quietly = TRUE)
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

inventory_file <- here(
  "outputs", "tables",
  "plfs_verified_occupation_inventory.csv"
)

isco_file <- here(
  "data", "raw", "crosswalk",
  "ISCO-08 EN.csv"
)

output_dir <- here("outputs", "tables")

if (!file.exists(inventory_file)) {
  stop("PLFS occupation inventory not found: ", inventory_file)
}

if (!file.exists(isco_file)) {
  stop("ISCO classification file not found: ", isco_file)
}

if (!dir.exists(output_dir)) {
  stop("Output directory does not exist: ", output_dir)
}


# 2. LOAD INPUT DATA ----------------------------------------

cat("\n============================================\n")
cat("BUILDING NCO-2015 / ISCO-08 CROSSWALK\n")
cat("============================================\n")

plfs_inventory <- read_csv(
  inventory_file,
  col_types = cols(
    year = col_double(),
    nco_code = col_character(),
    workers = col_integer()
  ),
  show_col_types = FALSE
)

isco_raw <- read_csv(
  isco_file,
  col_types = cols(.default = col_character()),
  show_col_types = FALSE
)

required_inventory <- c("year", "nco_code", "workers")
required_isco <- c("minor", "minor_label")

if (!all(required_inventory %in% names(plfs_inventory))) {
  stop("PLFS inventory has unexpected column names.")
}

if (!all(required_isco %in% names(isco_raw))) {
  stop("ISCO file has unexpected column names.")
}

cat("\nPLFS inventory records:", nrow(plfs_inventory), "\n")
cat("ISCO source records:", nrow(isco_raw), "\n")


# 3. VALIDATE NCO CODE FORMAT -------------------------------

cat("\n--- VALIDATING PLFS OCCUPATION CODES ---\n")

plfs_inventory <- plfs_inventory %>%
  mutate(
    nco_code = str_trim(nco_code),
    valid_nco_code = str_detect(nco_code, "^[0-9]{3}$")
  )

nco_format_audit <- plfs_inventory %>%
  group_by(year) %>%
  summarise(
    total_codes = n_distinct(nco_code),
    valid_three_digit_codes = n_distinct(
      nco_code[valid_nco_code]
    ),
    invalid_codes = n_distinct(
      nco_code[!valid_nco_code]
    ),
    total_worker_records = sum(workers),
    .groups = "drop"
  )

print(nco_format_audit)

write_csv(
  nco_format_audit,
  here(
    "outputs", "tables",
    "nco_code_format_audit.csv"
  )
)

if (any(!plfs_inventory$valid_nco_code)) {
  stop(
    "Invalid NCO codes detected. Inspect nco_code_format_audit.csv."
  )
}


# 4. CONSTRUCT ISCO MINOR-GROUP REFERENCE -------------------

cat("\n--- EXTRACTING ISCO MINOR-GROUP CODES ---\n")

isco_minor_reference <- isco_raw %>%
  transmute(
    isco_minor_code = str_trim(as.character(minor)),
    description = str_trim(as.character(minor_label))
  ) %>%
  filter(
    !is.na(isco_minor_code),
    str_detect(isco_minor_code, "^[0-9]{3}$")
  ) %>%
  group_by(isco_minor_code) %>%
  summarise(
    isco_code_listed = TRUE,
    
    n_descriptions = n_distinct(
      description[
        !is.na(description) & description != ""
      ]
    ),
    
    description_candidates = paste(
      sort(unique(
        description[
          !is.na(description) & description != ""
        ]
      )),
      collapse = " | "
    ),
    
    .groups = "drop"
  ) %>%
  arrange(isco_minor_code)

cat(
  "Unique three-digit ISCO codes:",
  nrow(isco_minor_reference),
  "\n"
)

write_csv(
  isco_minor_reference,
  here(
    "outputs", "tables",
    "isco_minor_group_reference.csv"
  )
)


# 5. BUILD NCO TO ISCO MAPPING -------------------------------

cat("\n--- BUILDING CLASSIFICATION CROSSWALK ---\n")

# Official hierarchical correspondence:
# NCO-2015 first three digits = ISCO-08 minor group.

nco_isco_crosswalk <- plfs_inventory %>%
  distinct(nco_code) %>%
  mutate(
    isco_minor_code = nco_code,
    mapping_basis = paste(
      "NCO-2015 first three digits correspond",
      "to ISCO-08 minor group"
    )
  ) %>%
  left_join(
    isco_minor_reference,
    by = "isco_minor_code"
  ) %>%
  mutate(
    mapping_status = if_else(
      !is.na(isco_code_listed),
      "Matched",
      "Unmatched"
    )
  ) %>%
  arrange(nco_code)

print(
  nco_isco_crosswalk %>%
    count(mapping_status)
)

write_csv(
  nco_isco_crosswalk,
  here(
    "outputs", "tables",
    "nco2015_isco08_minor_crosswalk.csv"
  )
)


# 6. CHECK FOR UNMATCHED OCCUPATION CODES --------------------

cat("\n--- UNMATCHED CODES ---\n")

unmatched_codes <- nco_isco_crosswalk %>%
  filter(mapping_status == "Unmatched")

print(unmatched_codes, n = Inf)

write_csv(
  unmatched_codes,
  here(
    "outputs", "tables",
    "nco_isco_unmatched_codes.csv"
  )
)


# 7. YEAR-WISE COVERAGE -------------------------------------

cat("\n--- YEAR-WISE CROSSWALK COVERAGE ---\n")

mapped_inventory <- plfs_inventory %>%
  mutate(
    isco_minor_code = nco_code
  ) %>%
  left_join(
    nco_isco_crosswalk %>%
      select(
        nco_code,
        isco_code_listed,
        mapping_status
      ),
    by = "nco_code"
  )

crosswalk_coverage <- mapped_inventory %>%
  group_by(year) %>%
  summarise(
    total_occupation_codes = n_distinct(nco_code),
    
    matched_occupation_codes = n_distinct(
      nco_code[mapping_status == "Matched"]
    ),
    
    unmatched_occupation_codes = n_distinct(
      nco_code[mapping_status == "Unmatched"]
    ),
    
    total_worker_records = sum(workers),
    
    matched_worker_records = sum(
      workers[mapping_status == "Matched"]
    ),
    
    coverage_percentage = round(
      100 * matched_worker_records / total_worker_records,
      2
    ),
    
    .groups = "drop"
  )

print(crosswalk_coverage)

write_csv(
  crosswalk_coverage,
  here(
    "outputs", "tables",
    "nco_isco_coverage_by_year.csv"
  )
)


# 8. INSPECT DESCRIPTION AMBIGUITIES -------------------------

cat("\n--- DESCRIPTION AMBIGUITIES ---\n")

description_ambiguities <- isco_minor_reference %>%
  filter(n_descriptions > 1)

print(description_ambiguities, n = Inf)

write_csv(
  description_ambiguities,
  here(
    "outputs", "tables",
    "isco_minor_description_ambiguities.csv"
  )
)


# 9. CHECK DUPLICATES ----------------------------------------

cat("\n--- CROSSWALK UNIQUENESS TEST ---\n")

duplicate_check <- nco_isco_crosswalk %>%
  count(nco_code) %>%
  filter(n > 1)

cat("Duplicate NCO mappings:", nrow(duplicate_check), "\n")

if (nrow(duplicate_check) > 0) {
  stop("Duplicate NCO codes detected in crosswalk.")
}

if (any(nco_isco_crosswalk$mapping_status == "Unmatched")) {
  warning(
    "Some occupation codes remain unmatched. ",
    "Review the unmatched-code output."
  )
} else {
  cat("All PLFS occupation codes matched to ISCO minor groups.\n")
}


# 10. CREATE FINAL MAPPED INVENTORY --------------------------

write_csv(
  mapped_inventory,
  here(
    "outputs", "tables",
    "plfs_occupation_inventory_with_isco.csv"
  )
)


# 11. SAVE RESEARCH METADATA ---------------------------------

research_metadata <- tibble(
  parameter = c(
    "Classification_source",
    "PLFS_occupation_level",
    "NCO_2015_mapping_level",
    "ISCO_08_mapping_level",
    "Mapping_method",
    "Years",
    "Worker_weights"
  ),
  
  value = c(
    "Government of India, NCO-2015 Volume I",
    "Three-digit NCO-2015 occupation code",
    "Three-digit group",
    "Three-digit minor group",
    "Official hierarchical correspondence",
    "2022 and 2024",
    "Inventory counts are unweighted records"
  )
)

write_csv(
  research_metadata,
  here(
    "outputs", "tables",
    "nco_isco_research_metadata.csv"
  )
)


# 12. FINAL STATUS -------------------------------------------

cat("\n============================================\n")
cat("NCO-ISCO CROSSWALK BUILD COMPLETED\n")
cat("============================================\n")

cat("\nUnique NCO codes:", nrow(nco_isco_crosswalk), "\n")
cat(
  "Unmatched codes:",
  nrow(unmatched_codes),
  "\n"
)

cat("\nFiles generated:\n")

cat(
  paste(
    c(
      "nco_code_format_audit.csv",
      "isco_minor_group_reference.csv",
      "nco2015_isco08_minor_crosswalk.csv",
      "nco_isco_unmatched_codes.csv",
      "nco_isco_coverage_by_year.csv",
      "isco_minor_description_ambiguities.csv",
      "plfs_occupation_inventory_with_isco.csv",
      "nco_isco_research_metadata.csv"
    ),
    collapse = "\n"
  ),
  "\n"
)

cat("\nNext stage: aggregate O*NET SSI to ISCO minor groups.\n")
