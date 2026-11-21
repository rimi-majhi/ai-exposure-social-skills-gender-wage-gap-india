
# ============================================================
# PROJECT: THE EMPATHY PREMIUM IN INDIA
# SCRIPT: 10_occupation_classification_audit.R
# PURPOSE:
#   1. Inspect PLFS occupation codes (2021-22 and 2023-24)
#   2. Audit the structure of the ISCO-08 classification file
#   3. Identify duplicate and ambiguous ISCO descriptions
#   4. Compare code formats without assuming equivalence
#   5. Export reproducible diagnostic tables for the project
#
# IMPORTANT:
#   This script does NOT establish an NCO-2015 to ISCO-08
#   correspondence. A verified classification concordance
#   is required before assigning O*NET social-skill scores.
# ============================================================


# 0. INITIAL SETUP --------------------------------------------

rm(list = ls())

required_packages <- c(
  "dplyr",
  "readr",
  "stringr",
  "here",
  "tibble"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace,
          logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "Install these packages first: ",
    paste(missing_packages, collapse = ", ")
  )
}

library(dplyr)
library(readr)
library(stringr)
library(here)
library(tibble)

cat("\n============================================\n")
cat("OCCUPATION CLASSIFICATION AUDIT\n")
cat("============================================\n")


# 1. DEFINE PROJECT FILES -------------------------------------

plfs_2022_zip <- here(
  "data", "raw", "plfs",
  "PLFS_Data_2021-22_CSV.zip"
)

plfs_2024_zip <- here(
  "data", "raw", "plfs",
  "CSV_data_PLFS_2023_2024.zip"
)

isco_file <- here(
  "data", "raw", "crosswalk",
  "ISCO-08 EN.csv"
)

output_dir <- here("outputs", "tables")

if (!dir.exists(output_dir)) {
  stop("Expected output directory does not exist: ", output_dir)
}

required_files <- c(
  plfs_2022_zip,
  plfs_2024_zip,
  isco_file
)

missing_files <- required_files[!file.exists(required_files)]

if (length(missing_files) > 0) {
  stop(
    "The following input files are missing:\n",
    paste(missing_files, collapse = "\n")
  )
}


# 2. LOAD PLFS FIRST-VISIT PERSON FILES -----------------------

cat("\n[1/6] Loading PLFS files...\n")

plfs_2022 <- read_csv(
  unz(
    plfs_2022_zip,
    "PLFS_Data_2021-22_CSV/perv1.csv"
  ),
  show_col_types = FALSE
) %>%
  mutate(year = 2022)

plfs_2024 <- read_csv(
  unz(
    plfs_2024_zip,
    "CSV_data_PLFS_2023_2024/perv1.csv"
  ),
  show_col_types = FALSE
) %>%
  mutate(year = 2024)

cat("2021-22 records:", nrow(plfs_2022), "\n")
cat("2023-24 records:", nrow(plfs_2024), "\n")


# 3. EXTRACT PLFS OCCUPATION CODES ----------------------------

cat("\n[2/6] Extracting PLFS occupation codes...\n")

occupation_variable <- "b6q7_perv1"

if (
  !occupation_variable %in% names(plfs_2022) ||
  !occupation_variable %in% names(plfs_2024)
) {
  stop("Occupation variable not found in both PLFS files.")
}

extract_occupation_codes <- function(data, survey_year) {
  
  data %>%
    transmute(
      year = survey_year,
      occupation_raw = as.character(
        .data[[occupation_variable]]
      ),
      occupation_code = str_trim(occupation_raw)
    ) %>%
    filter(
      !is.na(occupation_code),
      occupation_code != ""
    ) %>%
    mutate(
      occupation_code = str_pad(
        occupation_code,
        width = 3,
        side = "left",
        pad = "0"
      )
    ) %>%
    count(
      year,
      occupation_code,
      name = "worker_records"
    ) %>%
    arrange(year, occupation_code)
}

plfs_occupation_codes <- bind_rows(
  extract_occupation_codes(plfs_2022, 2022),
  extract_occupation_codes(plfs_2024, 2024)
)

occupation_summary <- plfs_occupation_codes %>%
  group_by(year) %>%
  summarise(
    unique_occupation_codes = n(),
    records_with_occupation_code = sum(worker_records),
    .groups = "drop"
  )

print(occupation_summary)

write_csv(
  plfs_occupation_codes,
  file.path(output_dir, "plfs_occupation_code_inventory.csv")
)

write_csv(
  occupation_summary,
  file.path(output_dir, "plfs_occupation_code_summary.csv")
)


# 4. LOAD AND INSPECT ISCO-08 FILE ----------------------------

cat("\n[3/6] Loading ISCO-08 classification...\n")

isco <- read_csv(
  isco_file,
  show_col_types = FALSE
)

cat("\nISCO column names:\n")
print(names(isco))

required_isco_columns <- c(
  "ISCO_version",
  "major",
  "major_label",
  "sub_major",
  "sub_major_label",
  "minor",
  "minor_label",
  "unit",
  "description"
)

missing_isco_columns <- setdiff(
  required_isco_columns,
  names(isco)
)

if (length(missing_isco_columns) > 0) {
  stop(
    "Unexpected ISCO file structure. Missing columns: ",
    paste(missing_isco_columns, collapse = ", ")
  )
}

isco_structure <- tibble(
  rows = nrow(isco),
  unique_major_codes = n_distinct(isco$major),
  unique_sub_major_codes = n_distinct(isco$sub_major),
  unique_minor_codes = n_distinct(isco$minor),
  unique_unit_codes = n_distinct(isco$unit),
  missing_unit_codes = sum(is.na(isco$unit)),
  missing_minor_codes = sum(is.na(isco$minor))
)

print(isco_structure)

write_csv(
  isco_structure,
  file.path(output_dir, "isco_classification_structure.csv")
)


# 5. AUDIT ISCO MINOR-GROUP LABELS ----------------------------

cat("\n[4/6] Auditing ISCO minor-group labels...\n")

isco_minor_audit <- isco %>%
  transmute(
    code = as.character(minor),
    description = str_trim(as.character(minor_label))
  ) %>%
  filter(
    !is.na(code),
    code != "",
    !is.na(description),
    description != ""
  ) %>%
  group_by(code) %>%
  summarise(
    descriptions = paste(
      sort(unique(description)),
      collapse = " | "
    ),
    n_descriptions = n_distinct(description),
    .groups = "drop"
  ) %>%
  arrange(code)

isco_duplicate_labels <- isco_minor_audit %>%
  filter(n_descriptions > 1)

print(
  isco_minor_audit %>%
    count(n_descriptions),
  n = Inf
)

cat(
  "\nMinor-group codes with multiple descriptions:",
  nrow(isco_duplicate_labels),
  "\n"
)

write_csv(
  isco_minor_audit,
  file.path(output_dir, "isco_minor_group_label_audit.csv")
)

write_csv(
  isco_duplicate_labels,
  file.path(output_dir, "isco_minor_group_ambiguities.csv")
)


# 6. INSPECT ISCO UNIT-GROUP CODES ----------------------------

cat("\n[5/6] Inspecting ISCO unit-group codes...\n")

isco_unit_audit <- isco %>%
  transmute(
    unit_code = as.character(unit),
    unit_description = str_trim(as.character(description)),
    minor_code = as.character(minor),
    minor_description = str_trim(as.character(minor_label))
  ) %>%
  filter(
    !is.na(unit_code),
    unit_code != ""
  ) %>%
  distinct() %>%
  arrange(unit_code)

isco_unit_duplicates <- isco_unit_audit %>%
  count(unit_code, name = "n_records") %>%
  filter(n_records > 1)

cat("Unique ISCO unit-group codes:",
    n_distinct(isco_unit_audit$unit_code), "\n")

cat("Unit-group codes with multiple records:",
    nrow(isco_unit_duplicates), "\n")

write_csv(
  isco_unit_audit,
  file.path(output_dir, "isco_unit_group_inventory.csv")
)

write_csv(
  isco_unit_duplicates,
  file.path(output_dir, "isco_unit_group_duplicates.csv")
)


# 7. COMPARE PLFS AND ISCO CODE FORMATS -----------------------

cat("\n[6/6] Comparing code formats...\n")

plfs_code_format <- plfs_occupation_codes %>%
  distinct(year, occupation_code) %>%
  mutate(
    code_length = nchar(occupation_code),
    numeric_code = str_detect(occupation_code, "^[0-9]+$")
  )

isco_code_format <- isco %>%
  transmute(
    minor_code = as.character(minor),
    unit_code = as.character(unit),
    minor_code_length = nchar(minor_code),
    unit_code_length = nchar(unit_code)
  )

format_summary <- plfs_code_format %>%
  group_by(year) %>%
  summarise(
    unique_codes = n(),
    three_digit_codes = sum(code_length == 3),
    numeric_codes = sum(numeric_code),
    .groups = "drop"
  )

print(format_summary)

write_csv(
  format_summary,
  file.path(output_dir, "occupation_code_format_comparison.csv")
)


# 8. DIRECT DIGIT MATCH: DIAGNOSTIC ONLY ----------------------

# This section checks whether PLFS codes appear among ISCO
# minor-group codes. A numerical match does NOT establish
# classification equivalence.

isco_minor_codes <- isco %>%
  transmute(
    comparison_code = as.character(minor)
  ) %>%
  filter(
    !is.na(comparison_code),
    comparison_code != ""
  ) %>%
  distinct()

direct_match_diagnostic <- plfs_occupation_codes %>%
  distinct(year, occupation_code) %>%
  mutate(
    direct_digit_match = occupation_code %in%
      isco_minor_codes$comparison_code
  ) %>%
  group_by(year) %>%
  summarise(
    total_plfs_codes = n(),
    digit_matches = sum(direct_digit_match),
    digit_nonmatches = sum(!direct_digit_match),
    match_percentage = round(
      100 * mean(direct_digit_match), 2
    ),
    .groups = "drop"
  )

print(direct_match_diagnostic)

write_csv(
  direct_match_diagnostic,
  file.path(output_dir, "direct_digit_match_diagnostic.csv")
)


# 9. CREATE A RESEARCH AUDIT SUMMARY --------------------------

audit_summary <- tibble(
  audit_item = c(
    "PLFS 2021-22 unique occupation codes",
    "PLFS 2023-24 unique occupation codes",
    "ISCO minor-group code count",
    "ISCO minor codes with multiple descriptions",
    "ISCO unit-group code count",
    "PLFS-to-ISCO direct digit match is validated"
  ),
  result = c(
    as.character(
      occupation_summary$unique_occupation_codes[
        occupation_summary$year == 2022
      ]
    ),
    as.character(
      occupation_summary$unique_occupation_codes[
        occupation_summary$year == 2024
      ]
    ),
    as.character(n_distinct(isco$minor)),
    as.character(nrow(isco_duplicate_labels)),
    as.character(n_distinct(isco$unit)),
    "NO"
  )
)

print(audit_summary)

write_csv(
  audit_summary,
  file.path(output_dir, "occupation_classification_audit_summary.csv")
)


# 10. FINAL STATUS --------------------------------------------

cat("\n============================================\n")
cat("AUDIT COMPLETED\n")
cat("============================================\n")

cat("\nFiles saved in outputs/tables:\n")

cat(
  paste(
    list.files(
      output_dir,
      pattern = "occupation|isco|plfs",
      full.names = FALSE
    ),
    collapse = "\n"
  ),
  "\n"
)

cat(
  "\nResearch status: classification diagnostics completed.\n",
  "NCO-2015 to ISCO-08 correspondence remains unverified.\n",
  "Do not merge SSI into PLFS until a defensible crosswalk is established.\n"
)
library(dplyr)
library(stringr)

plfs_occupation_detail <- bind_rows(
  plfs21 %>% mutate(year = 2022),
  plfs23 %>% mutate(year = 2024)
) %>%
  transmute(
    year,
    nco_code_raw = as.character(b6q7_perv1),
    nco_code = str_trim(nco_code_raw)
  ) %>%
  filter(!is.na(nco_code), nco_code != "") %>%
  count(year, nco_code, name = "workers") %>%
  mutate(
    nco_digits = str_remove_all(nco_code, "[^0-9]"),
    nco_length = nchar(nco_digits),
    nco_major = substr(nco_digits, 1, 1),
    nco_submajor = substr(nco_digits, 1, 2),
    nco_minor = substr(nco_digits, 1, 3),
    nco_unit = substr(nco_digits, 1, 4)
  )

print(plfs_occupation_detail, n = 30)

print(
  plfs_occupation_detail %>%
    count(year, nco_length)
)
library(readr)
library(dplyr)
library(here)

z21 <- here(
  "data", "raw", "plfs",
  "PLFS_Data_2021-22_CSV.zip"
)

z23 <- here(
  "data", "raw", "plfs",
  "CSV_data_PLFS_2023_2024.zip"
)

plfs21 <- read_csv(
  unz(z21, "PLFS_Data_2021-22_CSV/perv1.csv"),
  show_col_types = FALSE
)

plfs23 <- read_csv(
  unz(z23, "CSV_data_PLFS_2023_2024/perv1.csv"),
  show_col_types = FALSE
)

cat("2021-22:", nrow(plfs21), "records\n")
cat("2023-24:", nrow(plfs23), "records\n")
library(dplyr)
library(stringr)

plfs_occupation_detail <- bind_rows(
  plfs21 %>% mutate(year = 2022),
  plfs23 %>% mutate(year = 2024)
) %>%
  transmute(
    year,
    nco_code = str_trim(as.character(b6q7_perv1))
  ) %>%
  filter(!is.na(nco_code), nco_code != "") %>%
  count(year, nco_code, name = "workers") %>%
  mutate(
    nco_digits = str_remove_all(nco_code, "[^0-9]"),
    nco_length = nchar(nco_digits)
  )

cat("\n--- OCCUPATION CODE LENGTHS ---\n")
print(
  plfs_occupation_detail %>%
    count(year, nco_length)
)

cat("\n--- FIRST 30 OCCUPATION CODES ---\n")
print(
  plfs_occupation_detail %>%
    arrange(year, nco_code) %>%
    group_by(year) %>%
    slice_head(n = 30),
  n = 60
)
library(dplyr)
library(stringr)

cat("\n--- 2021-22 OCCUPATION VARIABLES ---\n")
print(grep("b6q7", names(plfs21), value = TRUE))

cat("\n--- 2023-24 OCCUPATION VARIABLES ---\n")
print(grep("b6q7", names(plfs23), value = TRUE))

cat("\n--- MAIN OCCUPATION VARIABLE: 2021-22 ---\n")
print(table(plfs21$b6q7_perv1, useNA = "ifany")[1:30])

cat("\n--- MAIN OCCUPATION VARIABLE: 2023-24 ---\n")
print(table(plfs23$b6q7_perv1, useNA = "ifany")[1:30])

cat("\n--- DETAILED OCCUPATION COMPONENTS: 2023-24 ---\n")
print(
  plfs23 %>%
    select(starts_with("b6q7_3pt")) %>%
    head(10)
)
library(dplyr)

cat("\n--- MAIN AND DETAILED OCCUPATION CODES ---\n")

plfs23 %>%
  select(
    b6q7_perv1,
    starts_with("b6q7_3pt")
  ) %>%
  mutate(
    reconstructed_code = paste0(
      b6q7_3pt1_perv1,
      b6q7_3pt2_perv1,
      b6q7_3pt3_perv1,
      b6q7_3pt4_perv1,
      b6q7_3pt5_perv1,
      b6q7_3pt6_perv1,
      b6q7_3pt7_perv1
    )
  ) %>%
  select(b6q7_perv1, reconstructed_code) %>%
  distinct() %>%
  head(30) %>%
  print(n = 30)

cat("\n--- MAIN VARIABLE STRUCTURE ---\n")
str(plfs23$b6q7_perv1)

cat("\n--- DETAILED VARIABLE STRUCTURE ---\n")
str(plfs23$b6q7_3pt1_perv1)
library(dplyr)

# Identify the seven detailed occupation fields
digit_cols <- paste0(
  "b6q7_3pt", 1:7, "_perv1"
)

# Reconstruct the seven-digit code
reconstructed <- do.call(
  paste0,
  lapply(plfs23[digit_cols], function(x) {
    ifelse(is.na(x), "", as.character(as.integer(x)))
  })
)

# Compare with the main occupation variable
occupation_comparison <- tibble(
  main_code = as.character(plfs23$b6q7_perv1),
  reconstructed_code = reconstructed
) %>%
  mutate(
    matches = main_code == reconstructed_code
  )

cat("\n--- FIRST 20 COMPARISONS ---\n")
print(head(occupation_comparison, 20))

cat("\n--- MATCHING SUMMARY ---\n")
print(
  occupation_comparison %>%
    summarise(
      total = n(),
      matches = sum(matches, na.rm = TRUE),
      mismatches = sum(!matches, na.rm = TRUE),
      missing_main = sum(is.na(main_code)),
      match_percentage = round(
        mean(matches, na.rm = TRUE) * 100, 2
      )
    )
)

cat("\n--- DISTINCT RECONSTRUCTED CODES ---\n")
print(
  occupation_comparison %>%
    filter(reconstructed_code != "") %>%
    count(reconstructed_code, sort = TRUE) %>%
    head(20)
)
library(dplyr)
library(tibble)

# Step 1: Define detailed occupation digit columns
digit_cols <- paste0("b6q7_3pt", 1:7, "_perv1")

# Step 2: Verify all columns exist
stopifnot(all(digit_cols %in% names(plfs23)))

# Step 3: Reconstruct occupation codes
reconstructed <- do.call(
  paste0,
  lapply(plfs23[digit_cols], function(x) {
    ifelse(is.na(x), "", as.character(as.integer(x)))
  })
)

# Step 4: Create comparison object
occupation_comparison <- tibble(
  main_code = as.character(plfs23$b6q7_perv1),
  reconstructed_code = reconstructed
) %>%
  mutate(
    matches = main_code == reconstructed_code
  )

# Step 5: Print results
cat("\n--- FIRST 20 COMPARISONS ---\n")
print(head(occupation_comparison, 20))

cat("\n--- MATCHING SUMMARY ---\n")
print(
  occupation_comparison %>%
    summarise(
      total = n(),
      matches = sum(matches, na.rm = TRUE),
      mismatches = sum(!matches, na.rm = TRUE),
      missing_main = sum(is.na(main_code)),
      match_percentage = round(
        mean(matches, na.rm = TRUE) * 100, 2
      )
    )
)

cat("\n--- MOST COMMON RECONSTRUCTED CODES ---\n")
print(
  occupation_comparison %>%
    filter(reconstructed_code != "") %>%
    count(reconstructed_code, sort = TRUE) %>%
    head(20)
)
