
# ============================================================
# PROJECT: THE EMPATHY PREMIUM IN INDIA
# SCRIPT: 12_aggregate_onet_ssi.R
#
# PURPOSE:
# Aggregate O*NET social-skill scores to ISCO-08
# three-digit minor groups for subsequent PLFS analysis.
#
# MAPPING CHAIN:
# O*NET-SOC 2019
#       |
# O*NET-SOC 2010
#       |
# SOC 2010
#       |
# ISCO-08 UNIT GROUP
#       |
# ISCO-08 MINOR GROUP
#
# AGGREGATION:
# Equal-weighted mean across unique O*NET-SOC 2019
# occupations linked to each ISCO minor group.
#
# IMPORTANT:
# This is an occupation-level proxy, not an individual
# measure of empathy.
#
# ============================================================


# 0. PACKAGES -----------------------------------------------

packages <- c(
  "dplyr",
  "readr",
  "stringr",
  "haven",
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
library(haven)
library(here)
library(tibble)


# 1. FILE PATHS ---------------------------------------------

ssi_file <- here(
  "data", "processed",
  "onet_social_skills_index.csv"
)

onet_crosswalk_file <- here(
  "data", "raw", "crosswalk",
  "2010_to_2019_Crosswalk.csv"
)

soc_isco_file <- here(
  "data", "raw", "crosswalk",
  "soc10_isco08.dta"
)

isco_file <- here(
  "data", "raw", "crosswalk",
  "ISCO-08 EN.csv"
)

output_dir <- here("outputs", "tables")
processed_dir <- here("data", "processed")

input_files <- c(
  ssi_file,
  onet_crosswalk_file,
  soc_isco_file,
  isco_file
)

if (any(!file.exists(input_files))) {
  stop(
    "Missing input files:\n",
    paste(input_files[!file.exists(input_files)],
          collapse = "\n")
  )
}

cat("\n============================================\n")
cat("O*NET SSI AGGREGATION PIPELINE\n")
cat("============================================\n")


# 2. LOAD DATA ----------------------------------------------

cat("\n[1/9] Loading datasets...\n")

ssi <- read_csv(
  ssi_file,
  show_col_types = FALSE
)

onet_crosswalk <- read_csv(
  onet_crosswalk_file,
  show_col_types = FALSE
)

soc_isco <- read_dta(
  soc_isco_file
)

isco_raw <- read_csv(
  isco_file,
  col_types = cols(.default = col_character()),
  show_col_types = FALSE
)

cat("SSI observations:", nrow(ssi), "\n")
cat("O*NET crosswalk observations:",
    nrow(onet_crosswalk), "\n")
cat("SOC-ISCO observations:", nrow(soc_isco), "\n")
cat("ISCO reference observations:",
    nrow(isco_raw), "\n")


# 3. VALIDATE SOURCE STRUCTURE -------------------------------

cat("\n[2/9] Validating source columns...\n")

required_ssi <- c(
  "onet_soc",
  "occupation",
  "SSI_raw",
  "SSI_z"
)

required_onet <- c(
  "O*NET-SOC 2010 Code",
  "O*NET-SOC 2019 Code"
)

required_soc_isco <- c(
  "soc10",
  "isco08"
)

required_isco <- c(
  "unit",
  "description"
)

if (!all(required_ssi %in% names(ssi))) {
  stop("SSI dataset columns do not match expected structure.")
}

if (!all(required_onet %in% names(onet_crosswalk))) {
  stop("O*NET crosswalk columns do not match expected structure.")
}

if (!all(required_soc_isco %in% names(soc_isco))) {
  stop("SOC-ISCO crosswalk columns do not match expected structure.")
}

if (!all(required_isco %in% names(isco_raw))) {
  stop("ISCO source columns do not match expected structure.")
}

if (anyDuplicated(ssi$onet_soc) > 0) {
  stop("Duplicate O*NET occupation codes in SSI dataset.")
}

if (anyNA(ssi$SSI_raw) || anyNA(ssi$SSI_z)) {
  stop("Missing SSI scores detected. Inspect source dataset.")
}

cat("Source structure validated.\n")


# 4. STANDARDISE CLASSIFICATION CODES ------------------------

cat("\n[3/9] Standardising occupational codes...\n")

normalise_numeric_code <- function(x, width) {
  
  x <- str_trim(as.character(x))
  
  # Remove a trailing decimal suffix from numeric imports
  x <- str_remove(x, "\\.0+$")
  
  # Retain only valid numeric strings
  x[is.na(x) | !str_detect(x, "^[0-9]+$")] <- NA_character_
  
  # Restore leading zeroes where required
  str_pad(
    x,
    width = width,
    side = "left",
    pad = "0"
  )
}


# O*NET 2010 to 2019 correspondence

onet_bridge <- onet_crosswalk %>%
  transmute(
    onet_soc_2010 = str_trim(
      as.character(`O*NET-SOC 2010 Code`)
    ),
    
    onet_soc_2019 = str_trim(
      as.character(`O*NET-SOC 2019 Code`)
    )
  ) %>%
  mutate(
    soc10_digits = str_remove_all(
      onet_soc_2010,
      "[^0-9]"
    ),
    
    soc10 = str_sub(soc10_digits, 1, 6)
  ) %>%
  filter(
    !is.na(onet_soc_2010),
    !is.na(onet_soc_2019),
    str_detect(soc10, "^[0-9]{6}$")
  ) %>%
  distinct(
    onet_soc_2010,
    onet_soc_2019,
    soc10
  )


# SOC 2010 to ISCO-08 correspondence

soc_isco_clean <- soc_isco %>%
  transmute(
    soc10 = normalise_numeric_code(soc10, 6),
    isco_unit_code = normalise_numeric_code(isco08, 4)
  ) %>%
  filter(
    !is.na(soc10),
    !is.na(isco_unit_code)
  ) %>%
  distinct(
    soc10,
    isco_unit_code
  )


# ISCO unit-group reference

isco_unit_reference <- isco_raw %>%
  transmute(
    isco_unit_code = normalise_numeric_code(unit, 4),
    unit_description = str_trim(as.character(description))
  ) %>%
  filter(!is.na(isco_unit_code)) %>%
  mutate(
    isco_minor_code = str_sub(
      isco_unit_code,
      1,
      3
    )
  ) %>%
  group_by(
    isco_unit_code,
    isco_minor_code
  ) %>%
  summarise(
    unit_description = paste(
      sort(unique(
        unit_description[
          !is.na(unit_description) &
            unit_description != ""
        ]
      )),
      collapse = " | "
    ),
    .groups = "drop"
  ) %>%
  distinct()

cat(
  "Unique O*NET 2010-2019 links:",
  nrow(onet_bridge), "\n"
)

cat(
  "Unique SOC-ISCO links:",
  nrow(soc_isco_clean), "\n"
)

cat(
  "Unique verified ISCO unit groups:",
  n_distinct(isco_unit_reference$isco_unit_code),
  "\n"
)

cat(
  "Unique ISCO minor groups:",
  n_distinct(isco_unit_reference$isco_minor_code),
  "\n"
)


# 5. CONNECT THE CLASSIFICATION SYSTEMS ----------------------

cat("\n[4/9] Building occupational mapping chain...\n")

# Left joins preserve unmatched links for diagnostics.

onet_soc_isco_all <- onet_bridge %>%
  left_join(
    soc_isco_clean,
    by = "soc10"
  ) %>%
  left_join(
    isco_unit_reference,
    by = "isco_unit_code"
  ) %>%
  mutate(
    mapping_status = case_when(
      is.na(isco_unit_code) ~ "SOC not matched",
      is.na(isco_minor_code) ~ "ISCO unit not verified",
      TRUE ~ "Mapped"
    )
  )

# Retain only verified links for SSI aggregation.
# Distinct ensures an O*NET occupation is counted once
# within a given ISCO minor group.

valid_onet_isco_links <- onet_soc_isco_all %>%
  filter(
    mapping_status == "Mapped"
  ) %>%
  distinct(
    onet_soc_2019,
    isco_minor_code
  )

cat(
  "Valid O*NET-to-ISCO minor-group links:",
  nrow(valid_onet_isco_links), "\n"
)


# 6. MAPPING COVERAGE AUDIT -------------------------------

cat("\n[5/9] Calculating mapping coverage...\n")

ssi_occupation_codes <- ssi %>%
  distinct(onet_soc)

all_ssi_codes <- ssi_occupation_codes$onet_soc

crosswalk_2019_codes <- unique(
  onet_bridge$onet_soc_2019
)

soc_matched_codes <- onet_soc_isco_all %>%
  filter(!is.na(isco_unit_code)) %>%
  distinct(onet_soc_2019) %>%
  pull(onet_soc_2019)

isco_verified_codes <- valid_onet_isco_links %>%
  distinct(onet_soc_2019) %>%
  pull(onet_soc_2019)

n_total <- length(all_ssi_codes)

coverage_values <- tibble(
  mapping_stage = c(
    "SSI occupations in O*NET 2010-2019 crosswalk",
    "SSI occupations linked to SOC-ISCO",
    "SSI occupations linked to verified ISCO groups"
  ),
  
  matched_occupations = c(
    sum(all_ssi_codes %in% crosswalk_2019_codes),
    sum(all_ssi_codes %in% soc_matched_codes),
    sum(all_ssi_codes %in% isco_verified_codes)
  )
) %>%
  mutate(
    total_ssi_occupations = n_total,
    unmatched_occupations =
      total_ssi_occupations - matched_occupations,
    
    coverage_percentage = round(
      100 * matched_occupations /
        total_ssi_occupations,
      2
    )
  )

print(coverage_values)

write_csv(
  coverage_values,
  here(
    "outputs", "tables",
    "onet_ssi_mapping_coverage.csv"
  )
)


# 7. IDENTIFY UNMATCHED OCCUPATIONS --------------------------

cat("\n[6/9] Identifying unmatched O*NET occupations...\n")

unmapped_ssi <- ssi %>%
  anti_join(
    valid_onet_isco_links %>%
      distinct(onet_soc_2019),
    by = c("onet_soc" = "onet_soc_2019")
  )

cat(
  "Unmapped SSI occupations:",
  nrow(unmapped_ssi), "\n"
)

write_csv(
  unmapped_ssi,
  here(
    "outputs", "tables",
    "onet_ssi_unmapped_occupations.csv"
  )
)


# 8. CREATE SSI-ISCO DATASET -------------------------------

cat("\n[7/9] Aggregating SSI by ISCO minor group...\n")

# Each O*NET-SOC 2019 occupation is counted once
# per ISCO minor group.
#
# An O*NET occupation may legitimately link to more than
# one ISCO minor group. We preserve these links and report
# the multiplicity instead of arbitrarily selecting one.
#
# Aggregation is an equal-weighted mean across the unique
# linked O*NET occupations. It is not employment weighted.

ssi_linked <- ssi %>%
  inner_join(
    valid_onet_isco_links,
    by = c("onet_soc" = "onet_soc_2019")
  ) %>%
  distinct(
    onet_soc,
    isco_minor_code,
    .keep_all = TRUE
  )

ssi_by_minor <- ssi_linked %>%
  group_by(isco_minor_code) %>%
  summarise(
    n_onet_occupations = n_distinct(onet_soc),
    
    SSI_raw_mean = mean(
      SSI_raw,
      na.rm = TRUE
    ),
    
    SSI_z_mean = mean(
      SSI_z,
      na.rm = TRUE
    ),
    
    SSI_raw_sd = sd(
      SSI_raw,
      na.rm = TRUE
    ),
    
    SSI_z_sd = sd(
      SSI_z,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  )


# Derive a standardised measure across mapped ISCO groups.
# The original O*NET z-score mean is retained separately.

ssi_by_minor <- ssi_by_minor %>%
  mutate(
    SSI_group_z = as.numeric(
      scale(SSI_raw_mean)
    )
  )


# Build a descriptive reference from unit-group labels.

isco_minor_descriptions <- isco_unit_reference %>%
  group_by(isco_minor_code) %>%
  summarise(
    n_isco_unit_groups = n_distinct(isco_unit_code),
    
    unit_group_descriptions = paste(
      sort(unique(
        unit_description[
          !is.na(unit_description) &
            unit_description != ""
        ]
      )),
      collapse = " | "
    ),
    
    .groups = "drop"
  )

ssi_by_minor <- ssi_by_minor %>%
  left_join(
    isco_minor_descriptions,
    by = "isco_minor_code"
  ) %>%
  arrange(isco_minor_code)


# Save the main processed dataset.

write_csv(
  ssi_by_minor,
  here(
    "data", "processed",
    "onet_ssi_isco_minor.csv"
  )
)


# 9. MAPPING MULTIPLICITY AND DIAGNOSTICS --------------------

cat("\n[8/9] Checking many-to-many mapping...\n")

onet_mapping_multiplicity <- valid_onet_isco_links %>%
  count(
    onet_soc_2019,
    name = "n_isco_minor_groups"
  )

ambiguous_onet_mappings <- onet_mapping_multiplicity %>%
  filter(n_isco_minor_groups > 1)

cat(
  "O*NET occupations mapped to multiple ISCO groups:",
  nrow(ambiguous_onet_mappings), "\n"
)

write_csv(
  onet_mapping_multiplicity,
  here(
    "outputs", "tables",
    "onet_mapping_multiplicity.csv"
  )
)

write_csv(
  ambiguous_onet_mappings,
  here(
    "outputs", "tables",
    "onet_isco_ambiguous_occupations.csv"
  )
)


# Identify SOC-ISCO unit codes absent from ISCO reference.

unverified_unit_links <- onet_soc_isco_all %>%
  filter(mapping_status == "ISCO unit not verified") %>%
  distinct(
    soc10,
    isco_unit_code
  )

write_csv(
  unverified_unit_links,
  here(
    "outputs", "tables",
    "onet_isco_unverified_unit_links.csv"
  )
)


# Full detailed mapping file.

write_csv(
  onet_soc_isco_all,
  here(
    "outputs", "tables",
    "onet_soc_isco_detailed_mapping.csv"
  )
)


# Full O*NET occupation to ISCO minor-group links.

write_csv(
  valid_onet_isco_links,
  here(
    "outputs", "tables",
    "onet2019_isco_minor_links.csv"
  )
)


# 10. FINAL QUALITY CHECKS ----------------------------------

cat("\n[9/9] Final quality checks...\n")

if (nrow(ssi_by_minor) == 0) {
  stop("No SSI-ISCO minor-group observations were generated.")
}

if (anyDuplicated(ssi_by_minor$isco_minor_code) > 0) {
  stop("Duplicate ISCO minor-group rows detected.")
}

if (anyNA(ssi_by_minor$SSI_raw_mean)) {
  stop("Missing aggregated SSI scores detected.")
}

if (any(!str_detect(
  ssi_by_minor$isco_minor_code,
  "^[0-9]{3}$"
))) {
  stop("Invalid ISCO minor-group code detected.")
}

cat("\n--- FINAL SSI-ISCO DATASET ---\n")
print(
  ssi_by_minor %>%
    select(
      isco_minor_code,
      n_onet_occupations,
      SSI_raw_mean,
      SSI_z_mean,
      SSI_group_z
    ),
  n = 30
)

cat("\n--- FINAL DATASET SUMMARY ---\n")

print(
  ssi_by_minor %>%
    summarise(
      isco_minor_groups = n(),
      minimum_occupations = min(n_onet_occupations),
      median_occupations = median(n_onet_occupations),
      maximum_occupations = max(n_onet_occupations),
      mean_SSI = mean(SSI_raw_mean),
      sd_SSI = sd(SSI_raw_mean)
    )
)

# Save detailed R object for later reproducibility.

saveRDS(
  ssi_by_minor,
  here(
    "data", "processed",
    "onet_ssi_isco_minor.rds"
  )
)

cat("\n============================================\n")
cat("O*NET SSI AGGREGATION COMPLETED\n")
cat("============================================\n")

cat(
  "\nFinal ISCO minor groups:",
  nrow(ssi_by_minor),
  "\n"
)

cat(
  "Unmapped O*NET occupations:",
  nrow(unmapped_ssi),
  "\n"
)

cat(
  "\nMain processed output:\n",
  "data/processed/onet_ssi_isco_minor.csv\n"
)

cat(
  "\nNext stage: construct the PLFS analysis dataset.\n"
)
