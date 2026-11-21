
# ============================================================
# PROJECT: THE EMPATHY PREMIUM IN INDIA
# SCRIPT 22: ROBUSTNESS AND SENSITIVITY TABLE
#
# Uses saved estimates only.
# Does not run any regressions.
# ============================================================

# 1. PROJECT SETUP

project_root <- paste0(
  "C:/Users/Rimi Majhi/OneDrive/Desktop/",
  "ADED DSE/empathy-premium"
)

setwd(project_root)

dir.create(
  "outputs/tables",
  recursive = TRUE,
  showWarnings = FALSE
)

# 2. CHECK REQUIRED FILES

required_files <- c(
  "outputs/tables/model_3_key_coefficients.csv",
  "outputs/tables/model_3_bell_mccaffrey.csv",
  "outputs/tables/model_3_support_min5_coefficients.csv",
  "outputs/tables/model_3_support_min10_coefficients.csv",
  "data/processed/model_3.rds",
  "data/processed/model_3_bell_mccaffrey.rds",
  "data/processed/model_3_support_min5.rds",
  "data/processed/model_3_support_min10.rds"
)

stopifnot(all(file.exists(required_files)))

# 3. READ SAVED COEFFICIENT TABLES

original <- read.csv(
  required_files[1]
)

bmc <- read.csv(
  required_files[2]
)

min5 <- read.csv(
  required_files[3]
)

min10 <- read.csv(
  required_files[4]
)

# 4. EXTRACT THREE-WAY INTERACTION

extract_interaction <- function(data, specification) {
  
  selected <- data[
    data$term == "female:SSI:AI",
    ,
    drop = FALSE
  ]
  
  stopifnot(nrow(selected) == 1)
  
  data.frame(
    specification = specification,
    estimate = selected$estimate,
    standard_error = selected$standard_error
  )
}

estimates <- rbind(
  extract_interaction(original, "Original Model 3"),
  extract_interaction(bmc, "Bell-McCaffrey-2"),
  extract_interaction(min5, "Minimum 5 per gender"),
  extract_interaction(min10, "Minimum 10 per gender")
)

# 5. ADD SAMPLE INFORMATION

sample_info <- data.frame(
  specification = c(
    "Original Model 3",
    "Bell-McCaffrey-2",
    "Minimum 5 per gender",
    "Minimum 10 per gender"
  ),
  
  observations = c(
    87457, 87457, 85892, 80561
  ),
  
  occupations = c(
    125, 125, 111, 97
  ),
  
  wald_F = c(
    43.66101,
    43.50328,
    43.86248,
    44.11757
  ),
  
  p_value = c(
    4.0062e-11,
    4.3416e-11,
    3.6154e-11,
    3.1764e-11
  )
)

# 6. COMBINE RESULTS

table_3 <- merge(
  sample_info,
  estimates,
  by = "specification",
  sort = FALSE
)

table_3$three_way_interaction <-
  sprintf(
    "%.5f",
    table_3$estimate
  )

table_3$standard_error_display <-
  sprintf(
    "%.5f",
    table_3$standard_error
  )

# 7. CREATE PUBLICATION TABLE

publication_table <- table_3[
  ,
  c(
    "specification",
    "observations",
    "occupations",
    "three_way_interaction",
    "standard_error_display",
    "wald_F",
    "p_value"
  )
]

names(publication_table) <- c(
  "Specification",
  "Observations",
  "Occupational groups",
  "Three-way coefficient",
  "Standard error",
  "Wald F-statistic",
  "Wald p-value"
)

cat("\nTABLE 3: ROBUSTNESS RESULTS\n")
print(publication_table, row.names = FALSE)

# 8. SAVE TABLES

write.csv(
  publication_table,
  "outputs/tables/table_3_robustness.csv",
  row.names = FALSE
)

write.csv(
  table_3,
  "outputs/tables/table_3_robustness_full.csv",
  row.names = FALSE
)

# 9. SAVE METHODOLOGICAL NOTES

notes <- c(
  "Table 3 reports the female x SSI x AI interaction.",
  "Original Model 3 includes occupation fixed effects.",
  "Bell-McCaffrey-2 changes the survey variance estimator.",
  "Minimum gender thresholds restrict occupational support.",
  "All estimates are observational associations.",
  "Bell-McCaffrey adjustment does not independently",
  "resolve dependence across occupational groups.",
  "Occupation-level inference requires separate assessment."
)

writeLines(
  notes,
  "outputs/tables/table_3_methodological_notes.txt"
)

# 10. VERIFY OUTPUTS

outputs <- c(
  "outputs/tables/table_3_robustness.csv",
  "outputs/tables/table_3_robustness_full.csv",
  "outputs/tables/table_3_methodological_notes.txt"
)

cat("\nOUTPUT VERIFICATION\n")

print(data.frame(
  file = outputs,
  saved = file.exists(outputs)
))

cat("\nTABLE 3 COMPLETED.\n")
