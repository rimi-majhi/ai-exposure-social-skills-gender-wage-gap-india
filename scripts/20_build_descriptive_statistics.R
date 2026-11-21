
# ============================================================
# PROJECT: THE EMPATHY PREMIUM IN INDIA
# SCRIPT 20: DESCRIPTIVE STATISTICS
#
# Purpose:
# 1. Calculate weighted descriptive statistics by gender/year.
# 2. Calculate unweighted sample counts.
# 3. Produce publication-ready Table 1.
#
# No regression models are estimated.
# ============================================================


# 1. PROJECT SETUP --------------------------------------------

project_root <- paste0(
  "C:/Users/Rimi Majhi/OneDrive/Desktop/",
  "ADED DSE/empathy-premium"
)

setwd(project_root)

library(survey)

dir.create(
  "outputs/tables",
  recursive = TRUE,
  showWarnings = FALSE
)

cat("\nPROJECT INITIALISED\n")


# 2. LOAD VALIDATED SURVEY DESIGN -----------------------------

design_model <- readRDS(
  "data/processed/plfs_survey_design_model.rds"
)

d <- design_model$variables

required <- c(
  "female",
  "year",
  "ln_real_wage",
  "age",
  "SSI",
  "AI"
)

stopifnot(all(required %in% names(d)))

cat("Observations:", nrow(d), "\n")
cat("Occupations:", length(unique(d$nco_code)), "\n")


# 3. CREATE GENDER AND YEAR GROUPS ----------------------------

design_desc <- stats::update(
  design_model,
  gender_label = factor(
    ifelse(female == 1, "Women", "Men"),
    levels = c("Men", "Women")
  ),
  year_label = factor(as.character(year))
)

cat("\nSurvey design prepared.\n")


# 4. CALCULATE WEIGHTED DESCRIPTIVE STATISTICS ---------------

descriptive_results <- survey::svyby(
  ~ ln_real_wage + age + SSI + AI,
  ~ gender_label + year_label,
  design = design_desc,
  FUN = survey::svymean,
  na.rm = TRUE,
  vartype = "se",
  keep.names = TRUE
)

descriptive_results <- as.data.frame(
  descriptive_results
)

# Standardise grouping labels for merging
descriptive_results$gender_label <- as.character(
  descriptive_results$gender_label
)

descriptive_results$year_label <- as.character(
  descriptive_results$year_label
)

cat("\nWEIGHTED DESCRIPTIVE STATISTICS\n")
print(descriptive_results)


# 5. CALCULATE UNWEIGHTED SAMPLE COUNTS -----------------------

raw_counts <- data.frame(
  gender_label = ifelse(
    d$female == 1,
    "Women",
    "Men"
  ),
  year_label = as.character(d$year)
)

sample_counts <- aggregate(
  rep(1L, nrow(raw_counts)),
  by = list(
    gender_label = raw_counts$gender_label,
    year_label = raw_counts$year_label
  ),
  FUN = sum
)

names(sample_counts)[3] <- "unweighted_n"

cat("\nUNWEIGHTED SAMPLE COUNTS\n")
print(sample_counts)


# 6. COMBINE TABLES -------------------------------------------

table_1 <- merge(
  descriptive_results,
  sample_counts,
  by = c("gender_label", "year_label"),
  all.x = TRUE,
  sort = TRUE
)

cat("\nCOMPLETE TABLE 1\n")
print(table_1)


# 7. SAVE OUTPUT FILES ----------------------------------------

write.csv(
  descriptive_results,
  "outputs/tables/table_1_weighted_descriptives.csv",
  row.names = FALSE
)

write.csv(
  sample_counts,
  "outputs/tables/table_1_sample_counts.csv",
  row.names = FALSE
)

write.csv(
  table_1,
  "outputs/tables/table_1_complete.csv",
  row.names = FALSE
)


# 8. VERIFY OUTPUTS -------------------------------------------

files <- c(
  "outputs/tables/table_1_weighted_descriptives.csv",
  "outputs/tables/table_1_sample_counts.csv",
  "outputs/tables/table_1_complete.csv"
)

cat("\nOUTPUT VERIFICATION\n")

print(
  data.frame(
    file = files,
    saved = file.exists(files)
  )
)

cat("\nSCRIPT 20 COMPLETED SUCCESSFULLY.\n")

gc()
