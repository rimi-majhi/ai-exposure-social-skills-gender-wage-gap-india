
# ============================================================
# PROJECT: THE EMPATHY PREMIUM IN INDIA
# SCRIPT 23: OCCUPATION-LEVEL INFERENCE ASSESSMENT
#
# PURPOSE:
# 1. Restore the validated PLFS survey design.
# 2. Recover the original Model 3.
# 3. Construct survey weights, occupation and PSU clusters.
# 4. Estimate an auxiliary weighted Model 3 using fixest.
# 5. Calculate two-way clustered standard errors.
# 6. Compare with the original svyglm estimates.
# 7. Save all outputs.
#
# IMPORTANT:
# This is an auxiliary weighted WLS sensitivity analysis.
# It does not replace the primary PLFS survey estimator.
# It does not reproduce the complete stratified Taylor
# linearisation variance estimator.
#
# Original Models 1-3 and previous sensitivity models
# are NOT re-estimated by this script.
# ============================================================


# 1. PROJECT SETUP --------------------------------------------

project_root <- paste0(
  "C:/Users/Rimi Majhi/OneDrive/Desktop/",
  "ADED DSE/empathy-premium"
)

setwd(project_root)

library(survey)
library(fixest)

cat("\nPROJECT INITIALISED\n")

cat(
  "Survey version:",
  as.character(packageVersion("survey")),
  "\n"
)

cat(
  "Fixest version:",
  as.character(packageVersion("fixest")),
  "\n"
)

dir.create(
  "outputs/tables",
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  "data/processed",
  recursive = TRUE,
  showWarnings = FALSE
)


# 2. RESTORE ORIGINAL OBJECTS -------------------------------

design_path <- "data/processed/plfs_survey_design_model.rds"

model_path <- "data/processed/model_3.rds"

stopifnot(file.exists(design_path))
stopifnot(file.exists(model_path))

design_model <- readRDS(design_path)

model_3 <- readRDS(model_path)

stopifnot(inherits(design_model, "survey.design"))
stopifnot(inherits(model_3, "svyglm"))

d <- design_model$variables

cat("\nMODEL SAMPLE\n")

cat("Observations:", nrow(d), "\n")

cat(
  "Occupational groups:",
  length(unique(d$nco_code)),
  "\n"
)


# 3. EXTRACT PSU AND STRATUM IDENTIFIERS ---------------------

cluster_mat <- as.matrix(
  design_model$cluster
)

strata_mat <- as.matrix(
  design_model$strata
)

cat("\nSURVEY DESIGN STRUCTURE\n")

cat(
  "Cluster columns:",
  ncol(cluster_mat),
  "\n"
)

cat(
  "Strata columns:",
  ncol(strata_mat),
  "\n"
)

# Confirm the expected one-stage survey design.

if (
  ncol(cluster_mat) != 1L ||
  ncol(strata_mat) != 1L
) {
  stop(
    "Unexpected survey design structure. ",
    "Inspect the cluster and strata identifiers."
  )
}

psu_raw <- as.character(
  cluster_mat[, 1]
)

stratum_raw <- as.character(
  strata_mat[, 1]
)

# Make PSU identifiers unique across strata.

psu_id <- interaction(
  stratum_raw,
  psu_raw,
  drop = TRUE,
  lex.order = TRUE
)

# Recover the probability weights from the survey design.

survey_weights <- as.numeric(
  stats::weights(design_model)
)

stopifnot(
  length(survey_weights) == nrow(d)
)

stopifnot(
  all(is.finite(survey_weights))
)

stopifnot(
  all(survey_weights > 0)
)

cat("\nCLUSTER COUNTS\n")

cat(
  "Occupations:",
  length(unique(d$nco_code)),
  "\n"
)

cat(
  "PSUs:",
  length(unique(psu_id)),
  "\n"
)

cat(
  "Strata:",
  length(unique(stratum_raw)),
  "\n"
)

cat("\nWEIGHT SUMMARY\n")

print(summary(survey_weights))


# 4. CONSTRUCT AUXILIARY MODEL DATA ---------------------------

m3_data <- data.frame(
  
  ln_real_wage = d$ln_real_wage,
  
  female = as.numeric(d$female),
  
  SSI = as.numeric(d$SSI),
  
  AI = as.numeric(d$AI),
  
  age = as.numeric(d$age),
  
  education_code = factor(
    d$education_code
  ),
  
  industry_code = factor(
    d$industry_code
  ),
  
  state = factor(
    d$state
  ),
  
  sector = factor(
    d$sector
  ),
  
  year = factor(
    d$year
  ),
  
  nco_code = factor(
    d$nco_code
  ),
  
  occupation_cluster = factor(
    d$nco_code
  ),
  
  psu_cluster = factor(
    psu_id
  ),
  
  survey_weight = survey_weights
  
)

# Normalise weights by their mean.
# Multiplying all weights by a constant does not change
# weighted least-squares point estimates.

m3_data$survey_weight_norm <-
  m3_data$survey_weight /
  mean(m3_data$survey_weight)

# Check completeness.

if (!all(complete.cases(m3_data))) {
  
  stop(
    "Missing observations found in auxiliary data. ",
    "Do not silently alter the original model sample."
  )
  
}

stopifnot(
  nrow(m3_data) == nrow(d)
)

cat("\nAUXILIARY DATA VALIDATED\n")

cat(
  "Observations:",
  nrow(m3_data),
  "\n"
)

cat(
  "Occupation clusters:",
  nlevels(m3_data$occupation_cluster),
  "\n"
)

cat(
  "PSU clusters:",
  nlevels(m3_data$psu_cluster),
  "\n"
)


# 5. DEFINE ORIGINAL MODEL 3 STRUCTURE -----------------------

# The occupation fixed effects are absorbed.
# The SSI and AI main effects are also absorbed by occupation FE.

formula_cluster <-
  ln_real_wage ~
  female +
  female:SSI +
  female:AI +
  female:SSI:AI +
  age +
  I(age^2) +
  fixest::i(education_code) +
  fixest::i(industry_code) +
  fixest::i(state) +
  fixest::i(sector) +
  fixest::i(year) |
  nco_code


# 6. ESTIMATE AUXILIARY TWO-WAY CLUSTERED WLS ---------------

cat("\n")
cat("====================================================\n")
cat("STARTING AUXILIARY TWO-WAY CLUSTERED ESTIMATION\n")
cat("====================================================\n")

start_time <- Sys.time()

model_3_cluster <- fixest::feols(
  
  # Correct argument name for fixest 0.14.2
  fml = formula_cluster,
  
  data = m3_data,
  
  weights = ~survey_weight_norm,
  
  # Two clustering dimensions:
  # 1. Occupational group.
  # 2. PLFS primary sampling unit.
  
  vcov = ~occupation_cluster + psu_cluster,
  
  # Small-sample cluster corrections.
  
  ssc = fixest::ssc(
    K.adj = TRUE,
    G.adj = TRUE,
    G.df = "min",
    t.df = "min"
  ),
  
  nthreads = 2,
  
  mem.clean = TRUE,
  
  data.save = TRUE
  
)

cat("\nAUXILIARY ESTIMATION COMPLETED\n")

print(
  Sys.time() - start_time
)


# 7. CHECK ESTIMATION SAMPLE ---------------------------------

estimated_n <- stats::nobs(
  model_3_cluster
)

cat(
  "\nAuxiliary model observations:",
  estimated_n,
  "\n"
)

cat(
  "Original model observations:",
  nrow(d),
  "\n"
)

if (estimated_n != nrow(d)) {
  
  warning(
    "The auxiliary estimation sample differs ",
    "from the original Model 3 sample."
  )
  
}


# 8. SAVE MODEL IMMEDIATELY ----------------------------------

cluster_model_path <-
  "data/processed/model_3_two_way_cluster_aux.rds"

saveRDS(
  model_3_cluster,
  cluster_model_path
)

cat(
  "\nAuxiliary model saved:",
  file.exists(cluster_model_path),
  "\n"
)


# 9. EXTRACT TWO-WAY CLUSTERED RESULTS -----------------------

cluster_summary <- summary(
  model_3_cluster
)

ct <- cluster_summary$coeftable

cat("\nAVAILABLE COEFFICIENT NAMES\n")

print(
  rownames(ct)
)

key_terms <- c(
  "female",
  "female:SSI",
  "female:AI",
  "female:SSI:AI"
)

available <- intersect(
  key_terms,
  rownames(ct)
)

if (!all(key_terms %in% available)) {
  
  stop(
    "Some interaction coefficients are missing. ",
    "Inspect the coefficient names printed above."
  )
  
}

# Extract estimates, standard errors, t statistics
# and p-values from the fixest clustered covariance.

cluster_results <- data.frame(
  
  term = available,
  
  estimate = ct[available, 1],
  
  standard_error = ct[available, 2],
  
  statistic = ct[available, 3],
  
  p_value = ct[available, 4],
  
  row.names = NULL
  
)

cat("\n")
cat("====================================================\n")
cat("TWO-WAY CLUSTERED RESULTS\n")
cat("====================================================\n")

print(
  cluster_results,
  digits = 7
)


# 10. CALCULATE CLUSTERED CONFIDENCE INTERVALS -----------

G_occ <- nlevels(
  m3_data$occupation_cluster
)

G_psu <- nlevels(
  m3_data$psu_cluster
)

cluster_df <- min(
  G_occ,
  G_psu
) - 1

cluster_results$inference_df <- cluster_df

critical_value <- qt(
  0.975,
  df = cluster_df
)

cluster_results$lower_95 <-
  cluster_results$estimate -
  critical_value *
  cluster_results$standard_error

cluster_results$upper_95 <-
  cluster_results$estimate +
  critical_value *
  cluster_results$standard_error


# 11. COMPARE WITH PRIMARY SURVEY MODEL ----------------------

original_beta <- coef(model_3)

auxiliary_beta <- coef(model_3_cluster)

comparison <- data.frame(
  
  term = key_terms,
  
  svyglm_estimate = as.numeric(
    original_beta[key_terms]
  ),
  
  two_way_cluster_estimate = as.numeric(
    auxiliary_beta[key_terms]
  ),
  
  svyglm_standard_error = sqrt(
    diag(vcov(model_3))[key_terms]
  ),
  
  two_way_cluster_standard_error =
    as.numeric(
      cluster_results$standard_error[
        match(
          key_terms,
          cluster_results$term
        )
      ]
    )
  
)

comparison$coefficient_difference <-
  comparison$two_way_cluster_estimate -
  comparison$svyglm_estimate

cat("\n")
cat("====================================================\n")
cat("POINT ESTIMATE AND STANDARD ERROR COMPARISON\n")
cat("====================================================\n")

print(
  comparison,
  digits = 7
)


# 12. THREE-WAY INTERACTION FOCUS -----------------------------

triple_result <- cluster_results[
  cluster_results$term == "female:SSI:AI",
  ,
  drop = FALSE
]

cat("\n")
cat("====================================================\n")
cat("THREE-WAY INTERACTION: INFERENCE CHECK\n")
cat("====================================================\n")

cat(
  "Coefficient:",
  triple_result$estimate,
  "\n"
)

cat(
  "Two-way clustered SE:",
  triple_result$standard_error,
  "\n"
)

cat(
  "Clustered t statistic:",
  triple_result$statistic,
  "\n"
)

cat(
  "Clustered p-value:",
  triple_result$p_value,
  "\n"
)

cat(
  "Inference degrees of freedom:",
  cluster_df,
  "\n"
)


# 13. SAVE RESULTS --------------------------------------------

write.csv(
  cluster_results,
  "outputs/tables/model_3_two_way_cluster_results.csv",
  row.names = FALSE
)

write.csv(
  comparison,
  "outputs/tables/model_3_inference_comparison.csv",
  row.names = FALSE
)

capture.output(
  cluster_summary,
  file = "outputs/tables/model_3_two_way_cluster_summary.txt"
)

writeLines(
  
  c(
    "AUXILIARY OCCUPATION-LEVEL INFERENCE ASSESSMENT",
    "",
    "Estimator: Weighted least squares using PLFS weights.",
    "Software: fixest.",
    "Variance estimator: Two-way clustered covariance.",
    "Cluster dimensions: occupation and PLFS PSU.",
    "Small-sample adjustment: fixest ssc.",
    "",
    "This auxiliary analysis does not reproduce the",
    "complete PLFS stratified Taylor-linearised variance",
    "estimator or finite population corrections.",
    "",
    "The svyglm estimates remain the primary survey-design",
    "estimates. This model is a supplementary sensitivity",
    "analysis for dependence across shared occupations."
  ),
  
  "outputs/tables/model_3_two_way_cluster_method_notes.txt"
  
)


# 14. VERIFY SAVED FILES -------------------------------------

output_files <- c(
  
  "data/processed/model_3_two_way_cluster_aux.rds",
  
  "outputs/tables/model_3_two_way_cluster_results.csv",
  
  "outputs/tables/model_3_inference_comparison.csv",
  
  "outputs/tables/model_3_two_way_cluster_summary.txt",
  
  "outputs/tables/model_3_two_way_cluster_method_notes.txt"
  
)

cat("\n")
cat("====================================================\n")
cat("OUTPUT VERIFICATION\n")
cat("====================================================\n")

print(
  data.frame(
    file = output_files,
    saved = file.exists(output_files)
  )
)

cat("\nSCRIPT 23 COMPLETED.\n")

gc()
