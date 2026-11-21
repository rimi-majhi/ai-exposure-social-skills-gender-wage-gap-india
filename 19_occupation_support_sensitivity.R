
# ============================================================
# PROJECT: THE EMPATHY PREMIUM IN INDIA
# SCRIPT: 19_occupation_support_sensitivity.R
#
# Purpose:
# 1. Audit occupational gender representation.
# 2. Run Model 3 with minimum gender-cell thresholds of 5 and 10.
# 3. Test the female x SSI x AI interaction.
# 4. Compare sensitivity estimates with the original model.
#
# The original Model 3 is NOT re-estimated.
# The Bell-McCaffrey model is NOT re-estimated.
# ============================================================

# 0. PROJECT SETUP --------------------------------------------

project_root <- paste0(
  "C:/Users/Rimi Majhi/OneDrive/Desktop/",
  "ADED DSE/empathy-premium"
)

setwd(project_root)

library(survey)

options(survey.lonely.psu = "fail")

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

cat("\nPROJECT INITIALISED\n")


# 1. RESTORE SAVED OBJECTS ------------------------------------

design_model <- readRDS(
  "data/processed/plfs_survey_design_model.rds"
)

model_3 <- readRDS(
  "data/processed/model_3.rds"
)

stopifnot(inherits(design_model, "survey.design"))
stopifnot(inherits(model_3, "svyglm"))

d <- design_model$variables

cat("Full sample observations:", nrow(d), "\n")
cat(
  "Full sample occupations:",
  length(unique(d$nco_code)),
  "\n"
)


# 2. OCCUPATIONAL GENDER SUPPORT -------------------------------

occ_counts <- aggregate(
  cbind(
    women = as.integer(d$female == 1),
    men = as.integer(d$female == 0)
  ) ~ nco_code,
  data = d,
  FUN = sum
)

occ_counts$minimum_gender_count <- pmin(
  occ_counts$women,
  occ_counts$men
)

thresholds <- c(5, 10)

threshold_audit <- data.frame(
  threshold = thresholds,
  occupations_retained = vapply(
    thresholds,
    function(k) {
      sum(
        occ_counts$women >= k &
          occ_counts$men >= k
      )
    },
    numeric(1)
  )
)

threshold_audit$occupations_excluded <-
  nrow(occ_counts) -
  threshold_audit$occupations_retained

write.csv(
  occ_counts,
  "outputs/tables/occupation_gender_counts.csv",
  row.names = FALSE
)

write.csv(
  threshold_audit,
  "outputs/tables/occupation_threshold_audit.csv",
  row.names = FALSE
)

cat("\nOCCUPATIONAL SUPPORT AUDIT\n")
print(threshold_audit)


# 3. LOAD ORIGINAL MODEL COEFFICIENTS -------------------------

original_results <- read.csv(
  "outputs/tables/model_3_key_coefficients.csv"
)

original_results$model <- "Original full sample"
original_results$observations <- nrow(d)
original_results$occupations <- length(unique(d$nco_code))

original_results <- original_results[
  ,
  c(
    "model", "term", "estimate",
    "standard_error", "observations",
    "occupations"
  )
]


# 4. ORIGINAL MODEL FORMULA -----------------------------------

formula_m3 <- formula(model_3)

# These are the exact four coefficients of primary interest.
key_terms <- c(
  "female",
  "female:SSI",
  "female:AI",
  "female:SSI:AI"
)

sensitivity_tables <- list()
wald_outputs <- list()


# 5. FUNCTION TO ESTIMATE A SUPPORT-RESTRICTED MODEL ----------

run_support_model <- function(k) {
  
  cat("\n")
  cat("================================================\n")
  cat("STARTING SUPPORT THRESHOLD:", k, "\n")
  cat("================================================\n")
  
  eligible_codes <- occ_counts$nco_code[
    occ_counts$women >= k &
      occ_counts$men >= k
  ]
  
  cat(
    "Eligible occupations:",
    length(eligible_codes),
    "\n"
  )
  
  # Retain original survey-design structure.
  design_k <- subset(
    design_model,
    nco_code %in% eligible_codes
  )
  
  n_k <- nrow(design_k$variables)
  occ_k <- length(unique(design_k$variables$nco_code))
  
  cat("Observations retained:", n_k, "\n")
  cat("Occupations retained:", occ_k, "\n")
  cat("Design degrees of freedom:", survey::degf(design_k), "\n")
  
  # Estimate the same Model 3 specification.
  # Save a full model frame for reliable variance calculations.
  cat("\nEstimating Model 3...\n")
  
  fit <- survey::svyglm(
    formula_m3,
    design = design_k,
    family = gaussian(),
    control = stats::glm.control(maxit = 10),
    model = TRUE,
    x = FALSE,
    y = TRUE
  )
  
  # Save immediately before further calculations.
  model_file <- paste0(
    "data/processed/model_3_support_min",
    k,
    ".rds"
  )
  
  saveRDS(fit, model_file)
  
  cat("Model saved:", model_file, "\n")
  
  # Extract coefficients and covariance matrix.
  b <- coef(fit)
  V <- vcov(fit)
  se <- sqrt(diag(V))
  
  available <- intersect(key_terms, names(b))
  
  coef_table <- data.frame(
    model = paste0("Minimum ", k, " per gender"),
    term = available,
    estimate = as.numeric(b[available]),
    standard_error = as.numeric(se[available]),
    observations = n_k,
    occupations = occ_k,
    stringsAsFactors = FALSE
  )
  
  # Save coefficient table.
  coef_file <- paste0(
    "outputs/tables/model_3_support_min",
    k,
    "_coefficients.csv"
  )
  
  write.csv(
    coef_table,
    coef_file,
    row.names = FALSE
  )
  
  # Joint test of the three-way interaction.
  wald <- survey::regTermTest(
    fit,
    ~ female:SSI:AI
  )
  
  print(wald)
  
  wald_file <- paste0(
    "outputs/tables/model_3_support_min",
    k,
    "_wald.txt"
  )
  
  capture.output(
    print(wald),
    file = wald_file
  )
  
  sensitivity_tables[[as.character(k)]] <<- coef_table
  wald_outputs[[as.character(k)]] <<- wald
  
  cat("\nThreshold", k, "completed successfully.\n")
  
  rm(fit, design_k, b, V, se)
  gc()
  
  invisible(coef_table)
}


# 6. RUN BOTH SENSITIVITY MODELS ------------------------------

for (k in thresholds) {
  
  tryCatch(
    run_support_model(k),
    error = function(e) {
      cat(
        "\nERROR at threshold", k, ":",
        conditionMessage(e),
        "\n"
      )
      cat("The original model and previous saved results remain intact.\n")
    }
  )
}


# 7. CONSOLIDATE COMPLETED RESULTS -----------------------------

if (length(sensitivity_tables) > 0) {
  
  sensitivity_results <- do.call(
    rbind,
    sensitivity_tables
  )
  
  combined_results <- rbind(
    original_results,
    sensitivity_results
  )
  
  write.csv(
    combined_results,
    "outputs/tables/model_3_support_sensitivity_comparison.csv",
    row.names = FALSE
  )
  
  cat("\n")
  cat("================================================\n")
  cat("FINAL COEFFICIENT COMPARISON\n")
  cat("================================================\n")
  
  print(combined_results, digits = 6)
  
} else {
  
  cat("\nNo sensitivity model completed successfully.\n")
}


# 8. SAVE EXECUTION STATUS ------------------------------------

status <- data.frame(
  threshold = thresholds,
  completed = vapply(
    as.character(thresholds),
    function(k) {
      !is.null(sensitivity_tables[[k]])
    },
    logical(1)
  )
)

write.csv(
  status,
  "outputs/tables/model_3_support_execution_status.csv",
  row.names = FALSE
)

cat("\nEXECUTION STATUS\n")
print(status)


# 9. VERIFY IMPORTANT FILES -----------------------------------

important_files <- c(
  "data/processed/model_3.rds",
  "data/processed/model_3_bell_mccaffrey.rds",
  "outputs/tables/model_3_key_coefficients.csv",
  "outputs/tables/model_3_bell_mccaffrey.csv",
  "outputs/tables/model_3_support_sensitivity_comparison.csv",
  "outputs/tables/model_3_support_execution_status.csv"
)

cat("\nFILE VERIFICATION\n")

print(
  data.frame(
    file = important_files,
    exists = file.exists(important_files)
  )
)

cat("\nSCRIPT FINISHED.\n")
