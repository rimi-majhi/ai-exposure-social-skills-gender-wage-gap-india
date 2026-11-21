
# ============================================================
# PROJECT: THE EMPATHY PREMIUM IN INDIA
# SCRIPT: 18_run_empirical_models.R
#
# PURPOSE:
# Estimate survey-weighted wage regressions.
#
# OUTCOME:
# Log real monthly earnings
#
# MAIN INTERACTION:
# Female x Social Skill Intensity x GenAI Exposure
#
# INFERENCE:
# PLFS stratification and PSU clustering
#
# PACKAGE:
# survey::svyglm()
# ============================================================


# 0. PACKAGES -----------------------------------------------

packages <- c(
  "survey",
  "dplyr",
  "readr",
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

library(survey)
library(dplyr)
library(readr)
library(here)
library(tibble)


# 1. LOAD VALIDATED SURVEY DESIGN ---------------------------

cat("\n============================================\n")
cat("EMPIRICAL REGRESSION PIPELINE\n")
cat("============================================\n")

design_file <- here(
  "data", "processed",
  "plfs_survey_design_model.rds"
)

if (!file.exists(design_file)) {
  stop("Validated model survey design not found.")
}

design_model <- readRDS(design_file)

cat(
  "Model observations:",
  nrow(design_model$variables),
  "\n"
)

cat(
  "Survey design degrees of freedom:",
  survey::degf(design_model),
  "\n"
)


# 2. CHECK REQUIRED VARIABLES -------------------------------

model_data <- design_model$variables

required_vars <- c(
  "ln_real_wage",
  "female",
  "SSI",
  "AI",
  "age",
  "education_code",
  "industry_code",
  "state",
  "sector",
  "year",
  "nco_code"
)

missing_vars <- setdiff(
  required_vars,
  names(model_data)
)

if (length(missing_vars) > 0) {
  stop(
    "Missing model variables: ",
    paste(missing_vars, collapse = ", ")
  )
}

if (
  anyNA(model_data[, required_vars])
) {
  stop("Missing values detected in model variables.")
}

if (
  any(!is.finite(model_data$ln_real_wage)) ||
  any(!is.finite(model_data$SSI)) ||
  any(!is.finite(model_data$AI))
) {
  stop("Infinite or non-finite model values detected.")
}

cat("\nModel variables validated.\n")


# 3. DEFINE CONTROL VARIABLES -------------------------------

# Demographic controls:
# age, age squared
#
# Human capital:
# education fixed effects
#
# Labour market:
# industry fixed effects, rural/urban sector
#
# Geography:
# state fixed effects
#
# Time:
# survey-year fixed effects

controls <- paste(
  "age",
  "I(age^2)",
  "factor(education_code)",
  "factor(industry_code)",
  "factor(state)",
  "factor(sector)",
  "factor(year)",
  sep = " + "
)


# 4. MODEL 1: BASELINE GENDER WAGE GAP ----------------------

cat("\n[1/5] Estimating baseline model...\n")

formula_m1 <- as.formula(
  paste(
    "ln_real_wage ~ female +",
    controls,
    "+ factor(nco_code)"
  )
)

model_1 <- survey::svyglm(
  formula_m1,
  design = design_model,
  family = gaussian()
)

cat("\n--- MODEL 1 RESULTS ---\n")
print(summary(model_1))


# 5. MODEL 2: THREE-WAY INTERACTION --------------------------

cat("\n[2/5] Estimating three-way interaction model...\n")

formula_m2 <- as.formula(
  paste(
    "ln_real_wage ~ female * SSI * AI +",
    controls
  )
)

model_2 <- survey::svyglm(
  formula_m2,
  design = design_model,
  family = gaussian()
)

cat("\n--- MODEL 2 RESULTS ---\n")
print(summary(model_2))


# 6. MODEL 3: OCCUPATION FIXED EFFECTS -----------------------

cat("\n[3/5] Estimating occupation fixed-effects model...\n")

# SSI, AI and SSI x AI are occupation-level variables.
# Their standalone effects are absorbed by occupation FE.
#
# The gender interactions remain identifiable.

formula_m3 <- as.formula(
  paste(
    "ln_real_wage ~ female +",
    "female:SSI +",
    "female:AI +",
    "female:SSI:AI +",
    controls,
    "+ factor(nco_code)"
  )
)

model_3 <- survey::svyglm(
  formula_m3,
  design = design_model,
  family = gaussian()
)

cat("\n--- MODEL 3 RESULTS ---\n")
print(summary(model_3))


# 7. EXTRACT COEFFICIENT TABLES -----------------------------

cat("\n[4/5] Extracting regression coefficients...\n")

extract_model <- function(model, model_name) {
  
  coefficient_matrix <- summary(model)$coefficients
  
  results <- as.data.frame(
    coefficient_matrix
  )
  
  results$term <- rownames(results)
  results$model <- model_name
  
  rownames(results) <- NULL
  
  names(results)[1:4] <- c(
    "estimate",
    "std_error",
    "statistic",
    "p_value"
  )
  
  df <- model$df.residual
  
  if (
    is.null(df) ||
    is.na(df) ||
    df <= 0
  ) {
    df <- Inf
  }
  
  critical_value <- qt(0.975, df = df)
  
  results %>%
    mutate(
      conf_low = estimate - critical_value * std_error,
      conf_high = estimate + critical_value * std_error
    ) %>%
    select(
      model,
      term,
      estimate,
      std_error,
      statistic,
      p_value,
      conf_low,
      conf_high
    )
}

model_results <- bind_rows(
  extract_model(model_1, "M1 Baseline"),
  extract_model(model_2, "M2 Three-way interaction"),
  extract_model(model_3, "M3 Occupation fixed effects")
)

write_csv(
  model_results,
  here(
    "outputs", "tables",
    "main_regression_results.csv"
  )
)


# 8. TEST THE MAIN THREE-WAY INTERACTION --------------------

cat("\n[5/5] Testing main interaction...\n")

triple_interaction <- model_results %>%
  filter(
    model %in% c(
      "M2 Three-way interaction",
      "M3 Occupation fixed effects"
    ),
    term == "female:SSI:AI"
  )

cat("\n--- MAIN THREE-WAY INTERACTION ---\n")
print(triple_interaction, width = Inf)

if (nrow(triple_interaction) == 0) {
  warning(
    "Interaction term not found. Inspect regression coefficient names."
  )
}

# Joint Wald test of the three-way interaction.
# Use tryCatch so an estimation or contrast issue is
# reported without preventing the remaining outputs.

wald_test <- tryCatch(
  survey::regTermTest(
    model_3,
    ~ female:SSI:AI
  ),
  error = function(e) {
    message("Wald test could not be calculated: ", e$message)
    NULL
  }
)

if (!is.null(wald_test)) {
  cat("\n--- WALD TEST: THREE-WAY INTERACTION ---\n")
  print(wald_test)
  
  capture.output(
    print(wald_test),
    file = here(
      "outputs", "tables",
      "three_way_interaction_wald_test.txt"
    )
  )
}


# 9. SAVE COMPLETE MODEL SUMMARIES --------------------------

capture.output(
  summary(model_1),
  file = here(
    "outputs", "tables",
    "model_1_baseline.txt"
  )
)

capture.output(
  summary(model_2),
  file = here(
    "outputs", "tables",
    "model_2_three_way.txt"
  )
)

capture.output(
  summary(model_3),
  file = here(
    "outputs", "tables",
    "model_3_occupation_fe.txt"
  )
)


# 10. SAVE ESTIMATED MODELS ---------------------------------

saveRDS(
  list(
    baseline = model_1,
    three_way = model_2,
    occupation_fe = model_3,
    wald_test = wald_test
  ),
  here(
    "data", "processed",
    "empirical_models.rds"
  )
)


# 11. SAMPLE AND MODEL COMPARISON ---------------------------

model_comparison <- tibble(
  model = c(
    "M1 Baseline",
    "M2 Three-way interaction",
    "M3 Occupation fixed effects"
  ),
  
  observations = c(
    nobs(model_1),
    nobs(model_2),
    nobs(model_3)
  ),
  
  design_degrees_freedom = c(
    survey::degf(design_model),
    survey::degf(design_model),
    survey::degf(design_model)
  )
)

cat("\n--- MODEL COMPARISON ---\n")
print(model_comparison)

write_csv(
  model_comparison,
  here(
    "outputs", "tables",
    "model_comparison.csv"
  )
)


# 12. FINAL STATUS ------------------------------------------

cat("\n============================================\n")
cat("REGRESSION PIPELINE COMPLETED\n")
cat("============================================\n")

cat(
  "\nFinal model observations:",
  nobs(model_3),
  "\n"
)

cat(
  "\nMain coefficient:",
  "female:SSI:AI",
  "\n"
)

cat(
  "\nResults saved to outputs/tables/.\n"
)

cat(
  "Model objects saved to data/processed/empirical_models.rds\n"
)
