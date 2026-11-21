
# ============================================================
# PROJECT: THE EMPATHY PREMIUM IN INDIA
# SCRIPT 21: FINAL MAIN REGRESSION TABLE
#
# Uses existing regression outputs only.
# No regression is estimated or rerun.
# ============================================================

project_root <- paste0(
  "C:/Users/Rimi Majhi/OneDrive/Desktop/",
  "ADED DSE/empathy-premium"
)

setwd(project_root)

# 1. Read saved main regression coefficients
results <- read.csv(
  "outputs/tables/main_regression_key_coefficients.csv",
  stringsAsFactors = FALSE
)

# 2. Define publication table terms
terms <- c(
  "female",
  "female:SSI",
  "female:AI",
  "SSI:AI",
  "female:SSI:AI"
)

labels <- c(
  "Female",
  "Female x Social Skills Index",
  "Female x GenAI Exposure",
  "Social Skills Index x GenAI Exposure",
  "Female x Social Skills Index x GenAI Exposure"
)

models <- c(
  "M1 Baseline",
  "M2 Three-way interaction",
  "M3 Occupation fixed effects"
)

# 3. Format coefficients and significance stars
format_cell <- function(x) {
  
  if (nrow(x) == 0) {
    return("--")
  }
  
  p <- x$p_value[1]
  
  stars <- if (p < 0.01) {
    "***"
  } else if (p < 0.05) {
    "**"
  } else if (p < 0.10) {
    "*"
  } else {
    ""
  }
  
  paste0(
    sprintf("%.4f", x$estimate[1]),
    stars,
    " (",
    sprintf("%.4f", x$std_error[1]),
    ")"
  )
}

# 4. Build wide regression table
table_2 <- data.frame(
  Variable = labels,
  check.names = FALSE
)

for (m in models) {
  
  values <- vapply(
    terms,
    function(term) {
      
      selected <- results[
        results$model == m &
          results$term == term,
        ,
        drop = FALSE
      ]
      
      format_cell(selected)
    },
    character(1)
  )
  
  table_2[[m]] <- values
}

# 5. Add specification information
specification <- data.frame(
  Specification = c(
    "Observations",
    "Education fixed effects",
    "Industry fixed effects",
    "State fixed effects",
    "Sector fixed effects",
    "Year fixed effects",
    "Occupation fixed effects",
    "Survey-weighted estimation"
  ),
  `M1 Baseline` = c(
    "87,457", "Yes", "Yes", "Yes",
    "Yes", "Yes", "Yes", "Yes"
  ),
  `M2 Three-way interaction` = c(
    "87,457", "Yes", "Yes", "Yes",
    "Yes", "Yes", "No", "Yes"
  ),
  `M3 Occupation fixed effects` = c(
    "87,457", "Yes", "Yes", "Yes",
    "Yes", "Yes", "Yes", "Yes"
  ),
  check.names = FALSE
)

# 6. Save tables
dir.create(
  "outputs/tables",
  recursive = TRUE,
  showWarnings = FALSE
)

write.csv(
  table_2,
  "outputs/tables/table_2_main_regression.csv",
  row.names = FALSE
)

write.csv(
  specification,
  "outputs/tables/table_2_model_specifications.csv",
  row.names = FALSE
)

# 7. Display results
cat("\nMAIN REGRESSION TABLE\n")
print(table_2, row.names = FALSE)

cat("\nMODEL SPECIFICATIONS\n")
print(specification, row.names = FALSE)

cat("\nOUTPUT VERIFICATION\n")

print(data.frame(
  file = c(
    "outputs/tables/table_2_main_regression.csv",
    "outputs/tables/table_2_model_specifications.csv"
  ),
  saved = file.exists(c(
    "outputs/tables/table_2_main_regression.csv",
    "outputs/tables/table_2_model_specifications.csv"
  ))
))

cat("\nSCRIPT 21 COMPLETED.\n")
