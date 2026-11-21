
# EMPATHY PREMIUM RESEARCH
# Script 03: Inspect actual O*NET variables

library(tidyverse)
library(readxl)
library(here)

# Files that we need to investigate
files_to_check <- c(
  "Essential Skills.xlsx",
  "Transferable Skills.xlsx",
  "Work Activities.xlsx",
  "Abilities.xlsx"
)

onet_dir <- here("data", "raw", "onet")

# Inspect each workbook
for (file_name in files_to_check) {
  
  file_path <- file.path(onet_dir, file_name)
  
  cat("\n\n====================================\n")
  cat("FILE:", file_name, "\n")
  cat("====================================\n")
  
  if (!file.exists(file_path)) {
    cat("File not found.\n")
    next
  }
  
  sheets <- excel_sheets(file_path)
  
  cat("\nWorksheets:", paste(sheets, collapse = ", "), "\n")
  
  for (sheet in sheets) {
    
    data <- read_excel(
      file_path,
      sheet = sheet,
      n_max = 8
    )
    
    cat("\nWORKSHEET:", sheet, "\n")
    
    cat("\nCOLUMN NAMES:\n")
    print(names(data))
    
    cat("\nFIRST 8 ROWS:\n")
    print(data, n = 8, width = Inf)
  }
}

# Inspect relevant files inside the O*NET database ZIP
zip_path <- file.path(onet_dir, "db_31_0_excel.zip")

zip_contents <- unzip(zip_path, list = TRUE)

cat("\n\n====================================\n")
cat("RELEVANT O*NET DATABASE FILES\n")
cat("====================================\n")

relevant_files <- zip_contents %>%
  filter(
    str_detect(
      Name,
      regex("skills|work activities|occupation data|abilities",
            ignore_case = TRUE)
    )
  ) %>%
  select(Name, Length)

print(relevant_files, n = Inf, width = Inf)
