
# EMPATHY PREMIUM RESEARCH
# Script 02: Inspect O*NET and crosswalk files

library(tidyverse)
library(readxl)
library(here)

# ------------------------------------------------
# 1. Locate O*NET Excel files
# ------------------------------------------------

onet_dir <- here("data", "raw", "onet")

onet_files <- list.files(
  path = onet_dir,
  pattern = "\\.xlsx$",
  full.names = TRUE,
  ignore.case = TRUE
)

# ------------------------------------------------
# 2. Identify worksheet names
# ------------------------------------------------

sheet_inventory <- map_dfr(onet_files, function(file) {
  
  tryCatch({
    
    sheets <- excel_sheets(file)
    
    tibble(
      file_name = basename(file),
      sheet_name = sheets
    )
    
  }, error = function(e) {
    
    tibble(
      file_name = basename(file),
      sheet_name = paste("ERROR:", conditionMessage(e))
    )
    
  })
})

print(sheet_inventory, n = Inf)

# ------------------------------------------------
# 3. Identify column names in each workbook
# ------------------------------------------------

column_inventory <- map_dfr(onet_files, function(file) {
  
  tryCatch({
    
    first_sheet <- excel_sheets(file)[1]
    
    data <- read_excel(
      file,
      sheet = first_sheet,
      n_max = 5
    )
    
    tibble(
      file_name = basename(file),
      sheet_name = first_sheet,
      columns = paste(names(data), collapse = " | ")
    )
    
  }, error = function(e) {
    
    tibble(
      file_name = basename(file),
      sheet_name = NA_character_,
      columns = paste("ERROR:", conditionMessage(e))
    )
    
  })
})

print(column_inventory, n = Inf)

# ------------------------------------------------
# 4. Inspect the O*NET database ZIP
# ------------------------------------------------

onet_zip <- file.path(
  onet_dir,
  "db_31_0_excel.zip"
)

if (file.exists(onet_zip)) {
  
  zip_contents <- unzip(
    onet_zip,
    list = TRUE
  )
  
  print(head(zip_contents, 30))
  
}

# ------------------------------------------------
# 5. Save inspection results
# ------------------------------------------------

write_csv(
  sheet_inventory,
  here("outputs", "onet_sheet_inventory.csv")
)

write_csv(
  column_inventory,
  here("outputs", "onet_column_inventory.csv")
)

message("O*NET inspection completed.")
# View O*NET worksheet inventory
sheet_inventory <- read_csv(
  here("outputs", "onet_sheet_inventory.csv"),
  show_col_types = FALSE
)

print(sheet_inventory, n = Inf)
column_inventory <- read_csv(
  here("outputs", "onet_column_inventory.csv"),
  show_col_types = FALSE
)

print(column_inventory, n = Inf, width = Inf)
