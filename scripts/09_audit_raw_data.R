
# ============================================================
# EMPATHY PREMIUM RESEARCH
# SCRIPT 09: RAW DATA AUDIT
# ============================================================

library(tidyverse)
library(readxl)
library(haven)
library(here)

# 1. PROJECT DIRECTORIES

project_root <- here::here()

plfs_dir <- here("data", "raw", "plfs")
cpi_dir  <- here("data", "raw", "cpi")
onet_dir <- here("data", "raw", "onet")

cat("\nPROJECT:", project_root, "\n")

# 2. INVENTORY ALL DOWNLOADED FILES

cat("\n========== PLFS FILES ==========\n")

plfs_files <- list.files(
  plfs_dir,
  recursive = TRUE,
  full.names = TRUE
)

print(basename(plfs_files))

cat("\n========== CPI FILES ==========\n")

cpi_files <- list.files(
  cpi_dir,
  recursive = TRUE,
  full.names = TRUE
)

print(basename(cpi_files))

cat("\n========== ONET FILES ==========\n")

onet_files <- list.files(
  onet_dir,
  recursive = TRUE,
  full.names = TRUE
)

print(basename(onet_files))


# 3. INSPECT PLFS ZIP FILES

cat("\n========== PLFS ZIP CONTENTS ==========\n")

plfs_zips <- plfs_files[
  grepl("\\.zip$", plfs_files, ignore.case = TRUE)
]

for (z in plfs_zips) {
  
  cat("\nZIP FILE:", basename(z), "\n")
  
  contents <- unzip(z, list = TRUE)
  
  print(
    contents %>%
      select(Name, Length) %>%
      head(30)
  )
}


# 4. INSPECT PLFS CSV FILES

cat("\n========== PLFS CSV HEADERS ==========\n")

plfs_csvs <- plfs_files[
  grepl("\\.csv$", plfs_files, ignore.case = TRUE)
]

for (f in plfs_csvs) {
  
  cat("\nFILE:", basename(f), "\n")
  
  print(
    names(
      read_csv(
        f,
        n_max = 0,
        show_col_types = FALSE
      )
    )
  )
}


# 5. INSPECT CPI EXCEL WORKBOOKS

cat("\n========== CPI WORKBOOKS ==========\n")

cpi_excels <- cpi_files[
  grepl("\\.xlsx?$", cpi_files, ignore.case = TRUE)
]

for (f in cpi_excels) {
  
  cat("\nFILE:", basename(f), "\n")
  
  sheets <- excel_sheets(f)
  
  print(sheets)
  
  for (s in sheets) {
    
    cat("\nSHEET:", s, "\n")
    
    preview <- read_excel(
      f,
      sheet = s,
      n_max = 6
    )
    
    print(names(preview))
    print(preview)
  }
}


# 6. INSPECT EXISTING SSI DATASET

cat("\n========== SSI DATASET ==========\n")

ssi_path <- here(
  "data", "processed",
  "onet_social_skills_index.csv"
)

if (file.exists(ssi_path)) {
  
  ssi <- read_csv(
    ssi_path,
    show_col_types = FALSE
  )
  
  cat("\nDIMENSIONS:\n")
  print(dim(ssi))
  
  cat("\nCOLUMN NAMES:\n")
  print(names(ssi))
  
  cat("\nFIRST 5 RECORDS:\n")
  print(head(ssi, 5), width = Inf)
  
} else {
  
  cat("SSI CSV NOT FOUND\n")
  
}


# 7. INSPECT OTHER PLFS DATA FORMATS

cat("\n========== OTHER PLFS FORMATS ==========\n")

print(
  plfs_files[
    grepl(
      "\\.(dta|xlsx|xls|txt|sav)$",
      plfs_files,
      ignore.case = TRUE
    )
  ]
)


# 8. SAVE FILE INVENTORY

inventory <- tibble(
  file = c(plfs_files, cpi_files, onet_files),
  category = c(
    rep("PLFS", length(plfs_files)),
    rep("CPI", length(cpi_files)),
    rep("ONET", length(onet_files))
  )
) %>%
  mutate(
    file_size_bytes = file.info(file)$size
  )

dir.create(
  here("outputs", "tables"),
  recursive = TRUE,
  showWarnings = FALSE
)

write_csv(
  inventory,
  here(
    "outputs", "tables",
    "raw_data_inventory.csv"
  )
)

message("\nRAW DATA AUDIT COMPLETED.")
