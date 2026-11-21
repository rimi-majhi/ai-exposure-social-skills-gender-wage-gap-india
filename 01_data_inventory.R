
# EMPATHY PREMIUM RESEARCH
# Script 01: Raw Data Inventory

library(tidyverse)
library(here)

# 1. Locate the raw data folder
raw_dir <- here("data", "raw")

# 2. Check that the folder exists
if (!dir.exists(raw_dir)) {
  stop("Cannot find data/raw. Check your RStudio project directory.")
}

# 3. Identify all files in the raw data folders
raw_files <- list.files(
  path = raw_dir,
  recursive = TRUE,
  full.names = TRUE,
  include.dirs = FALSE
)

# 4. Stop if no files are found
if (length(raw_files) == 0) {
  stop("No files found inside data/raw.")
}

# 5. Create an inventory
inventory <- tibble(
  file_name = basename(raw_files),
  folder = basename(dirname(raw_files)),
  extension = tolower(tools::file_ext(raw_files)),
  size_mb = round(
    file.info(raw_files)$size / (1024^2), 2
  ),
  full_path = normalizePath(
    raw_files,
    winslash = "/",
    mustWork = FALSE
  )
)

# 6. Display all files
print(inventory, n = Inf)

# 7. Save the inventory
dir.create(
  here("outputs"),
  showWarnings = FALSE,
  recursive = TRUE
)

write_csv(
  inventory,
  here("outputs", "raw_file_inventory.csv")
)

message("Data inventory successfully created.")
