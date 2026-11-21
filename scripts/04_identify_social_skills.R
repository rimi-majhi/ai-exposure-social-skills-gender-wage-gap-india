
library(tidyverse)
library(readxl)
library(here)

# Import relevant O*NET files
essential <- read_excel(
  here("data/raw/onet/Essential Skills.xlsx")
) %>%
  mutate(Source = "Essential Skills")

transferable <- read_excel(
  here("data/raw/onet/Transferable Skills.xlsx")
) %>%
  mutate(Source = "Transferable Skills")

abilities <- read_excel(
  here("data/raw/onet/Abilities.xlsx")
) %>%
  mutate(Source = "Abilities")

# Combine the three datasets
onet_skills <- bind_rows(
  essential,
  transferable,
  abilities
)

# Identify relevant social and communication skills
target_terms <- c(
  "Social Perceptiveness",
  "Active Listening",
  "Coordination",
  "Negotiation",
  "Persuasion",
  "Service Orientation",
  "Oral Expression",
  "Oral Comprehension",
  "Speaking",
  "Instructing"
)

# Find which variables actually exist
skill_catalogue <- onet_skills %>%
  filter(
    str_detect(
      str_to_lower(`Element Name`),
      str_to_lower(
        paste(target_terms, collapse = "|")
      )
    )
  ) %>%
  distinct(
    Source,
    `Element ID`,
    `Element Name`
  )

# Check occupation coverage and measurement scales
skill_coverage <- onet_skills %>%
  semi_join(
    skill_catalogue,
    by = c("Source", "Element ID", "Element Name")
  ) %>%
  group_by(
    Source,
    `Element Name`,
    `Scale ID`,
    `Scale Name`
  ) %>%
  summarise(
    Occupations = n_distinct(`O*NET-SOC Code`),
    Observations = n(),
    .groups = "drop"
  )
