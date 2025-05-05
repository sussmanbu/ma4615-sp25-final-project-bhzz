## clean_data.R

# -------------------------------------------------------
# Cleaning script for Chicago Violence Victim Demographics
# -------------------------------------------------------

library(tidyverse)

# --- 1. Import Data ---
violence_raw <- read_csv(here::here("dataset", "Violence_Reduction_-_Victim_Demographics_-_Aggregated.csv"))

# --- 2. Extract Year ---
violence_raw <- violence_raw %>%
  mutate(Year = as.numeric(substr(TIME_PERIOD, 1, 4)))

# --- 3. Identify unknown values ---
# Columns to check for UNKNOWN
cols_check <- c("AGE", "SEX", "RACE", "JUVENILE_I", 
                "DOMESTIC_I", "GUNSHOT_INJURY_I", 
                "NUMBER_OF_VICTIMS", "PRIMARY_TYPE")

# Split data: rows with UNKNOWN vs clean rows
violence_na <- violence_raw %>%
  filter(if_any(all_of(cols_check), ~ .x == "UNKNOWN"))

violence_clean <- violence_raw %>%
  filter(if_all(all_of(cols_check), ~ .x != "UNKNOWN"))

# --- 4. Special handling for GUNSHOT_INJURY_I ---
violence_na <- violence_na %>%
  mutate(GUNSHOT_INJURY_I = ifelse(Year <= 2010 & GUNSHOT_INJURY_I == "UNKNOWN", 
                                   "Before2010", 
                                   GUNSHOT_INJURY_I))

# --- 5. Remove rows with >= 2 UNKNOWN values (excluding Before2010 cases) ---
violence_na <- violence_na %>%
  mutate(missing_count = rowSums(across(c("AGE", "SEX", "RACE", "GUNSHOT_INJURY_I"), ~ .x == "UNKNOWN"))) %>%
  filter(missing_count < 2) %>%
  select(-missing_count)

# --- 6. Combine cleaned subsets ---
violence_final <- bind_rows(violence_clean, violence_na)

# --- 7. Race recoding ---
violence_final <- violence_final %>%
  mutate(RACE = ifelse(RACE %in% c("WBH", "WWH"), "HIS", RACE))

# --- 8. Save cleaned data ---
write_rds(violence_final, file = here::here("dataset", "Violence_cleaned.rds"))