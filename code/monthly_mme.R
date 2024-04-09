# -------------------------------------
# Script: monthly mme
# Author: Anton Hung
# Purpose: Calculate monthly mme
# Notes:
# -------------------------------------
library(arrow)
library(tidyverse)
library(lubridate)
library(data.table)
library(furrr)
library(tidylog)
library(tictoc)
library(doFuture)

load_dir <- "/mnt/general-data/disability/create_cohort/"

###### COHORT
cohort <- readRDS(paste0(load_dir, "final/desc_cohort.rds"))
setDT(cohort)
cohort <- cohort[, c("BENE_ID",
                     "washout_start_dt",
                     "washout_cal_end_dt",
                     # "washout_12mos_end_dt",
                     # "study_cal_end_dt",
                     "dem_race",
                     "dem_race_cond",
                     "disability_pain_cal",
                     "opioid_pain_washout_cal"
)]




all_opioids_clean <- readRDS(paste0("/mnt/general-data/disability/mediation_unsafe_pain_mgmt/all_pain_opioids.rds"))

setDT(all_opioids_clean)
setkey(all_opioids_clean, BENE_ID)
all_opioids_clean <- all_opioids_clean[, .(BENE_ID, NDC, opioid, mme_strength_per_day, rx_start_dt, rx_end_dt)]

# Merge to the analysis cohort
all_opioids_clean_merged <- merge(all_opioids_clean, cohort, by = "BENE_ID")

# Filter opioid prescriptions to only contain those within mediator period
all_opioids_clean_mediator_period <- all_opioids_clean_merged[
  all_opioids_clean_merged$rx_start_dt %within% interval(
    all_opioids_clean_merged$washout_start_dt, all_opioids_clean_merged$washout_cal_end_dt
  ), 
]

# Calculate max daily dose -----------------------------------------------------

# Group by beneficiary and create a list column containing each beneficiairy's data
opioids <- all_opioids_clean_mediator_period[, list(data = list(data.table(.SD))), by = BENE_ID]

# Create function
calculate_max_daily_dose <- function(data) {
  to_modify <- copy(data)
  
  # Calculate the date limit based on washout_cal_end_dt + 182 days
  washout_date_limit <- to_modify$washout_cal_end_dt #+ lubridate::days(182)
  
  long <- to_modify[, .(date = seq(rx_start_dt, rx_end_dt, by = "1 day"), 
                        NDC, opioid, mme_strength_per_day), by = .(seq_len(nrow(data)))
  ][date <= washout_date_limit, ]  # Filter rows based on date limit
  
  long[, .(total_mme_strength = sum(mme_strength_per_day, na.rm = TRUE))
  ][, .(mediator_monthly_dose_mme = total_mme_strength/6)]
}

plan(multisession, workers = 50)

# Apply function
out <- foreach(data = opioids$data, 
               id = opioids$BENE_ID, 
               .combine = "rbind",
               .options.future = list(chunk.size = 1e4)) %dofuture% {
                 out <- calculate_max_daily_dose(data)
                 out$BENE_ID <- id
                 setcolorder(out, "BENE_ID")
                 out
               }

plan(sequential)

save_dir <- "/home/amh2389/medicaid"
saveRDS(out, file.path(save_dir, "mediator_monthly_dose_mme.rds"))