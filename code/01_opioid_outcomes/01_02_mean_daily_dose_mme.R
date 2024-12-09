# READ ME -----------------------------------------------------------------
#
#       Author: Sarah Forrest
#      Created: 24 Aug 2023
#      edited: 27 Jan 2024 (Nick)
# last edited: 09 May 2024 (Anton)
# 
#        Notes: Modified code originally written by Kat Hoffman and Nick
#               Williams in the 0x_mediator_dose_mme.R script
#        
#       Output: A mediator dataset with the the maximum daily dose (MME) of 
#           opioids prescribed to a beneficiary within the mediator period, 
#           accounting for multiple prescriptions on the same day, for all 
#           beneficiaries in the analysis cohort
#
# May 9th edit: Load in 4 cohorts, one for each of the years 2016-2019,
#           and compute mme for each cohort during the defined followup period
#
# -------------------------------------------------------------------------

library(tidyverse)
library(janitor)
library(readxl)
library(arrow)
library(lubridate)
library(data.table)
library(tictoc)
library(foreach)
library(doFuture)
library(furrr)

save_dir <- "/mnt/general-data/disability/post_surgery_opioid_use/tmp"

# # Read in cohort and dates
# dts_cohorts <- readRDS("/mnt/general-data/disability/create_cohort/final/desc_cohort.rds")
# setDT(dts_cohorts)
# dts_cohorts <- dts_cohorts[opioid_pain_washout_12mos_cal == 1, .(BENE_ID, washout_cal_end_dt)]
# setkey(dts_cohorts, BENE_ID)

all_opioids_clean <- readRDS("/mnt/general-data/disability/mediation_unsafe_pain_mgmt/all_pain_opioids.rds")
setDT(all_opioids_clean)
setkey(all_opioids_clean, BENE_ID)
all_opioids_clean <- all_opioids_clean[, .(BENE_ID, NDC, opioid, mme_strength_per_day, rx_start_dt, rx_end_dt)]


# repeat daily mean mme calculations for each year
for (my_year in 2016:2019) {

  dts_cohorts <- readRDS(file.path(save_dir, my_year, paste0("cohort_",my_year,"_full.rds")))
  # dts_cohorts <- dts_cohorts[1:100,]
  setDT(dts_cohorts)
  setkey(dts_cohorts, BENE_ID)

  
  # Merge to the analysis cohort
  all_opioids_clean_merged <- merge(all_opioids_clean, dts_cohorts, by = "BENE_ID")
  
  # Filter opioid prescriptions to only contain those within mediator period
  all_opioids_clean_mediator_period <- all_opioids_clean_merged[
    all_opioids_clean_merged$rx_start_dt %within% interval(
      all_opioids_clean_merged$followup_start_dt, all_opioids_clean_merged$followup_end_dt
    ), 
  ]
  
  # Calculate max daily dose -----------------------------------------------------
  
  # Group by beneficiary and create a list column containing each beneficiairy's data
  opioids <- all_opioids_clean_mediator_period[, list(data = list(data.table(.SD))), by = BENE_ID]
  
  # Create function
  calculate_mean_daily_dose <- function(data) {
    to_modify <- copy(data)
    
    # to_modify[, c("rx_start_dt", "rx_end_dt") := lapply(.SD, as.Date), 
    #           .SDcols = c("rx_start_dt", "rx_end_dt")]
    
    # Calculate the date limit based on washout_cal_end_dt + 182 days
    washout_date_limit <- to_modify$followup_end_dt
    
    long <- to_modify[, .(date = seq(rx_start_dt, rx_end_dt -1, by = "1 day"), 
                          NDC, opioid, mme_strength_per_day), by = .(seq_len(nrow(data)))
    ][date <= washout_date_limit, ]  # Filter rows based on date limit
    
    long[, .(total_mme_strength = sum(mme_strength_per_day, na.rm = TRUE)), by = .(date)
    ][, .(mediator_mean_daily_dose_mme = mean(total_mme_strength))]
  }
  
  tic()
  
  plan(multisession, workers = 8)
  
  # Apply function
  out <- foreach(data = opioids$data, 
                 id = opioids$BENE_ID, 
                 .combine = "rbind",
                 .options.future = list(chunk.size = 1e4)) %dofuture% {
                   out <- calculate_mean_daily_dose(data)
                   out$BENE_ID <- id
                   setcolorder(out, "BENE_ID")
                   out
                 }
  
  plan(sequential)
  out <- out |>
    mutate(mediator_mean_daily_dose_mme = pmin(mediator_mean_daily_dose_mme, quantile(mediator_mean_daily_dose_mme,0.99)))
  toc()
  
  # Right join with cohort
  mean_daily_dose <- merge(out, dts_cohorts[, .(BENE_ID)], all.y = TRUE, by = "BENE_ID")
  
  # Convert NAs to 0 for observations in the cohort that didn't have a claim
  mean_daily_dose[, mediator_mean_daily_dose_mme := fifelse(is.na(mediator_mean_daily_dose_mme), 0, mediator_mean_daily_dose_mme)]
  
  # Save final dataset -----------------------------------------------------------
  
  saveRDS(mean_daily_dose, file.path(save_dir, my_year, paste0(my_year, "_mean_daily_dose_mme.rds")))

}
