# -------------------------------------
# Script: pain_or_disability
# Author: Anton Hung 2024-05-15
# Purpose:
# Notes:
# -------------------------------------

library(tidyverse)

save_dir <- "/mnt/general-data/disability/post_surgery_opioid_use/tmp"

for (my_year in 2016:2019) {
  
  dts_cohorts <- readRDS(file.path(save_dir, my_year, paste0("cohort_",my_year,"_full.rds")))
  
  chronic_pain_df <- readRDS(file.path(save_dir, my_year, paste0(my_year,"_cohort_has_chronic_pain.rds")))
  disability_df <- readRDS(file.path(save_dir, my_year, paste0(my_year,"_cohort_has_disability.rds")))
  
  dts_cohorts <- dts_cohorts |>
    left_join(chronic_pain_df[,c("BENE_ID", "chronic_pain_any")]) |>
    left_join(disability_df) |>
    mutate(disability_washout_cal = case_when(is.na(disability_washout_cal) ~ 0, 
                                              TRUE ~ disability_washout_cal),
           chronic_pain_any = case_when(is.na(chronic_pain_any) ~ 0, 
                                        TRUE ~ chronic_pain_any))
  
  cohort_disability_pain <- dts_cohorts |>
    mutate(
      disability_pain_cal = case_when(disability_washout_cal == 1 & chronic_pain_any == 1 ~ "disability and chronic pain",
                                      disability_washout_cal == 1 & chronic_pain_any == 0 ~ "disability only",
                                      disability_washout_cal == 0 & chronic_pain_any == 1 ~ "chronic pain only",
                                      disability_washout_cal == 0 & chronic_pain_any == 0 ~ "neither")) |>
    select(BENE_ID, disability_pain_cal)
  
  saveRDS(cohort_disability_pain, file.path(save_dir, my_year, paste0(my_year,"_cohort_disability_or_pain.rds")))
  
}
