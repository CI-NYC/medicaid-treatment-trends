################################################################################
################################################################################
###  CREATE CHRONIC PAIN VARIABLES
###  Kat Hoffman, March 2023
###  Purpose: clean TAFOTH and TAFIPH files for chronic pain ICD codes
###  Output: cleaned data file containing minimum date the beneficiary ("data/final/chronic_pain.rds")
###        has a chronic pain ICD code in the study duration
###         and indicators of whether it occurs in washout or overall study duration
################################################################################
################################################################################

# Set up -----------------------------------------------------------------------

# load libraries
library(arrow)
library(tidyverse)
library(lubridate)
library(data.table)


cohort_2016 <- readRDS("/mnt/general-data/disability/post_surgery_opioid_use/tmp/final_cohorts/cohort_2016.rds")

############################################################################
############################################################################
# Step 4: add indicators for when the minimum date of pain occurred
############################################################################
############################################################################

pain_all <- readRDS("/mnt/general-data/disability/create_cohort/intermediate/tmp/pain_all.rds")

# get minimum pain date after washout period starts
pain_min <- pain_all |>
  filter(BENE_ID %in% cohort_2016$BENE_ID) |>
  left_join(cohort_2016) |>
  filter(dgcd_dt %within% interval(washout_start_dt %m+% days(182), washout_start_dt %m+% days(364))) |>
  group_by(BENE_ID) |>
  arrange(dgcd_dt) |> 
  filter(row_number() == 1) |>
  select(BENE_ID, pain_dt = dgcd_dt)

saveRDS(pain_min, "data/tmp/pain_min.rds")

# pain_min <- read_rds("data/tmp/pain_min.rds")

tic()
all_pain_clean <- 
  dts_cohorts |>
  select(BENE_ID, washout_start_dt, washout_cal_end_dt, washout_12mos_end_dt, washout_cont_end_dt,
         study_cal_end_dt, study_cont_end_dt
  ) |>
  left_join(pain_min) |>
  mutate(pain_washout_cal = case_when(pain_dt %within% interval(washout_start_dt, washout_cal_end_dt) ~ 1,
                                      TRUE ~ 0),
         pain_washout_12mos_cal = case_when(pain_dt %within% interval(washout_start_dt, washout_12mos_end_dt) ~ 1,
                                            TRUE ~ 0),
         pain_washout_cont = case_when(pain_dt %within% interval(washout_start_dt, washout_cont_end_dt) ~ 1,
                                       TRUE ~ 0),
         pain_study_cal = case_when(pain_dt %within% interval(washout_start_dt, study_cal_end_dt) ~ 1,
                                    TRUE ~ 0),
         pain_study_cont = case_when(pain_dt %within% interval(washout_start_dt, study_cont_end_dt) ~ 1,
                                     TRUE ~ 0)) |>
  select(BENE_ID, pain_dt, 
         pain_washout_cal, pain_washout_12mos_cal,
         pain_washout_cont,
         pain_study_cal, pain_study_cont)
toc()

write_rds(all_pain_clean, "data/final/pain.rds") # save final data file