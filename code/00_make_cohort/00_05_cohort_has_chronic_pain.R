################################################################################
################################################################################
###  CREATE CHRONIC PAIN VARIABLES
###  Kat Hoffman, March 2023
###  Purpose: clean TAFOTH and TAFIPH files for chronic pain ICD codes
###  Output: cleaned data file containing minimum date the beneficiary ("data/final/chronic_pain.rds")
###        has a chronic pain ICD code in the study duration
###         and indicators of whether it occurs in washout or overall study duration
###
###  Modified by Anton: Simplified from 04_define_comorbidity_vars/define_chronic_pain.R
###                     to only check over the 6-month follow-up period for chronic pain.
###                     Previously, iterated over 0:17 to calculate 6-month rolling windows
###
################################################################################
################################################################################

# Set up -----------------------------------------------------------------------

# load libraries
library(arrow)
library(tidyverse)
library(lubridate)
library(data.table)
library(tictoc)
library(foreach)
# library(future)
# library(furrr)
# library(ggalluvial)
# library(doParallel)
# # options(cores=50)
# registerDoParallel()
# plan(multicore)
# getDoParWorkers()

save_dir <- "/mnt/general-data/disability/post_surgery_opioid_use/tmp"

for (my_year in 2016:2019) {
  
  dts_cohorts <- readRDS(file.path(save_dir, my_year, paste0("cohort_",my_year,"_full.rds")))
  
  chronic_pain_icds <- read.csv("/home/amh2389/medicaid/medicaid_treatment_trends/code/00_make_cohort/chronic_pain_icd10_20230216.csv") |>
    filter(CRITERIA == "Inclusion")
  
  ############################################################################
  ############################################################################
  # Step 1: read in all pain codes created in  define_pain.R script
  ############################################################################
  ############################################################################
  
  
  pain_all <- readRDS("/mnt/general-data/disability/create_cohort/intermediate/tmp/pain_all.rds")
  
  # add in pain categories
  pain_all_adj <- 
    pain_all |>
    left_join(chronic_pain_icds |> select(pain_cat = PAIN_CAT,
                                          dgcd = ICD9_OR_10)) |>
    select(BENE_ID, pain_cat, dgcd_dt) |>
    distinct()
  
  # pain_cat                                                n
  # <chr>                                               <int>
  # 1 Arthritis/Joint/Bone Pain (Other than Back/Neck) 42561532
  # 2 Back Pain                                        21991078
  # 3 Back/Neck Pain Unspecified                         146214
  # 4 Headache                                          3021709
  # 5 Misc Pain                                         8498759
  # 6 Neck Pain                                         8088842
  # 7 Neurologic Pain                                   7368100
  
  #### CREATE FILTER DATA FRAMES (too large to use with future)
  
  headaches_df <-
    pain_all_adj |>
    filter(pain_cat == "Headache") |>
    left_join(dts_cohorts |> select(BENE_ID, washout_start_dt, followup_start_dt)) 
  
  arthritis_df <-
    pain_all_adj |>
    filter(pain_cat == "Arthritis/Joint/Bone Pain (Other than Back/Neck)") |>
    left_join(dts_cohorts |> select(BENE_ID, washout_start_dt, followup_start_dt)) 
  
  back_df <-
    pain_all_adj |>
    filter(pain_cat == "Back Pain") |>
    left_join(dts_cohorts |> select(BENE_ID, washout_start_dt, followup_start_dt)) 
  
  backneck_unsp_df <-
    pain_all_adj |>
    filter(pain_cat == "Back/Neck Pain Unspecified") |>
    left_join(dts_cohorts |> select(BENE_ID, washout_start_dt, followup_start_dt)) 
  
  misc_df <-
    pain_all_adj |>
    filter(pain_cat == "Misc Pain") |>
    left_join(dts_cohorts |> select(BENE_ID, washout_start_dt, followup_start_dt)) 
  
  neck_df <-
    pain_all_adj |>
    filter(pain_cat == "Neck Pain") |>
    left_join(dts_cohorts |> select(BENE_ID, washout_start_dt, followup_start_dt)) 
  
  neuro_df <-
    pain_all_adj |>
    filter(pain_cat == "Neurologic Pain") |>
    left_join(dts_cohorts |> select(BENE_ID, washout_start_dt, followup_start_dt)) 
  
  
  
  ### FUNCTION TO MAP OVER PAIN CAT DFS
  
  rolling_windows <- function(pain_cat_df, pain_cat_name){
    print(paste(pain_cat_name, Sys.time()))
    
    relevant_pain_dts <-
      pain_cat_df |>
      # keep only codes within the 6 month window of interest
      mutate(start_month = washout_start_dt,
             end_month = followup_start_dt-1) |>
      # then, filter the diagnosis codes to only contain those filled within the relevant time frame
      filter(dgcd_dt %within% interval(start_month, end_month)) |>
      group_by(BENE_ID, pain_cat) |>
      add_count() |> # add number of dg codes within this window per beneficiary
      filter(n > 1)  |> # only keep codes that show up more than once 
      mutate(first_pain = min(dgcd_dt),
             pain_90 = first_pain + days(90)) |>
      filter(!(dgcd_dt %within% interval(first_pain, pain_90))) # filter out first pain and everything within 90 days
    
    chronic_pain_per_month <-
      relevant_pain_dts |>
      ungroup() |>
      select(BENE_ID) |>
      distinct() # |>
      # mutate(month = 7)
    
    chronic_pain_per_month[[pain_cat_name]] <- 1
    
    print(paste(pain_cat_name, "COMPLETE", Sys.time()))
    
    return(chronic_pain_per_month)
  }
  
  chronic_pain_headache <- rolling_windows(headaches_df, "headache")
  chronic_pain_arthritis <- rolling_windows(arthritis_df, "arthritis")
  chronic_pain_back <- rolling_windows(back_df, "back")
  chronic_pain_backneck_unsp <- rolling_windows(backneck_unsp_df, "backneck_unsp")
  chronic_pain_misc <- rolling_windows(misc_df, "misc")
  chronic_pain_neck_df <- rolling_windows(neck_df, "neck")
  chronic_pain_neuro <- rolling_windows(neuro_df, "neuro")

  
  overall_pain_list <- list(chronic_pain_headache, 
                      chronic_pain_arthritis,
                      chronic_pain_back,
                      chronic_pain_backneck_unsp,
                      chronic_pain_misc,
                      chronic_pain_neck_df,
                      chronic_pain_neuro)
  
  chronic_pain_df <- overall_pain_list |>
    reduce(full_join) |>
    mutate(across(where(is.numeric), ~replace_na(.x, 0))) |>
    mutate(chronic_pain_n = arthritis + back + backneck_unsp + headache + misc + neck + neuro,
           chronic_pain_any = 1) |> # only in this data set if they have chronic pain for that month
    select(BENE_ID, chronic_pain_n, chronic_pain_any)
  
  saveRDS(chronic_pain_df, file.path(save_dir, my_year, paste0(my_year,"_cohort_has_chronic_pain.rds")))
  
}

