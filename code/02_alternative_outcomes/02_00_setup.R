# -------------------------------------
# Script: setup
# Author: Anton Hung
# Purpose: This code initializes all the beginning variables that remain constant for all remaining scripts

# Notes: 
# -------------------------------------

# Libraries
library(arrow)
library(dplyr)
library(lubridate)
library(data.table)
library(yaml)

# Directories
src_root <- "/mnt/processed-data/disability"
drv_root <- "/home/amh2389/medicaid/plotting_trends/output"

save_dir <- "/mnt/general-data/disability/post_surgery_opioid_use/tmp"

# Read in OTL (Other services line) 
files <- paste0(list.files(src_root, pattern = "TAFOTL", recursive = TRUE))
parquet_files <- grep("\\.parquet$", files, value = TRUE)
otl <- open_dataset(file.path(src_root, parquet_files))


# Read in RXL (pharmacy line)
files <- paste0(list.files(src_root, pattern = "TAFRXL", recursive = TRUE))
parquet_files <- grep("\\.parquet$", files, value = TRUE)
rxl <- open_dataset(file.path(src_root, parquet_files))


# Read in cohort and dates
# cohort <- readRDS("/mnt/general-data/disability/create_cohort/final/desc_cohort.rds")
# setDT(cohort)
# setkey(cohort, BENE_ID)
# cohort <- cohort[, .(BENE_ID, washout_start_dt, washout_cal_end_dt, washout_12mos_end_dt, dem_race_cond, disability_pain_12mos_cal)]
# setkey(cohort, BENE_ID)


# codebook
codebook <- read_yaml("/home/amh2389/medicaid/medicaid_treatment_trends/code/02_alternative_outcomes/mediator_codes.yml")
claims_vars <- c("BENE_ID", "LINE_SRVC_BGN_DT", "LINE_SRVC_END_DT", "LINE_PRCDR_CD_SYS", "LINE_PRCDR_CD")



# function definition to remove repetition
# this function retrieves all the beneficiaries who have TRUE indicator for whether they had a claim for a certain treatment during the 12-month period

my_func <- function(codes){
  # Filter OTL to claims codes
  claims <- select(otl, all_of(claims_vars)) |> 
    filter(LINE_PRCDR_CD %in% codes) |>
    collect()
  
  setDT(claims)
  setkey(claims, BENE_ID)
  
  claims[, LINE_SRVC_BGN_DT := fifelse(is.na(LINE_SRVC_BGN_DT), 
                                       LINE_SRVC_END_DT, 
                                       LINE_SRVC_BGN_DT)]
  # Inner join with cohort 
  claims <- unique(merge(claims, cohort, by = "BENE_ID"))
  
  # Filter to claims within mediator time-frame
  claims <- claims[LINE_SRVC_BGN_DT %within% interval(followup_start_dt, 
                                                      followup_end_dt)]#, 
                   # .(BENE_ID, LINE_SRVC_BGN_DT, washout_start_dt, washout_12mos_end_dt, LINE_PRCDR_CD)]
  
  # Create indicator variable for whether or not a patient had claim in mediator period
  # Right join with cohort
  claims <- claims[, new_column := as.numeric(.N > 0), by = "BENE_ID"] |>
    select(c("BENE_ID", "new_column"))
  claims
}
