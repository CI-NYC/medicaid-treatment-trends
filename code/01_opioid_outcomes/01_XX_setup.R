# -------------------------------------
# Script: 01_00_setup
# Author: Anton Hung
# Purpose:
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

cohort <- readRDS(paste0(load_dir, "final/desc_cohort.rds"))
setDT(cohort)

cohort <- cohort[, c("BENE_ID",
                     "washout_start_dt",
                     "washout_cal_end_dt",
                     "washout_12mos_end_dt",
                     "dem_race_cond",
                     "disability_pain_12mos_cal",
                     "opioid_pain_washout_12mos_cal"
)]



