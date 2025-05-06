# -------------------------------------
# Script: has_disability
# Author: Anton Hung
# Purpose:
# Notes:
# -------------------------------------

library(tidyverse)
library(lubridate)
library(data.table)
library(tictoc)

elig_nested <- readRDS("/mnt/general-data/disability/create_cohort/intermediate/tafdebse/elig_nested.rds")

dis_elig_cds <- c(
  "11", # = Individuals Receiving SSI
  "12", # = Aged, Blind and Disabled Individuals in 209(b) States
  "13", # = Individuals Receiving Mandatory State Supplements
  "17", # = Individuals Who Lost Eligibility for SSI/SSP Due to an Increase in OASDI Benefits in 1972
  "18", # = Individuals Who Would be Eligible for SSI/SSP but for OASDI COLA increases since April 1977
  "19", # = Disabled Widows and Widowers Ineligible for SSI due to Increase in OASDI
  "20", # = Disabled Widows and Widowers Ineligible for SSI due to Early Receipt of Social Security
  "21", # = Working Disabled under 1619(b)
  "22", # = Disabled Adult Children
  "37", # = Aged, Blind or Disabled Individuals Eligible for but Not Receiving Cash Assistance
  "38", #  = Individuals Eligible for Cash Assistance except for Institutionalization
  "39", # = Individuals Receiving Home and Community Based Services under Institutional Rules
  "40", # = Optional State Supplement Recipients — 1634 States, and SSI Criteria States with 1616 Agreements
  "41", # = Optional State Supplement Recipients — 209(b) States, and SSI Criteria States without 1616 Agreements
  "46", # = Poverty Level Aged or Disabled
  "47", # = Work Incentives Eligibility Group
  "48", # = Ticket to Work Basic Group
  "50", # = Family Opportunity Act Children with Disabilities,
  "51", # = Individuals Eligible for Home and Community-Based Services
  "52", # = Individuals Eligible for Home and Community-Based Services — Special Income Level
  "59", # = Medically Needy Aged, Blind or Disabled
  "60"  # = Medically Needy Blind or Disabled Individuals Eligible in 1973
)


save_dir <- "/mnt/general-data/disability/post_surgery_opioid_use/tmp"

for (my_year in 2016:2019) {

  dts_cohorts <- readRDS(file.path(save_dir, my_year, paste0("cohort_",my_year,"_full.rds")))
  setDT(dts_cohorts)
  setkey(dts_cohorts, BENE_ID)
  # dts_cohorts <- dts_cohorts[1:1000,]
  
  tic()
  
  elig_long <-
    elig_nested |>
    right_join(dts_cohorts) |>
    unnest(data) |>
    rename(year = RFRNC_YR) |>
    select(-ELGBLTY_GRP_CD_LTST) |>
    pivot_longer(cols = starts_with("ELGBLTY_GRP_CD"),
                 names_to = "month",
                 values_to = "elig_code",
                 values_drop_na = T) |>
    mutate(month = parse_number(month),
           year = as.numeric(year),
           elig_dt = as.Date(paste0(year, "-", month, "-01")))

  toc()
  
  setDT(elig_long)
  last_elig_cal <- elig_long[elig_dt %within% interval(washout_start_dt, followup_start_dt-1), ][order(elig_dt)] |>
    group_by(BENE_ID) |>
    slice_tail(n = 1) |>
    ungroup() |>
    select(BENE_ID, washout_cal_elig_code = elig_code)

  elig_clean <- last_elig_cal |>
    mutate(disability_washout_cal = case_when(washout_cal_elig_code %in% dis_elig_cds ~ 1,
                                              TRUE ~ 0)) |>
    filter(disability_washout_cal == 1) |>
    select(BENE_ID, disability_washout_cal)
  
  
  saveRDS(elig_clean, file.path(save_dir, my_year, paste0(my_year,"_cohort_has_disability.rds")))
  
}

