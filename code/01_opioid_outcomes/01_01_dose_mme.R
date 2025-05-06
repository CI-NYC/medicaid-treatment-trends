# READ ME -----------------------------------------------------------------
#
#       Author: Kat Hoffman
# Last updated: 26 January 2024 (Nick)
# 
# -------------------------------------------------------------------------

library(tidyverse)
library(janitor)
library(readxl)
library(arrow)
library(lubridate)
library(data.table)

save_dir <- "/mnt/general-data/disability/post_surgery_opioid_use/tmp"

source("~/medicaid/medicaid-treatment-trends/R/helpers.R")

cohort <- rbind(readRDS(file.path(save_dir, "2016", paste0("cohort_","2016","_pain_only.rds"))),
                readRDS(file.path(save_dir, "2017", paste0("cohort_","2017","_pain_only.rds"))),
                readRDS(file.path(save_dir, "2018", paste0("cohort_","2018","_pain_only.rds"))),
                readRDS(file.path(save_dir, "2019", paste0("cohort_","2019","_pain_only.rds"))))

# Read in RXL (pharmacy line)
rxl <- open_rxl()

# Read in IPL (Inpatient line)
ipl <- open_ipl()

#  Read in OTL (Other services line) 
otl <- open_otl()

excluded_cpt_cds <- c(36400:36415, "G0001", 43200:43272, 45300:45387, 70010:79999, 80048:89399, 93000:93278)

mme <- readRDS(file.path("/mnt/general-data/disability/mediation_unsafe_pain_mgmt", "opioids_mme.rds"))

rxl_opioids <-
  rxl |>
  filter(NDC %in% mme$NDC,
         BENE_ID %in% cohort$BENE_ID) |>
  select(BENE_ID, CLM_ID, RX_FILL_DT, contains("ndc"), DAYS_SUPPLY) |>
  collect() |>
  left_join(mme)

rxl_opioids <- rxl_opioids |>
  mutate(setting = "other_outpatient")

# saveRDS(rxl_opioids, file.path(save_dir, "opioid_data", "rxl_opioids_pain_mme.rds"))

ipl_opioids <- 
  ipl |>
  filter(NDC %in% mme$NDC,
         BENE_ID %in% cohort$BENE_ID) |>
  mutate(LINE_SRVC_BGN_DT = ifelse(is.na(LINE_SRVC_BGN_DT), LINE_SRVC_END_DT, LINE_SRVC_BGN_DT)) |>
  select(BENE_ID, CLM_ID, LINE_SRVC_BGN_DT, contains("NDC")) |>
  collect() |>
  left_join(mme)

ipl_opioids <- ipl_opioids |>
  mutate(setting = "inpatient_acute")

otl_opioids <- 
  otl |>
  filter(NDC %in% mme$NDC,
         BENE_ID %in% cohort$BENE_ID) |>
  mutate(LINE_SRVC_BGN_DT = ifelse(is.na(LINE_SRVC_BGN_DT), LINE_SRVC_END_DT, LINE_SRVC_BGN_DT)) |>
  select(BENE_ID, CLM_ID, LINE_SRVC_BGN_DT, contains("NDC"), LINE_PRCDR_CD, REV_CNTR_CD) |>
  collect() |>
  left_join(mme)

acute_CLM_ID <- otl_opioids |>
  filter((grepl("^045[0-9]$|^0981$", REV_CNTR_CD) |
           grepl("^9928[1-5]|99288",LINE_PRCDR_CD)) &
  !LINE_PRCDR_CD %in% excluded_cpt_cds) |>
  pull(CLM_ID)
  
otl_opioids <- otl_opioids |>
  mutate(setting = ifelse(CLM_ID %in% acute_CLM_ID, "other_acute", "other_outpatient")) |>
  select(-c("LINE_PRCDR_CD", "REV_CNTR_CD"))

otl_opioids <- rbind(ipl_opioids, otl_opioids)

# saveRDS(otl_opioids, file.path(save_dir, "opioid_data", "otl_opioids_pain_mme.rds"))


# MME conversion ############################################################

# calculate strength per day in Milligram Morphine Equivalent (MME) units
# no caps on number of pills, days supply, and pills per day
rxl_opioids <-
  rxl_opioids |>
  drop_na(BENE_ID) |>
  mutate(number_pills = case_when(!is.na(NDC_QTY) ~ abs(NDC_QTY),
                                  TRUE ~ 1),
         days_supply = case_when(!is.na(DAYS_SUPPLY) ~ DAYS_SUPPLY,
                                 TRUE ~ 1), # best assumption we can make if missing a days supply var
         pills_per_day = number_pills / days_supply,
         strength = parse_number(numeratorValue),
         strength_per_day = strength * pills_per_day,
         mme_strength_per_day = strength_per_day * conversion, 
         mme_strength_per_day = pmin(mme_strength_per_day, quantile(mme_strength_per_day, 0.99)))

# keep only relevant vars for RXL opioids
rxl_opioids <-
  rxl_opioids |>
  select(BENE_ID,
         setting,
         opioid,
         NDC,
         dose_form,
         days_supply,
         pills_per_day,
         strength,
         strength_per_day,
         mme_strength_per_day,
         days_supply,
         rx_start_dt = RX_FILL_DT) |>
  mutate(rx_end_dt = as.Date(rx_start_dt + (days_supply - 1))) |>
  arrange(BENE_ID, rx_start_dt, opioid)

# filter to opioids for pain, calculate strength per day in Milligram Morphine Equivalent (MME) units
otl_opioids <-
  otl_opioids |>
  drop_na(BENE_ID) |>
  mutate(strength = parse_number(numeratorValue),
         # we assume all OTL opioids are one day supply (outpatient)
         mme_strength_per_day = strength * conversion) 

# summary(otl_opioids$mme_strength_per_day)

# keep only relevant vars for OTL opioids
otl_opioids <-
  otl_opioids |>
  select(BENE_ID,
         setting,
         NDC,
         dose_form,
         opioid,
         strength,
         mme_strength_per_day,
         rx_start_dt = LINE_SRVC_BGN_DT) |>
  mutate(rx_end_dt = as.Date(rx_start_dt)) |> # 1 day supply assumption
  arrange(BENE_ID, rx_start_dt, opioid)


all_opioids_clean <- full_join(otl_opioids, rxl_opioids)

saveRDS(all_opioids_clean, file.path(save_dir, "opioid_data/all_pain_opioids.rds"))


