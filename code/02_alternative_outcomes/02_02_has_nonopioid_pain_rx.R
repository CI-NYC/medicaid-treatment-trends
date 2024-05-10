# -------------------------------------
# Script: nonopioid_pain_rx
# Author: Anton Hung
# Purpose: redoing nonopioid_pain_rx for the larger time interval (washout_start_dt to washout_12mos_end_dt)
# Notes:
# -------------------------------------


library(arrow)
library(dplyr)
library(lubridate)
library(data.table)

# Directories
src_root <- "/mnt/processed-data/disability"
drv_root <- "/home/amh2389/medicaid/plotting_trends/output"

save_dir <- "/mnt/general-data/disability/post_surgery_opioid_use/tmp"


for (my_year in 2016:2019) {
  # Read in OTL (Other services line) 
  files <- paste0(list.files(src_root, pattern = "TAFOTL", recursive = TRUE))
  parquet_files <- grep("\\.parquet$", files, value = TRUE)
  otl <- open_dataset(file.path(src_root, parquet_files))
  
  
  # Read in RXL (pharmacy line)
  files <- paste0(list.files(src_root, pattern = "TAFRXL", recursive = TRUE))
  parquet_files <- grep("\\.parquet$", files, value = TRUE)
  rxl <- open_dataset(file.path(src_root, parquet_files))
  

  cohort <- readRDS(file.path(save_dir, my_year, paste0("cohort_",my_year,"_full.rds")))
  setDT(cohort)
  setkey(cohort, BENE_ID)
  
  # Read in non opioid pain list
  nop <- readRDS("/mnt/general-data/disability/mediation_unsafe_pain_mgmt/mediation_unsafe_pain_mgmt_nonopioid_pain_ndc.rds")
  
  # OTL ---------------------------------------------------------------------
  
  # Filter OTL to non-opioid pain NDC
  otl_vars <- c("BENE_ID", "LINE_SRVC_BGN_DT", "LINE_SRVC_END_DT", "NDC", "NDC_QTY")
  
  otl <- select(otl, all_of(otl_vars)) |> 
    filter(NDC %in% nop$NDC) |>
    collect() |> 
    as.data.table()
  
  otl[, LINE_SRVC_BGN_DT := fifelse(is.na(LINE_SRVC_BGN_DT), 
                                    LINE_SRVC_END_DT, 
                                    LINE_SRVC_BGN_DT)]
  
  # Inner join with cohort 
  otl <- unique(merge(otl, cohort, by = "BENE_ID"))
  otl <- merge(otl, nop[, c(1, 3)], all.x = TRUE, by = "NDC")
  
  # Filter to claims within mediator time-frame
  otl <- otl[LINE_SRVC_BGN_DT %within% interval(followup_start_dt, 
                                                followup_end_dt), 
             .(BENE_ID, LINE_SRVC_BGN_DT, followup_start_dt, followup_end_dt, NDC, NDC_QTY, flag_nop)]
  
  
  # RXL ---------------------------------------------------------------------
  
  rxl_vars <- c("BENE_ID", "RX_FILL_DT", "NDC", "NDC_QTY", "DAYS_SUPPLY")
  
  rxl <- select(rxl, all_of(rxl_vars)) |> 
    filter(NDC %in% nop$NDC) |>
    collect() |> 
    as.data.table()
  
  # Inner join with cohort 
  rxl <- unique(merge(rxl, cohort, by = "BENE_ID"))
  rxl <- merge(rxl, nop[, c(1, 3)], all.x = TRUE, by = "NDC")
  
  # Filter to claims within mediator time-frame
  rxl <- rxl[RX_FILL_DT %within% interval(followup_start_dt, 
                                          followup_end_dt), 
             .(BENE_ID, RX_FILL_DT, followup_start_dt, followup_end_dt, NDC, NDC_QTY, DAYS_SUPPLY, flag_nop)]
  
  # Combine both datasets and keep only unique rows
  nop_pain <- rbind(otl[, .(BENE_ID, NDC, NDC_QTY, flag_nop)], 
                    rxl[, .(BENE_ID, NDC, NDC_QTY, flag_nop)])
  
  nop_pain <- unique(nop_pain)
  
  nop_pain <- merge(cohort, nop_pain, all.x = TRUE)
  
  nop_pain[, has_nonopioid_pain_rx := as.numeric(any(!is.na(NDC))), by = BENE_ID]
  nop_pain <- unique(nop_pain[, .(BENE_ID, has_nonopioid_pain_rx)])
  
  # cohort <- merge(nop_pain, cohort, all.y = TRUE, by = "BENE_ID")

  saveRDS(nop_pain, file.path(save_dir, my_year, paste0(my_year,"_cohort_has_non_opioid_rx.rds")))
  
}


