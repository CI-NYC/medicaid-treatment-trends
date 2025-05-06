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
save_dir <- "/mnt/general-data/disability/post_surgery_opioid_use/tmp"

source("~/medicaid/medicaid-treatment-trends/R/helpers.R")


for (my_year in 2016:2019) {
  # Read in OTL (Other services line) 
  otl <- open_otl()
  
  # Read in IPL (Inpatient line) 
  ipl <- open_ipl()
  
  # Read in RXL (pharmacy line)
  rxl <- open_rxl()
  

  cohort <- readRDS(file.path(save_dir, my_year, paste0("cohort_",my_year,"_pain_only.rds")))
  setDT(cohort)
  setkey(cohort, BENE_ID)
  
  # Read in non opioid pain list
  nop <- readRDS("/mnt/general-data/disability/mediation_unsafe_pain_mgmt/mediation_unsafe_pain_mgmt_nonopioid_pain_ndc.rds")
  
  # IPL  ---------------------------------------------------------------------
  ipl_vars <- c("BENE_ID", "LINE_SRVC_BGN_DT", "LINE_SRVC_END_DT", "NDC", "NDC_QTY")
  
  ipl <- select(ipl, all_of(ipl_vars)) |> 
    filter(NDC %in% nop$NDC) |>
    collect() |> 
    as.data.table()
  
  ipl[, LINE_SRVC_BGN_DT := fifelse(is.na(LINE_SRVC_BGN_DT), 
                                    LINE_SRVC_END_DT, 
                                    LINE_SRVC_BGN_DT)]
  
  # Inner join with cohort 
  ipl <- unique(merge(ipl, cohort, by = "BENE_ID"))
  ipl <- merge(ipl, nop[, c(1, 3)], all.x = TRUE, by = "NDC")
  
  # Filter to claims within mediator time-frame
  ipl <- ipl[LINE_SRVC_BGN_DT %within% interval(followup_start_dt, 
                                                followup_end_dt), 
             .(BENE_ID, LINE_SRVC_BGN_DT, followup_start_dt, followup_end_dt, NDC, NDC_QTY, flag_nop)]
  
  # OTL ---------------------------------------------------------------------
  
  excluded_cpt_cds <- c(36400:36415, "G0001", 43200:43272, 45300:45387, 70010:79999, 80048:89399, 93000:93278)
  
  # Filter OTL to non-opioid pain NDC
  otl_vars <- c("BENE_ID", "CLM_ID", "LINE_SRVC_BGN_DT", "LINE_SRVC_END_DT", "NDC", "NDC_QTY", "LINE_PRCDR_CD", "REV_CNTR_CD")
  
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
             .(BENE_ID, CLM_ID, LINE_PRCDR_CD, LINE_SRVC_BGN_DT, followup_start_dt, followup_end_dt, NDC, NDC_QTY, REV_CNTR_CD, flag_nop)]
  
  acute_CLM_ID <- otl |>
    filter((grepl("^045[0-9]$|^0981$", REV_CNTR_CD) |
                grepl("^9928[1-5]|99288",LINE_PRCDR_CD)) &
               !LINE_PRCDR_CD %in% excluded_cpt_cds) |>
    pull(CLM_ID)
  
  otl_acute <- otl |>
    filter(CLM_ID %in% acute_CLM_ID)
  
  otl_opatient <- otl |>
    filter(!CLM_ID %in% acute_CLM_ID)
  
  
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
  # nop_pain <- rbind(otl[, .(BENE_ID, NDC, NDC_QTY, flag_nop)], 
  #                   rxl[, .(BENE_ID, NDC, NDC_QTY, flag_nop)])
  # 
  # nop_pain <- unique(nop_pain)
  # 
  # nop_pain <- merge(cohort, nop_pain, all.x = TRUE)
  # 
  # nop_pain[, has_nonopioid_pain_rx := as.numeric(any(!is.na(NDC))), by = BENE_ID]
  # nop_pain <- unique(nop_pain[, .(BENE_ID, has_nonopioid_pain_rx)])
  

  cohort <- cohort |>
    mutate(has_nonopioid_pain_rx_ipl = ifelse(BENE_ID %in% ipl$BENE_ID, 1, 0),
           has_nonopioid_pain_rx_otl_acute = ifelse(BENE_ID %in% otl_acute$BENE_ID, 1, 0),
           has_nonopioid_pain_rx_otl_opatient = ifelse(BENE_ID %in% otl_opatient$BENE_ID, 1, 0),
           has_nonopioid_pain_rx_rxl = ifelse(BENE_ID %in% rxl$BENE_ID, 1, 0))
  
  saveRDS(cohort, file.path(save_dir, my_year, paste0(my_year,"_cohort_has_non_opioid_rx.rds")))
  
}


