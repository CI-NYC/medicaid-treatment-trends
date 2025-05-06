# READ ME -----------------------------------------------------------------
#
#       Author: Nick Williams
#       Updated: 2024-01-26
#       Updated: 2024-05-09 (Anton)
# 
# -------------------------------------------------------------------------

library(arrow)
library(dplyr)
library(lubridate)
library(data.table)

save_dir <- "/mnt/general-data/disability/post_surgery_opioid_use/tmp"

source("~/medicaid/medicaid-treatment-trends/R/helpers.R")

# Read in RXL (pharmacy line)
rxl <- open_rxl()

# Read in RXL (inpatient line)
ipl <- open_ipl()

# Read in OTL (Other services line) 
otl <- open_otl()

excluded_cpt_cds <- c(36400:36415, "G0001", 43200:43272, 45300:45387, 70010:79999, 80048:89399, 93000:93278)


# Read in opioid pain list
op <- readRDS(file.path("/mnt/general-data/disability/mediation_unsafe_pain_mgmt", "mediation_unsafe_pain_mgmt_opioid_pain_ndc.rds"))

for (my_year in 2016:2019) {
  dts_cohorts <- readRDS(file.path(save_dir, my_year, paste0("cohort_",my_year,"_pain_only.rds")))
  setDT(dts_cohorts)
  setkey(dts_cohorts, BENE_ID)
  
  
  # IPL ---------------------------------------------------------------------
  
  # Filter OTL to opioid pain NDC
  ipl_vars <- c("BENE_ID", "CLM_ID", "LINE_SRVC_BGN_DT", "LINE_SRVC_END_DT", "NDC", "NDC_QTY")
  
  ipl_opioids <- select(ipl, all_of(ipl_vars)) |> 
    filter(NDC %in% op$NDC) |>
    collect() |> 
    as.data.table()
  
  ipl_opioids[, LINE_SRVC_BGN_DT := fifelse(is.na(LINE_SRVC_BGN_DT), 
                                    LINE_SRVC_END_DT, 
                                    LINE_SRVC_BGN_DT)]
  
  # Inner join with cohort 
  ipl_opioids <- unique(merge(ipl_opioids, dts_cohorts, by = "BENE_ID"))
  ipl_opioids <- merge(ipl_opioids, op[, c(1, 3, 4)], all.x = TRUE, by = "NDC")
  
  # Filter to claims within mediator time-frame
  ipl_opioids <- ipl_opioids[LINE_SRVC_BGN_DT %within% interval(followup_start_dt, 
                                                followup_end_dt), 
             .(BENE_ID, CLM_ID, LINE_SRVC_BGN_DT, followup_start_dt, followup_end_dt, NDC, NDC_QTY, 
               flag_opioid_analgesic, flag_opioid_anesthetics)] |>
    mutate(setting = "inpatient_acute")
  
  # OTL ---------------------------------------------------------------------
  
  # Filter OTL to opioid pain NDC
  otl_vars <- c("BENE_ID", "CLM_ID", "LINE_SRVC_BGN_DT", "LINE_SRVC_END_DT", "NDC", "NDC_QTY", "REV_CNTR_CD", "LINE_PRCDR_CD")
  
  otl_opioids <- select(otl, all_of(otl_vars)) |> 
    filter(NDC %in% op$NDC) |>
    collect() |> 
    as.data.table()
  
  otl_opioids[, LINE_SRVC_BGN_DT := fifelse(is.na(LINE_SRVC_BGN_DT), 
                                    LINE_SRVC_END_DT, 
                                    LINE_SRVC_BGN_DT)]
  
  # Inner join with cohort 
  otl_opioids <- unique(merge(otl_opioids, dts_cohorts, by = "BENE_ID"))
  otl_opioids <- merge(otl_opioids, op[, c(1, 3, 4)], all.x = TRUE, by = "NDC")
  
  # Filter to claims within mediator time-frame
  otl_opioids <- otl_opioids[LINE_SRVC_BGN_DT %within% interval(followup_start_dt, 
                                                followup_end_dt), 
             .(BENE_ID, CLM_ID, LINE_SRVC_BGN_DT, followup_start_dt, followup_end_dt, NDC, NDC_QTY, REV_CNTR_CD, LINE_PRCDR_CD, flag_opioid_analgesic, flag_opioid_anesthetics)]
  
  acute_CLM_ID <- otl_opioids |>
    filter((grepl("^045[0-9]$|^0981$", REV_CNTR_CD) |
              grepl("^9928[1-5]|99288",LINE_PRCDR_CD)) &
             !LINE_PRCDR_CD %in% excluded_cpt_cds) |>
    pull(CLM_ID)
  
  otl_opioids <- otl_opioids |>
    mutate(setting = ifelse(CLM_ID %in% acute_CLM_ID, "other_acute", "other_outpatient")) |>
    select(-c("LINE_PRCDR_CD", "REV_CNTR_CD"))
  
  otl_opioids <- rbind(ipl_opioids, otl_opioids)
  
  # RXL ---------------------------------------------------------------------
  
  rxl_vars <- c("BENE_ID", "CLM_ID", "RX_FILL_DT", "NDC", "NDC_QTY", "DAYS_SUPPLY")
  
  rxl_opioids <- select(rxl, all_of(rxl_vars)) |> 
    filter(NDC %in% op$NDC) |>
    collect() |> 
    as.data.table()
  
  # Inner join with cohort 
  rxl_opioids <- unique(merge(rxl_opioids, dts_cohorts, by = "BENE_ID"))
  rxl_opioids <- merge(rxl_opioids, op[, c(1, 3, 4)], all.x = TRUE, by = "NDC")
  
  # Filter to claims within mediator time-frame
  rxl_opioids <- rxl_opioids[RX_FILL_DT %within% interval(followup_start_dt, 
                                          followup_end_dt), 
             .(BENE_ID, CLM_ID, RX_FILL_DT, followup_start_dt, followup_end_dt, NDC, NDC_QTY, DAYS_SUPPLY,
               flag_opioid_analgesic, flag_opioid_anesthetics)]
  
  # Export ------------------------------------------------------------------
  
  # saveRDS(otl, file.path(save_dir, "trends_otl_opioid_pain_rx.rds"))
  # saveRDS(rxl, file.path(save_dir, "trends_rxl_opioid_pain_rx.rds"))

  saveRDS(otl_opioids, file.path(save_dir, my_year, paste0(my_year, "_otl_opioid_pain_rx.rds")))
  saveRDS(rxl_opioids, file.path(save_dir, my_year, paste0(my_year, "_rxl_opioid_pain_rx.rds")))
}