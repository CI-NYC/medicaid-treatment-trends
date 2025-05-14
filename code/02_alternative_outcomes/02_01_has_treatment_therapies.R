# -------------------------------------
# Script: outcomes
# Author: Anton Hung
# Purpose: Create columns for the following outcomes:
# 1. Counseling
# 2. Physical therapy
# 3. Acupuncture
# 4. Chiropractic work
# 5. Interventions
# Notes:
# -------------------------------------

# Libraries
library(arrow)
library(dplyr)
library(lubridate)
library(data.table)
library(yaml)

source("~/medicaid/medicaid-treatment-trends/R/helpers.R")

save_dir <- "/mnt/general-data/disability/post_surgery_opioid_use/tmp"

otl <- open_otl()

iph <- open_iph()

otl_vars <- c("BENE_ID", "CLM_ID", "LINE_SRVC_BGN_DT", "LINE_SRVC_END_DT", "LINE_PRCDR_CD_SYS", "LINE_PRCDR_CD", "REV_CNTR_CD")

iph_vars <- c("BENE_ID", "SRVC_BGN_DT", "SRVC_END_DT", paste0("PRCDR_CD_", 1:6))

excluded_cpt_cds <- c(36400:36415, "G0001", 43200:43272, 45300:45387, 70010:79999, 80048:89399, 93000:93278)

# function definition to remove repetition
# this function retrieves all the beneficiaries who have TRUE indicator for whether they had a claim for a certain treatment during the 12-month period

search_treatment_claims <- function(treatments_codes, df, treatment_name){
  print(treatment_name)
  
  codes <- treatments_codes |>
    filter(treatment == treatment_name) |>
    pull(cd)
  
  # Filter OTL to claims codes
  otl_claims <- select(otl, all_of(otl_vars)) |> 
    filter(LINE_PRCDR_CD %in% codes) |>
    collect() |>
    as.data.table()
  
  otl_claims[, LINE_SRVC_BGN_DT := fifelse(is.na(LINE_SRVC_BGN_DT), 
                                           LINE_SRVC_END_DT, 
                                           LINE_SRVC_BGN_DT)]
  
  otl_claims <- unique(merge(otl_claims, df, by = "BENE_ID"))
  
  otl_claims <- otl_claims[LINE_SRVC_BGN_DT %within% interval(followup_start_dt, 
                                                              followup_end_dt)]
  
  
  acute_CLM_ID <- otl_claims |>
    filter((grepl("^045[0-9]$|^0981$", REV_CNTR_CD) |
              grepl("^9928[1-5]|99288",LINE_PRCDR_CD)) &
             !LINE_PRCDR_CD %in% excluded_cpt_cds) |>
    pull(CLM_ID)
  
  otl_acute <- otl_claims |>
    filter(CLM_ID %in% acute_CLM_ID)
  
  otl_opatient <- otl_claims |>
    filter(!CLM_ID %in% acute_CLM_ID)
  
  
  iph_claims <- select(iph, all_of(iph_vars)) |>
    filter(PRCDR_CD_1 %in% codes |
             PRCDR_CD_2 %in% codes |
             PRCDR_CD_3 %in% codes |
             PRCDR_CD_4 %in% codes |
             PRCDR_CD_5 %in% codes |
             PRCDR_CD_6 %in% codes) |>
    collect() |>
    as.data.table()
  
  iph_claims[, SRVC_BGN_DT := fifelse(is.na(SRVC_BGN_DT), 
                                      SRVC_END_DT, 
                                      SRVC_BGN_DT)]
  
  iph_claims <- unique(merge(iph_claims, df, by = "BENE_ID"))
  
  iph_claims <- iph_claims[SRVC_BGN_DT %within% interval(followup_start_dt, 
                                                              followup_end_dt)]
  
  df <- df |>
    mutate(!!paste0("has_",treatment_name,"_iph") := ifelse(BENE_ID %in% iph_claims$BENE_ID, 1, 0),
           !!paste0("has_",treatment_name,"_otl_acute") := ifelse(BENE_ID %in% otl_acute$BENE_ID, 1, 0),
           !!paste0("has_",treatment_name,"_otl_outpatient") := ifelse(BENE_ID %in% otl_opatient$BENE_ID, 1, 0)) |>
    select(-washout_start_dt, -followup_start_dt, -followup_end_dt, -disability_pain_cal)
  
  saveRDS(df, file.path(save_dir, my_year, paste0(my_year,"_cohort_has_", treatment_name, ".rds")))
}


# Read in CPT, HCPC, and Modifier codes for mediator claims ####################
codes <- read_yaml("~/medicaid/undertreated-pain/data/public/mediator_codes.yml")
mediators <- c("Physical therapy",
               "Counseling",
               "Acupuncture",
               "Chiropractic",
               "Ablative techniques",
               "Botulinum toxin injections",
               "Electrical nerve stimulation",
               "Intrathecal drug therapies",
               "Epidural steroids",
               "Blocks",
               "Minimally invasive spinal procedures",
               "Trigger point injection")
treatments_df <- data.frame()
for (mediator in mediators) {
  for (code in codes[[mediator]]){
    treatments_df <- rbind(treatments_df, 
                           data.frame(cd = names(code), 
                                      treatment = rep(tolower(gsub(" ", "_", mediator)), length(names(code))))
    )
  }
}

treatments_df <- treatments_df |>
  mutate(treatment = ifelse(treatment %in% c("physical_therapy", 
                                             "counseling",
                                             "acupuncture",
                                             "chiropractic"), treatment, "intervention"))



# Loop through years ###########################################################

for (my_year in 2016:2019){
  print(my_year)
  
  cohort <- readRDS(file.path(save_dir, my_year, paste0("cohort_",my_year,"_pain_only.rds")))
  setDT(cohort)
  setkey(cohort, BENE_ID)

  search_treatment_claims(treatments_df, cohort, "physical_therapy")
  
  search_treatment_claims(treatments_df, cohort, "counseling")
  
  search_treatment_claims(treatments_df, cohort, "chiropractic")
  
  search_treatment_claims(treatments_df, cohort, "acupuncture")

  search_treatment_claims(treatments_df, cohort, "intervention")
  
}
