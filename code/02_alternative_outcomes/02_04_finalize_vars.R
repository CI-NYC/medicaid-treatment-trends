# -------------------------------------
# Script: outcomes
# Author: Anton Hung
# Purpose: Create columns for the following outcomes:
# 1. Counseling
# 2. Multimodal pain treatment
# 3. Non-opioid pain prescription
# 4. Physical therapy
# 5. Acupuncture
# 6. Chiropractic work
# Notes:
# -------------------------------------

# has_multimodal_pain_treatment <- readRDS("has_multimodal_pain_treatment.rds")
# cohort <- merge(has_multimodal_pain_treatment, cohort, all.y = TRUE, by = "BENE_ID")

# all of the outcomes except for acupuncture have already been calculated in previous scripts 
# cohort <- cohort[, .(BENE_ID, 
#                      washout_start_dt, 
#                      washout_cal_end_dt,
#                      washout_12mos_end_dt, 
#                      dem_race_cond, 
#                      disability_pain_12mos_cal,
#                      has_counseling,
#                      has_multimodal_pain_treatment,
#                      has_nonopioid_pain_rx,
#                      has_physical_therapy,
#                      has_chiropractic)]


source("/home/amh2389/medicaid/medicaid_treatment_trends/code/02_alternative_outcomes/02_00_setup.R")

for (my_year in 2016:2019){
  
  cohort <- readRDS(file.path(save_dir, my_year, paste0("cohort_",my_year,"_full.rds")))
  setDT(cohort)
  setkey(cohort, BENE_ID)

  # Acupuncture
  mediator = "Acupuncture"
  codes <- c(names(codebook[[mediator]]$CPT), 
             names(codebook[[mediator]]$HCPC), 
             names(codebook[[mediator]]$Modifiers))
  claims <- my_func(codes) |>
    select(c("BENE_ID", "new_column")) |>
    rename(has_acupuncture = new_column)
  cohort <- merge(claims, cohort, all.y = T, by = "BENE_ID")
  cohort[, has_acupuncture := fifelse(is.na(has_acupuncture), 0, has_acupuncture)]
  cohort <- unique(cohort)
  
  
  # Physical therapy
  mediator = "Physical therapy"
  codes <- c(names(codebook[[mediator]]$CPT), 
             names(codebook[[mediator]]$HCPC), 
             names(codebook[[mediator]]$Modifiers))
  claims <- my_func(codes) |>
    rename(has_physical_therapy = new_column)
  cohort <- merge(claims, cohort, all.y = T, by = "BENE_ID")
  cohort[, has_physical_therapy := fifelse(is.na(has_physical_therapy), 0, has_physical_therapy)]
  cohort <- unique(cohort)
  
  # chiropractic
  mediator <- "Chiropractic"
  codes <- c(names(codebook[[mediator]]$CPT), 
             names(codebook[[mediator]]$HCPC))
  claims <- my_func(codes) |>
    rename(has_chiropractic = new_column)
  cohort <- merge(claims, cohort, all.y = T, by = "BENE_ID")
  cohort[, has_chiropractic := fifelse(is.na(has_chiropractic), 0, has_chiropractic)]
  cohort <- unique(cohort)
  
  # Counseling
  mediator = "Counseling"
  codes <- c(names(codebook[[mediator]]$CPT), 
             names(codebook[[mediator]]$HCPC), 
             names(codebook[[mediator]]$ICD10))
  claims <- my_func(codes) |>
    select(c("BENE_ID", "new_column")) |>
    rename(has_counseling = new_column)
  cohort <- merge(claims, cohort, all.y = T, by = "BENE_ID")
  cohort[, has_counseling := fifelse(is.na(has_counseling), 0, has_counseling)]
  cohort <- unique(cohort)
  
  
  cohort <- cohort[, .(BENE_ID, has_acupuncture, has_physical_therapy, has_chiropractic, has_counseling)]
  
  saveRDS(cohort, file.path(save_dir, my_year, paste0(my_year,"_cohort_has_alternative_treatments.rds")))
}
