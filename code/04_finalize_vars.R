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
cohort <- cohort[, .(BENE_ID, 
                     washout_start_dt, 
                     washout_12mos_end_dt, 
                     dem_race_cond, 
                     disability_pain_12mos_cal,
                     has_counseling,
                     has_multimodal_pain_treatment,
                     has_nonopioid_pain_rx,
                     has_physical_therapy,
                     has_chiropractic)]


# Acupuncture
mediator = "Acupuncture"
codebook <- read_yaml("/home/amh2389/medicaid/plotting_trends/code/mediator_codes.yml")
codes <- c(names(codebook[[mediator]]$CPT), 
           names(codebook[[mediator]]$HCPC), 
           names(codebook[[mediator]]$Modifiers))
claims <- my_func(codes) |>
  select(c("BENE_ID", "new_column")) |>
  rename(has_acupuncture = new_column)
cohort <- merge(claims, cohort, all.y = T, by = "BENE_ID")
cohort[, has_acupuncture := fifelse(is.na(has_acupuncture), 0, has_acupuncture)]
cohort <- unique(cohort)


