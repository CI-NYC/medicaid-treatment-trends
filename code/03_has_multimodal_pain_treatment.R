# -------------------------------------
# Script: mediator_has_multimodal_pain_treatment
# Author: Anton Hung
# Purpose: determining which beneficiaries received multimodal pain treatment.
# Must first find the following:
# 1. "mediator_has_ablative_techniques",
# 2. "mediator_has_blocks",
# 3. "mediator_has_botulinum_toxin",
# 4. "mediator_has_epidural_steroid",
# 5. "mediator_has_intrathecal_drug_therapy",
# 6. "mediator_has_minimally_invasive_spinal_procedure",
# 7. "mediator_nonopioid_pain_rx",
# 8. "mediator_has_counseling",
# 9. "mediator_has_trigger_point_injection", 
# 10. "mediator_has_pt_mt_chiro"
# Notes: these recalculations are being done because we are using a different time interval
# -------------------------------------

# Read in OTL (Other services line) 
files <- paste0(list.files(src_root, pattern = "TAFOTL", recursive = TRUE))
parquet_files <- grep("\\.parquet$", files, value = TRUE)
otl <- open_dataset(file.path(src_root, parquet_files))

# Ablative techniques
mediator = "Ablative techniques"
codes <- c(names(codebook[[mediator]]$CPT), 
           names(codebook[[mediator]]$HCPC), 
           names(codebook[[mediator]]$Modifiers))
claims <- my_func(codes) |>
  rename(has_ablative_techniques = new_column)
cohort <- merge(claims, cohort, all.y = T, by = "BENE_ID")
cohort[, has_ablative_techniques := fifelse(is.na(has_ablative_techniques), 0, has_ablative_techniques)]
cohort <- unique(cohort)


# Blocks
mediator <- "Blocks"
codes <- c(names(codebook[[mediator]]$CPT), 
           names(codebook[[mediator]]$HCPC), 
           names(codebook[[mediator]]$Modifiers))
claims <- my_func(codes) |>
  rename(has_blocks = new_column)
cohort <- merge(claims, cohort, all.y = T, by = "BENE_ID")
cohort[, has_blocks := fifelse(is.na(has_blocks), 0, has_blocks)]
cohort <- unique(cohort)


# Botulinum toxin injections
mediator <- "Botulinum toxin injections"
codes <- c(names(codebook[[mediator]]$CPT), 
           names(codebook[[mediator]]$HCPC), 
           names(codebook[[mediator]]$Modifiers))
claims <- my_func(codes) |>
  rename(has_botulinum_toxin = new_column)
cohort <- merge(claims, cohort, all.y = T, by = "BENE_ID")
cohort[, has_botulinum_toxin := fifelse(is.na(has_botulinum_toxin), 0, has_botulinum_toxin)]
cohort <- unique(cohort)


# Epidural steroids
mediator <- "Epidural steroids"
codes <- c(names(codebook[[mediator]]$CPT), 
           names(codebook[[mediator]]$HCPC), 
           names(codebook[[mediator]]$Modifiers))
claims <- my_func(codes) |>
  rename(has_epidural_steroid = new_column)
cohort <- merge(claims, cohort, all.y = T, by = "BENE_ID")
cohort[, has_epidural_steroid := fifelse(is.na(has_epidural_steroid), 0, has_epidural_steroid)]
cohort <- unique(cohort)


# Intrathecal drug therapies
mediator <- "Intrathecal drug therapies"
codes <- c(names(codebook[[mediator]]$CPT), 
           names(codebook[[mediator]]$HCPC), 
           names(codebook[[mediator]]$Modifiers))
claims <- my_func(codes) |>
  rename(has_intrathecal_drug_therapy = new_column)
cohort <- merge(claims, cohort, all.y = T, by = "BENE_ID")
cohort[, has_intrathecal_drug_therapy := fifelse(is.na(has_intrathecal_drug_therapy), 0, has_intrathecal_drug_therapy)]
cohort <- unique(cohort)


# Minimally invasive spinal procedures
mediator <- "Minimally invasive spinal procedures"
codes <- c(names(codebook[[mediator]]$CPT), 
           names(codebook[[mediator]]$HCPC))
claims <- my_func(codes) |>
  select(c("BENE_ID", "new_column")) |>
  rename(has_minimally_invasive_spinal_procedure = new_column)
cohort <- merge(claims, cohort, all.y = T, by = "BENE_ID")
cohort[, has_minimally_invasive_spinal_procedure := fifelse(is.na(has_minimally_invasive_spinal_procedure), 0, has_minimally_invasive_spinal_procedure)]
cohort <- unique(cohort)


# Trigger point injection
mediator <- "Trigger point injection"
codes <- c(names(codebook[[mediator]]$CPT), 
           names(codebook[[mediator]]$HCPC), 
           names(codebook[[mediator]]$Modifiers))
claims <- my_func(codes) |>
  select(c("BENE_ID", "new_column")) |>
  rename(has_trigger_point_injection = new_column)
cohort <- merge(claims, cohort, all.y = T, by = "BENE_ID")
cohort[, has_trigger_point_injection := fifelse(is.na(has_trigger_point_injection), 0, has_trigger_point_injection)]
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


# 9. Non-opioid pain treatment (calculated in 02_has_nonopioid_pain_rx.R)
# cohort <- merge(nop_pain, cohort, all.y = TRUE, by = "BENE_ID")
# cohort <- unique(cohort)


# 10. physical therapy, massage therapy, chiropractic
# https://github.com/CI-NYC/disability/blob/c18082c9c726a2d99ab24889ce9fd11e58e965b3/projects/mediation_unsafe_pain_mgmt/05_merge_mediators.R#L140
# cohort <- merge(has_pt_mt_chiro, cohort, all.y = TRUE, by = "BENE_ID")
# cohort <- unique(cohort)


cohort[, count_multimodal_pain_treatment := rowSums(.SD), 
       .SDcols = c("has_ablative_techniques",
                   "has_blocks",
                   "has_botulinum_toxin",
                   "has_epidural_steroid",
                   "has_intrathecal_drug_therapy",
                   "has_minimally_invasive_spinal_procedure",
                   "has_nonopioid_pain_rx",
                   "has_counseling",
                   "has_trigger_point_injection", 
                   "has_pt_mt_chiro")
][, has_multimodal_pain_treatment := 
    fifelse(count_multimodal_pain_treatment >= 2, 1, 0)] # |>
# select(c("BENE_ID", "has_multimodal_pain_treatment"))

# saveRDS(cohort, file.path(drv_root, "has_multimodal_pain_treatment.rds"))
