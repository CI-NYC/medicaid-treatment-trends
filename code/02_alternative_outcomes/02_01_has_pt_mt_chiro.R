# -------------------------------------
# Script: mediator_has_pt_mt_chiro
# Author: Anton Hung
# Purpose: redoing has_pt_mt_chiro for the larger time interval (washout_start_dt to washout_12mos_end_dt)
# need: 
# 1. mediator_has_physical_therapy 
# 2. mediator_has_massage_therapy
# 3. mediator_has_chiropractic
# Notes:
# -------------------------------------

# cohort_for_pt_mt_chiro <- as.data.table(readRDS(file.path("/mnt/general-data/disability/create_cohort/final/analysis_cohort.rds")))
# cohort_for_pt_mt_chiro <- cohort_for_pt_mt_chiro[, .(BENE_ID, washout_start_dt, washout_12mos_end_dt)]
# setkey(cohort_for_pt_mt_chiro, BENE_ID)

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

# massage therapy
mediator <- "Massage therapy"
codes <- c(names(codebook[[mediator]]$CPT), 
           names(codebook[[mediator]]$HCPC), 
           names(codebook[[mediator]]$Modifiers))
claims <- my_func(codes) |>
  rename(has_massage_therapy = new_column)
cohort <- merge(claims, cohort, all.y = T, by = "BENE_ID")
cohort[, has_massage_therapy := fifelse(is.na(has_massage_therapy), 0, has_massage_therapy)]
cohort <- unique(cohort)

# physical therapy, chiro, massage therapy all together
cohort[, has_pt_mt_chiro := 
         fifelse(has_physical_therapy + 
                   has_massage_therapy + 
                   has_chiropractic >= 1, 
                 1, 0)]

# Remove unneeded columns when done
cohort <- cohort[, -c("has_massage_therapy")]
