
library(dplyr)
library(data.table)

save_dir <- "/mnt/general-data/disability/post_surgery_opioid_use/tmp"

for (my_year in 2016:2019) {
  cohort <- readRDS(file.path(save_dir, my_year, paste0("cohort_",my_year,"_pain_only.rds"))) |>
    select(BENE_ID, disability_pain_cal)
  urbanicity <- readRDS(file.path(save_dir, my_year, paste0(my_year, "_cohort_urbanicity.rds")))
  region <- readRDS(file.path(save_dir, my_year, paste0(my_year, "_cohort_region.rds")))
  sex <- readRDS(file.path(save_dir, my_year, paste0(my_year, "_cohort_sex.rds")))
  age <- readRDS(file.path(save_dir, my_year, paste0(my_year, "_cohort_age.rds")))
  race <- readRDS(file.path(save_dir, my_year, paste0("cohort_",my_year,"_race.rds")))
  
  mme <- readRDS(file.path(save_dir, my_year, paste0(my_year, "_mean_daily_dose_mme_outpatient.rds")))
  proportion_days_covered <- readRDS(file.path(save_dir, my_year, paste0(my_year, "_proportion_days_covered_outpatient.rds")))
  non_opioid_rx <- readRDS(file.path(save_dir, my_year, paste0(my_year, "_cohort_has_non_opioid_rx.rds"))) |>
    mutate(has_nonopioid_pain_rx = as.numeric(has_nonopioid_pain_rx_otl_opatient + has_nonopioid_pain_rx_rxl > 0)) |>
    select(BENE_ID, has_nonopioid_pain_rx)
  physical_therapy <- readRDS(file.path(save_dir, my_year, paste0(my_year, "_cohort_has_physical_therapy.rds"))) |>
    select(BENE_ID, has_physical_therapy = has_physical_therapy_otl_outpatient)
  counseling <- readRDS(file.path(save_dir, my_year, paste0(my_year, "_cohort_has_counseling.rds"))) |>
    select(BENE_ID, has_counseling = has_counseling_otl_outpatient)
  acupuncture <- readRDS(file.path(save_dir, my_year, paste0(my_year, "_cohort_has_acupuncture.rds"))) |>
    select(BENE_ID, has_acupuncture = has_acupuncture_otl_outpatient)
  chiropractic <- readRDS(file.path(save_dir, my_year, paste0(my_year, "_cohort_has_chiropractic.rds"))) |>
    select(BENE_ID, has_chiropractic = has_chiropractic_otl_outpatient)
  intervention <- readRDS(file.path(save_dir, my_year, paste0(my_year, "_cohort_has_intervention.rds"))) |>
    select(BENE_ID, has_intervention = has_intervention_otl_outpatient)

  cohort <- cohort |>
    left_join(age) |>
    left_join(sex) |>
    left_join(region) |>
    left_join(urbanicity) |>
    left_join(race) |>
    mutate(race_multi_na = as.numeric(dem_race_cond %in% c("Multiracial, non-Hispanic", "Missing")),
           race_black = as.numeric(dem_race_cond == "Black, non-Hispanic"),
           race_hispanic = as.numeric(dem_race_cond == "Hispanic, all races"),
           race_aian_hpi = as.numeric(dem_race_cond == "AIAN_or_HPI"),
           race_asian = as.numeric(dem_race_cond == "Asian, non-Hispanic")) |>
    select(-dem_race_cond) |>
    left_join(mme) |>
    left_join(proportion_days_covered) |>
    mutate(has_opioid = as.numeric(mean_daily_dose_mme_outpatient>0)) |>
    left_join(non_opioid_rx) |>
    mutate(has_any_pharma = as.numeric(has_opioid + has_nonopioid_pain_rx > 0)) |>
    left_join(physical_therapy) |>
    left_join(counseling) |>
    left_join(acupuncture) |>
    left_join(chiropractic) |>
    mutate(has_acu_chiro = as.numeric(has_acupuncture + has_chiropractic > 0)) |>
    left_join(intervention) |>
    mutate(has_any_treatment = as.numeric(has_any_pharma +
                                            has_physical_therapy +
                                            has_counseling +
                                            has_chiropractic +
                                            has_acupuncture +
                                            has_intervention > 0))
  
  cohort$mean_daily_dose_mme_outpatient <- pmin(cohort$mean_daily_dose_mme_outpatient,100)

  saveRDS(cohort, file.path(save_dir, my_year, paste0(my_year, "_analysis_cohort.rds")))
}
