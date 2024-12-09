
library(dplyr)
library(data.table)

save_dir <- "/mnt/general-data/disability/post_surgery_opioid_use/tmp"

race <- readRDS("/mnt/general-data/disability/create_cohort/final/desc_cohort.rds") |>
  select(BENE_ID, dem_race_cond)

for (my_year in 2017) {
  disability_or_pain <- readRDS(file.path(save_dir, my_year, paste0(my_year, "_cohort_disability_or_pain.rds")))
  urbanicity <- readRDS(file.path(save_dir, my_year, paste0(my_year, "cohort_urbanicity.rds")))
  region <- readRDS(file.path(save_dir, my_year, paste0(my_year, "cohort_region.rds")))
  sex <- readRDS(file.path(save_dir, my_year, paste0(my_year, "cohort_sex.rds")))
  age <- readRDS(file.path(save_dir, my_year, paste0(my_year, "cohort_age.rds")))
  race <- readRDS(file.path(save_dir, my_year, paste0(my_year, "cohort_race.rds")))
  mme <- readRDS(file.path(save_dir, my_year, paste0(my_year, "_mean_daily_dose_mme.rds")))
  proportion_days_covered <- readRDS(file.path(save_dir, my_year, paste0(my_year, "_proportion_days_covered.rds")))
  alternative_treaments <- readRDS(file.path(save_dir, my_year, paste0(my_year, "_cohort_has_alternative_treatments.rds")))
  non_opioid_rx <- readRDS(file.path(save_dir, my_year, paste0(my_year, "_cohort_has_non_opioid_rx.rds")))
  
  cohort <- disability_or_pain |>
    left_join(age) |>
    left_join(sex) |>
    left_join(region) |>
    left_join(urbanicity) |>
    left_join(race) |>
    mutate(race_multi_na = as.numeric(dem_race_cond == "multi_or_na"),
           race_black = as.numeric(dem_race_cond == "Black, non-Hispanic"),
           race_hispanic = as.numeric(dem_race_cond == "Hispanic, all races"),
           race_aian_hpi = as.numeric(dem_race_cond == "AIAN_or_HPI"),
           race_asian = as.numeric(dem_race_cond == "Asian, non-Hispanic")) |>
    select(-dem_race_cond) |>
    left_join(mme) |>
    left_join(proportion_days_covered) |>
    mutate(opioid_yn = as.numeric(mediator_opioid_days_covered>0)) |>
    left_join(alternative_treaments) |>
    left_join(non_opioid_rx)
  
  cohort$mediator_mean_daily_dose_mme <- pmin(cohort$mediator_mean_daily_dose_mme,100)

  saveRDS(cohort, file.path(save_dir, my_year, paste0(my_year, "analysis_cohort.rds")))
}
