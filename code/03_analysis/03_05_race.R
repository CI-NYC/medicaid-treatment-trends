
library(dplyr)
library(data.table)
library(arrow)

drv <- "/mnt/general-data/disability/disenrollment/tafdebse"
save_dir <- "/mnt/general-data/disability/post_surgery_opioid_use/tmp"

race <- readRDS("/mnt/general-data/disability/create_cohort/final/desc_cohort.rds") |>
  select(BENE_ID, dem_race_cond)

for (my_year in 2016:2019) {
  cohort <- readRDS(file.path(save_dir, my_year, paste0("cohort_",my_year,"_full.rds")))
  
  cohort <- cohort |>
    left_join(race) |>
    select(BENE_ID, dem_race_cond)
  
  saveRDS(cohort, file.path(save_dir, my_year, paste0(my_year, "cohort_race.rds")))
}
