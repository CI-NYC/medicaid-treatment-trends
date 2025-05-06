
library(tidyverse)
library(data.table)

save_dir <- "/mnt/general-data/disability/post_surgery_opioid_use/tmp"

for (my_year in 2016:2019) {
  
  dts_cohorts <- readRDS(file.path(save_dir, my_year, paste0("cohort_",my_year,"_full.rds")))
  # dts_cohorts <- dts_cohorts[1:100,]
  setDT(dts_cohorts)
  setkey(dts_cohorts, BENE_ID)
  
  disability_or_pain <- readRDS(file.path(save_dir, my_year, paste0(my_year,"_cohort_disability_or_pain.rds")))
  
  dts_cohorts <- dts_cohorts |>
    left_join(disability_or_pain) |>
    filter(disability_pain_cal %in% c("disability and chronic pain", "chronic pain only"))
  
  saveRDS(dts_cohorts, file.path(save_dir, my_year, paste0("cohort_",my_year,"_pain_only.rds")))
  
}