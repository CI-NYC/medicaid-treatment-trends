
library(tidyverse)
library(data.table)
library(tidyr)

save_dir <- "/mnt/general-data/disability/post_surgery_opioid_use/tmp"

results <- data.frame()

for (my_year in 2016:2019) {
  
  dts_cohorts <- readRDS(file.path(save_dir, my_year, paste0("cohort_",my_year,"_pain_only.rds")))
  setDT(dts_cohorts)
  setkey(dts_cohorts, BENE_ID)
  
  race <- readRDS(file.path(save_dir, my_year, paste0("cohort_",my_year,"_race.rds")))
  
  dts_cohorts <- dts_cohorts |>
    left_join(race) |>
    group_by(dem_race_cond, disability_pain_cal) |>
    summarise(counts = n())
  
  results <- rbind(results, dts_cohorts |>
    pivot_wider(names_from = disability_pain_cal, values_from = counts, values_fill = list(counts = 0)) |>
    mutate(year = my_year) |>
    relocate(year, .before = dem_race_cond))
}

write.csv(results, "~/medicaid/medicaid-treatment-trends/code/04_table_one/strata_sizes_all.csv")


all <- data.frame()
# num unique beneficiaries
for (my_year in 2016:2019) {
  
  all <- rbind(all,readRDS(file.path(save_dir, my_year, paste0("cohort_",my_year,"_pain_only.rds"))))
  
  
}