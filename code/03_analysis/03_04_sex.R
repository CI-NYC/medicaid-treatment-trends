
library(arrow)
library(lubridate)
library(data.table)
library(dplyr)

save_dir <- "/mnt/general-data/disability/post_surgery_opioid_use/tmp"

src_root <- "/mnt/processed-data/disability"

sex <- readRDS("/mnt/general-data/disability/create_cohort/final/joined_df.rds") |>
  select(BENE_ID, SEX_CD) |>
  mutate(SEX_M = as.numeric(SEX_CD == "M")) |>
  select(BENE_ID, SEX_M)


for (my_year in 2016:2019) {
  cohort <- readRDS(file.path(save_dir, my_year, paste0("cohort_",my_year,"_full.rds")))
  
  cohort <- cohort |>
  left_join(sex) |>
    select(BENE_ID, SEX_M)
  
  saveRDS(cohort, file.path(save_dir, my_year, paste0(my_year, "cohort_sex.rds")))
}