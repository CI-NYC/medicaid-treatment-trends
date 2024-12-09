
library(arrow)
library(lubridate)
library(data.table)
library(dplyr)
library(tidyverse)


save_dir <- "/mnt/general-data/disability/post_surgery_opioid_use/tmp"

src_root <- "/mnt/processed-data/disability"

# Read in DBS (demographics)
files <- paste0(list.files(src_root, pattern = "TAFDEBSE", recursive = TRUE))
parquet_files <- grep("\\.parquet$", files, value = TRUE)
dbs <- open_dataset(file.path(src_root, parquet_files))

# keep age and birth date columns for the first year each beneficiary appears in demographics
birth_dates_cols <-
  dbs |>
  arrange(RFRNC_YR) |>
  select(BENE_ID, BIRTH_DT, AGE) |>
  collect()

# keep only the first non NA birth dates
birth_dates <-
  birth_dates_cols |>
  drop_na(BIRTH_DT) |> # 46,894 missing a birth date before doing this
  distinct(BENE_ID, .keep_all = T) 


for (my_year in 2016:2019) {
  cohort <- readRDS(file.path(save_dir, my_year, paste0("cohort_",my_year,"_full.rds")))
  cohort <- cohort |>
    mutate(washout_start_dt = case_when( is.na(washout_start_dt) ~ as.Date(paste0(my_year, "-01-01")), TRUE ~ washout_start_dt))
  
  cohort <- cohort |>
    left_join(birth_dates) |>
    mutate(age_enrollment = floor(as.numeric(difftime(washout_start_dt, BIRTH_DT, units = "days") / 365.25))) |>
    select(BENE_ID, age_enrollment)
  
  saveRDS(cohort, file.path(save_dir, my_year, paste0(my_year, "cohort_age.rds")))
}
