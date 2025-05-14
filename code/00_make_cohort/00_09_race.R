
library(tidyverse)
library(data.table)

save_dir <- "/mnt/general-data/disability/post_surgery_opioid_use/tmp"
source("~/medicaid/medicaid-treatment-trends/R/helpers.R")

race <- open_demo() |>
  select(BENE_ID, RFRNC_YR, RACE_ETHNCTY_CD) |>
  collect() |>
  drop_na() |> # drop all missing values 
  group_by(BENE_ID) |>
  arrange(RFRNC_YR) |>
  filter(row_number() == 1) |> # keep only the first non-missing value for each bene_id
  select(-RFRNC_YR) |> # don't need ref year anymore
  ungroup() 

for (my_year in 2016:2019) {
  
  dts_cohorts <- readRDS(file.path(save_dir, my_year, paste0("cohort_",my_year,"_pain_only.rds")))
  setDT(dts_cohorts)
  setkey(dts_cohorts, BENE_ID)
  
  dts_cohorts <- dts_cohorts |>
    left_join(race) |>
    mutate(dem_race_cond = case_when(RACE_ETHNCTY_CD == "1" ~ "White, non-Hispanic",
                                RACE_ETHNCTY_CD == "2" ~ "Black, non-Hispanic",
                                RACE_ETHNCTY_CD == "3" ~ "Asian, non-Hispanic",
                                RACE_ETHNCTY_CD %in% c("4","5") ~ "AIAN_or_HPI",
                                RACE_ETHNCTY_CD == "6" ~ "Multiracial, non-Hispanic",
                                RACE_ETHNCTY_CD == "7" ~ "Hispanic, all races",
                                TRUE ~ "Missing")) |>
    select(BENE_ID, dem_race_cond)
    
  
  
  saveRDS(dts_cohorts, file.path(save_dir, my_year, paste0("cohort_",my_year,"_race.rds")))
  
}
