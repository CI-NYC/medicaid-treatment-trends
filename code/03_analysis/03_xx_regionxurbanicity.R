# -------------------------------------
# Script:
# Author:
# Purpose:
# Notes:
# -------------------------------------


library(tidyverse)
library(janitor)
library(readxl)
library(arrow)
library(lubridate)
library(data.table)
library(tictoc)
library(foreach)
library(doFuture)
library(furrr)
library(glue)

save_dir <- "/mnt/general-data/disability/post_surgery_opioid_use/tmp"

# Create a data frame with states and their respective regions
states_df <- data.frame(
  state = c("AL", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA", "ID", "IL", 
            "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", 
            "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", 
            "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", 
            "WY","HI","AK"),
  region = c("South", "West", "South", "West", "West", "Northeast", "South", "South", 
             "South", "South", "West", "Midwest", "Midwest", "Midwest", "Midwest", 
             "South", "South", "Northeast", "South", "Northeast", "Midwest", "Midwest", 
             "South", "Midwest", "West", "Midwest", "West", "Northeast", "Northeast", 
             "West", "Northeast", "South", "Midwest", "Midwest", "South", "West", 
             "Northeast", "Northeast", "South", "Midwest", "South", "South", "West", 
             "Northeast", "South", "West", "South", "Midwest", "West", "West", "West")
)

td <- "/mnt/processed-data/disability/"
dbs_files <- paste0(list.files(td, pattern = "*TAFDEBSE_\\d+\\.parquet", recursive = TRUE))
dbs <- open_dataset(paste0(td, dbs_files), format = "parquet", partition = "year")
states <- dbs |> 
  select(BENE_ID, STATE_CD)  |>
  filter(!is.na(BENE_ID)) |>
  collect() |> 
  as.data.table() |>
  unique() |>
  left_join(states_df, by = c("STATE_CD" = "state")) 

states <- states[!duplicated(BENE_ID)]

urbanicity <- rbind(readRDS(file.path(save_dir, "2016/2016cohort_urbanicity.rds")),
                    readRDS(file.path(save_dir, "2017/2017cohort_urbanicity.rds")),
                    readRDS(file.path(save_dir, "2018/2018cohort_urbanicity.rds")),
                    readRDS(file.path(save_dir, "2019/2019cohort_urbanicity.rds"))) |>
  distinct()

race <- rbind(readRDS(file.path(save_dir, "2016/2016cohort_race.rds")),
              readRDS(file.path(save_dir, "2017/2017cohort_race.rds")),
              readRDS(file.path(save_dir, "2018/2018cohort_race.rds")),
              readRDS(file.path(save_dir, "2019/2019cohort_race.rds"))) |>
  distinct()

cohort <- urbanicity |>
  left_join(states) |>
  left_join(race)


# Create a contingency table
for (race in unique(cohort$dem_race_cond)){
  
  cohort2 <- cohort |>
    filter(dem_race_cond==race)
  
  contingency_table <- table(cohort2$region, cohort2$RUCC_2013)
  
  write.csv(contingency_table, file.path("~/medicaid/medicaid_treatment_trends/output/contingency_tables_checking_positivity", glue("RUCCxRegion_pairs_{race}.csv")))
}

