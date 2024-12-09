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

for (my_year in 2016:2019) {
  cohort <- readRDS(file.path(save_dir, my_year, paste0("cohort_",my_year,"_full.rds")))
  
  cohort <- merge(cohort, states, by = "BENE_ID", all.x = TRUE) |>
    mutate(region_west = as.numeric(region == "West"),
           region_midwest = as.numeric(region == "Midwest"),
           region_northeast = as.numeric(region == "Northeast")) |>
    select(BENE_ID, region_west, region_midwest, region_northeast)
  
  saveRDS(cohort, file.path(save_dir, my_year, paste0(my_year, "cohort_region.rds")))
}
