# -------------------------------------
# Script:
# Author:
# Purpose:
# Notes:
# -------------------------------------

library(arrow)
library(tidyverse)


load_dir <- "/mnt/general-data/disability/post_surgery_opioid_use/tmp"


cohort_2019 <- data.frame()
for (i in 0:13) {
  cohort_2019 <- rbind(cohort_2019, readRDS(file.path(load_dir, "2019", paste0("cohort_",2019,"_",i,".rds"))))
}
cohort_2019 <- cohort_2019[!is.na(cohort_2019$washout_start_dt),]

cohort_2018 <- data.frame()
for (i in 0:12) {
  cohort_2018 <- rbind(cohort_2018, readRDS(file.path(load_dir, "2018", paste0("cohort_",2018,"_",i,".rds"))))
}

cohort_2017 <- data.frame()
for (i in 0:11) {
  cohort_2017 <- rbind(cohort_2017, readRDS(file.path(load_dir, "2017", paste0("cohort_",2017,"_",i,".rds"))))
}

cohort_2016 <- data.frame()
for (i in 0:10) {
  cohort_2016 <- rbind(cohort_2016, readRDS(file.path(load_dir, "2016", paste0("cohort_",2016,"_",i,".rds"))))
}

desc_cohort <- readRDS("/mnt/general-data/disability/create_cohort/final/desc_cohort.rds")



########## STATES exclusions
# excluding RI for all years
# excluding michigan for 2017 and 2018

src_root <- "/mnt/processed-data/disability"

# read in tafdebse data base (all years)
files <- paste0(list.files(src_root, pattern = "TAFDEBSE", recursive = TRUE))
parquet_files <- grep("\\.parquet$", files, value = TRUE)
dbs <- open_dataset(file.path(src_root, parquet_files))

states <- dbs |> 
  select(BENE_ID, STATE_CD) |>
  collect() |>
  filter(!is.na(BENE_ID)) |>
  distinct()

exclusion_state_2016_2019 <- states |> 
  filter(STATE_CD %in% c("MD", "RI")) |> 
  distinct(BENE_ID) |>
  mutate(cohort_exclusion_state = 1)

exclusion_state_2017_2018 <- states |> 
  filter(STATE_CD %in% c("MD","RI","MI")) |> 
  distinct(BENE_ID) |>
  mutate(cohort_exclusion_state = 1)


cohort_2016 <- cohort_2016 |>
  filter(!BENE_ID %in% exclusion_state_2016_2019$BENE_ID)

cohort_2017 <- cohort_2017 |>
  filter(!BENE_ID %in% exclusion_state_2017_2018$BENE_ID)

cohort_2018 <- cohort_2018 |>
  filter(!BENE_ID %in% exclusion_state_2017_2018$BENE_ID)

cohort_2019 <- cohort_2019 |>
  filter(!BENE_ID %in% exclusion_state_2016_2019$BENE_ID)



##### Establishing follow-up start and end dates
cohort_2016 <- cohort_2016 |>
  mutate(followup_start_dt = as.Date("2016-07-01"),
         followup_end_dt = as.Date("2016-12-31"))

cohort_2017 <- cohort_2017 |>
  mutate(followup_start_dt = washout_start_dt %m+% months(6),
         followup_end_dt = followup_start_dt %m+% days(182))

cohort_2018 <- cohort_2018 |>
  mutate(followup_start_dt = washout_start_dt %m+% months(6),
         followup_end_dt = followup_start_dt %m+% days(182))

cohort_2019 <- cohort_2019 |>
  mutate(followup_start_dt = washout_start_dt %m+% months(6),
         followup_end_dt = followup_start_dt %m+% days(182))

saveRDS(cohort_2016, file.path(load_dir, "2016/cohort_2016_full.rds"))
saveRDS(cohort_2017, file.path(load_dir, "2017/cohort_2017_full.rds"))
saveRDS(cohort_2018, file.path(load_dir, "2018/cohort_2018_full.rds"))
saveRDS(cohort_2019, file.path(load_dir, "2019/cohort_2019_full.rds"))