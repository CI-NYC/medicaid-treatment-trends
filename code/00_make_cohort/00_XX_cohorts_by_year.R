# -------------------------------------
# Script:
# Author: Anton Hung
# Purpose:
# Notes: ROUGH work for testing, not used in final version
# -------------------------------------

# library(foreach)
# library(doFuture)
# library(lubridate)
# library(dplyr)

library(data.table)
library(lubridate)
library(dplyr)
library(arrow)
library(tictoc)
src_root <- "/mnt/processed-data/disability"

# Read in DTS 
files <- paste0(list.files(src_root, pattern = "TAFDEDTS", recursive = TRUE))
parquet_files <- grep("\\.parquet$", files, value = TRUE)
dts <- open_dataset(file.path(src_root, parquet_files))


my_year <- 2018

cohort <- readRDS("/mnt/general-data/disability/create_cohort/final/desc_cohort.rds") |>
  filter(year(washout_start_dt) <= my_year)

dts <- dts |>
  filter(BENE_ID %in% cohort$BENE_ID) |>
  filter(RFRNC_YR == my_year |
           (RFRNC_YR == my_year - 1 & ENRLMT_END_DT == paste0(my_year-1,"-12-31"))) |>
  select(BENE_ID, ENRLMT_START_DT, ENRLMT_END_DT, RFRNC_YR) |>
  collect()

# Convert date columns to numeric (days since a reference date)
ranges <- dts %>%
  mutate(
    start_num = as.numeric(ENRLMT_START_DT),
    end_num = as.numeric(ENRLMT_END_DT)
  )

ranges2 <- ranges[10000001:10100000,]

tic()
# Arrange by start date within each ID group
collapsed_ranges <- ranges2 %>%
  group_by(BENE_ID) %>%
  arrange(start_num) %>%
  
  # group_by(CLM_ID) |>
  mutate(prev_end_num = lag(end_num, default = first(end_num))) %>%
  filter(start_num <= prev_end_num + 1) %>%
  summarise(
    date_start = as.Date(first(ENRLMT_START_DT)),
    date_end = as.Date(max(ENRLMT_END_DT))
  )
toc()

cohort <- 

claims <- claims |>
  left_join(collapsed_ranges) |>
  mutate(cohort_exclusion_noncontinuous = case_when(
    washout_start_dt %within% interval(date_start, date_end) & 
      surgery_dt %within% interval(date_start, date_end) ~ 0, TRUE ~ 1))



identify_beneficiaries <- function(data, year) {
  enrollment_date <- data$ENRLMT_START_DT
  enrollment_end <- data$ENRLMT_END_DT
  last_enrollment_end <- lag(enrollment_end)
  
  # QUESTION 1: is this row's start date on Jan 1st of the correct year?
  q1 <- year(enrollment_date) == year &
    month(enrollment_date) == 1 & 
    day(enrollment_date) == 1
  
  # QUESTION 2: is this row continuous over at least 6 months?
  # q2 <- (interval(enrollment_date, enrollment_end) |>
  #          as.period() |>
  #          month()) >= 6
  q2 <- time_length(enrollment_end - enrollment_date, "days") >= 182
  
  if (any(q1 & q2)) return(enrollment_date[first(which(q1 & q2))])
  
  # QUESTION 3: is the previous row continuous with the current row
  end <- length(enrollment_date)
  q3 <- (last_enrollment_end[1:end] + days(1)) == enrollment_date[1:end]
  
  # QUESTION 4: is the previous row continuous with the current row for at least 6 months?
  # q4 <- (interval(lag(enrollment_date), enrollment_end)) |>
  #   as.period() |>
  #   month() >= 6
  q4 <- time_length(enrollment_end - lag(enrollment_date), "days") >= 182
  
  
  if (any(q1 & q3 & q4, na.rm = TRUE)) {
    return(max(as.Date(paste0(year - 1, "-07-01")),
               lag(enrollment_date)[which(q1 & q3 & q4)],
               na.rm = TRUE))
  }
}





for (i in c(0:12)){
  # start time
  ptm <- proc.time()
  cat("Processing", i, "millionth row\n")
  
  
  sample_df <- dts_cohorts[(1+i*1000000):(min(1000000*(i+1), nrow(dts_cohorts))),]
  plan(multisession, workers = 10)
  
  cohort_x <- foreach(data = sample_df$data, id = sample_df$BENE_ID,
                      .combine = "rbind",
                      .options.future = list(chunk = 1e5)) %dofuture% {
                        date <- identify_beneficiaries(data, my_year)
                        if (is.null(date)) date <- NA_Date_
                        data.frame(BENE_ID = id, washout_start_dt = date)
                      }
  
  plan(sequential)
  
  cohort_x <- cohort_x |>
    filter(!is.na(washout_start_dt))
  saveRDS(cohort_x, file.path("/mnt/general-data/disability/post_surgery_opioid_use/tmp",my_year,
                              paste0("cohort_",my_year,"_",i,".rds")))
  
  elapsed_time <- proc.time() - ptm
  cat("Elapsed time:", elapsed_time[3], "seconds\n")
}

