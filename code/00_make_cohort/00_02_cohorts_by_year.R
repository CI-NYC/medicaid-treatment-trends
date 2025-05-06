# -------------------------------------
# Script:
# Author: Nick Williams
# Purpose:
# Notes:
# -------------------------------------

library(foreach)
library(doFuture)
library(lubridate)
library(dplyr)

my_year <- 2017

cohort <- readRDS("/mnt/general-data/disability/create_cohort/final/desc_cohort.rds") |>
  filter(year(washout_start_dt) <= my_year)


dts_cohorts <- readRDS("/mnt/general-data/disability/create_cohort/intermediate/tafdedts/nest_dts.rds") |>   filter(BENE_ID %in% cohort$BENE_ID)


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

