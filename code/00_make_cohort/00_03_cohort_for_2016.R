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

cohort <- readRDS("/mnt/general-data/disability/create_cohort/final/desc_cohort.rds")

dts_cohorts <- readRDS("/mnt/general-data/disability/create_cohort/intermediate/tafdedts/nest_dts.rds") |>
  filter(BENE_ID %in% cohort$BENE_ID)
sample_df <- dts_cohorts[1:100000,]

identify_beneficiaries <- function(data, year) {
  # enrollment_date <- data$ENRLMT_START_DT
  # enrollment_end <- data$ENRLMT_END_DT
  # last_enrollment_end <- lag(enrollment_end)
  
  # QUESTION 1: is this row's start date on Jan 1st of the correct year?
  q1 <- year(data$ENRLMT_START_DT) == year &
    month(data$ENRLMT_START_DT) == 1 &
    day(data$ENRLMT_START_DT) == 1

  # QUESTION 2: is this row continuous over at least 6 months?
  q2 <- (interval(data$ENRLMT_START_DT, data$ENRLMT_END_DT) |>
           as.period() |>
           month()) >= 6

  if (any(q1 & q2)) return(data$ENRLMT_START_DT[which(q1 & q2)])
  
  # # QUESTION 3: is the previous row continuous with the current row
  # end <- length(data$ENRLMT_START_DT)
  # q3 <- (lag(data$ENRLMT_END_DT)[1:end] + days(1)) == data$ENRLMT_START_DT[1:end]
  # 
  # # QUESTION 4: is the previous row continuous with the current row for at least 6 months?
  # q4 <- (interval(lag(data$ENRLMT_START_DT), data$ENRLMT_END_DT)) |> 
  #   as.period() |>  
  #   month() >= 6
  # 
  # if (any(q1 & q3 & q4, na.rm = TRUE)) {
  #   return(max(as.Date(paste0(year - 1, "-07-01")), 
  #              lag(data$ENRLMT_START_DT)[which(q1 & q3 & q4)], 
  #              na.rm = TRUE))
  # }
}

ptm <- proc.time()


for (year in c(2016)){
  
  plan(multisession, workers = 10) 
  
  cohort_x <- foreach(data = sample_df$data, id = sample_df$BENE_ID, 
                      .combine = "rbind", 
                      .options.future = list(chunk = 1e5)) %dofuture% {
                        date <- identify_beneficiaries(data, year)
                        if (is.null(date)) date <- NA_Date_
                        data.frame(BENE_ID = id, washout_start_dt = date)
                      }
  
  plan(sequential)
  
  cohort_x <- cohort_x |>
    filter(!is.na(washout_start_dt))
  saveRDS(cohort_x, file.path("/mnt/general-data/disability/post_surgery_opioid_use/tmp",
                              paste0("cohort_",year,".rds")))
  
}

elapsed_time <- proc.time() - ptm
cat("Elapsed time:", elapsed_time[3], "seconds\n")
# saveRDS(cohort_x, file.path("/mnt/general-data/disability/post_surgery_opioid_use/tmp", paste0(elapsed_time[3], ".rds")))



tmp <- dts_cohorts |>
  slice(1:1000) |>
  mutate(first_value = sapply(data, `[`, 1))

  
  mutate(start = data[[1]][[1,"ENRLMT_START_DT"]],
         end = data[[1]][[1,"ENRLMT_END_DT"]]) 
|>
  # filter(!is.na(end)) |>
  filter(start == as.Date("2016-01-01") & end > as.Date("2016-06-30"))
