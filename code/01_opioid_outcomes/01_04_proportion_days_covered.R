# READ ME -----------------------------------------------------------------
#
#       Author: Nick Williams
#       Last updated: 2023-11-15
#       Updated: 2024-05-09 (Anton)
# 
# -------------------------------------------------------------------------

library(lubridate)
library(foreach)
library(doFuture)
library(tidyverse)

# src_root <- "/mnt/processed-data/disability"
# drv_root <- "/mnt/general-data/disability/mediation_unsafe_pain_mgmt"
save_dir <- "/mnt/general-data/disability/post_surgery_opioid_use/tmp"


for (my_year in 2016:2019) {
  
  dts_cohorts <- readRDS(file.path(save_dir, my_year, paste0("cohort_",my_year,"_full.rds")))
  setDT(dts_cohorts)
  setkey(dts_cohorts, BENE_ID)
  
  otl <- readRDS(file.path(save_dir, my_year, paste0(my_year,"_otl_opioid_pain_rx.rds")))
  rxl <- readRDS(file.path(save_dir, my_year, paste0(my_year,"_rxl_opioid_pain_rx.rds")))
  
  prop_days_covered <- function(data) {
    dur <- 0
    current_int <- data$rx_int[1]
    for (i in 1:nrow(data)) {
      check <- intersect(current_int, data$rx_int[i + 1])
      if (is.na(check)) {
        # if they don't intersect, add the duration of the first interval
        dur <- dur + as.duration(current_int)
        current_int <- data$rx_int[i + 1]
      } else {
        # if they do intersect, then update current interval as the union
        current_int <- union(current_int, data$rx_int[i + 1])
      }
    }
    
    max(time_length(dur, "days") / 182, 1 / 182)
  }
  
  opioids <- otl |> 
    mutate(rx_int = interval(LINE_SRVC_BGN_DT, LINE_SRVC_BGN_DT + days(1)), 
           rx_int = intersect(rx_int, interval(followup_start_dt, 
                                               followup_end_dt))) |> 
    select(BENE_ID, rx_int) |> 
    as_tibble() |> 
    bind_rows({
      rxl |> 
        mutate(DAYS_SUPPLY = replace_na(DAYS_SUPPLY, 1), 
               rx_int = interval(RX_FILL_DT, RX_FILL_DT + days(DAYS_SUPPLY)), 
               rx_int = intersect(rx_int, interval(followup_start_dt, 
                                                   followup_end_dt))) |> 
        select(BENE_ID, rx_int) |> 
        as_tibble()
    })
  
  opioids <- group_by(opioids, BENE_ID) |> 
    arrange(BENE_ID, int_start(rx_int)) |> 
    nest()
  
  plan(multisession, workers = 50)
  
  opioids$mediator_opioid_days_covered <- 
    foreach(x = opioids$data, 
            .combine = "c",
            .options.future = list(chunk.size = 1e4)) %dofuture% {
              prop_days_covered(x)
            }
  
  plan(sequential)
  
  opioids <- select(dts_cohorts, BENE_ID) |> 
    left_join(select(opioids, -data)) |> 
    mutate(mediator_opioid_days_covered = replace_na(mediator_opioid_days_covered, 0))
  
  # saveRDS(opioids, file.path(save_dir, "trends_proportion_days_covered.rds"))
  saveRDS(opioids, file.path(save_dir, my_year, paste0(my_year,"_proportion_days_covered.rds")))
  
}