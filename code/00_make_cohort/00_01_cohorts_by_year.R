# -------------------------------------
# Script: cohorts_by_year
# Author: Anton Hung
# Purpose:
# Notes:
# -------------------------------------

library(lubridate)
library(dplyr)
library(foreach)
library(doParallel)

cohort <- readRDS("/mnt/general-data/disability/create_cohort/final/desc_cohort.rds")

dts_cohorts <- readRDS("/mnt/general-data/disability/create_cohort/intermediate/tafdedts/nest_dts.rds") |>
  filter(BENE_ID %in% cohort$BENE_ID)



# identify_beneficiaries <- function(df, which_year){
#   # general structure:
#   # iterate through people
#   # within a person, iterate through their enrollment dates
#   # if eligible, append the beneficiary's appropriate washout start date to the output
#   # some intermediate variables:
#     # dt_1 refers to the CURRENT enrollment period (current row)
#     # dt_0 refers to the PREVIOUS enrollment period (previous row)
#   
#   # initialize output data frame
#   result <- data.frame(BENE_ID = character(),
#                        washout_start_dt = character())
#   
#   # iterate through beneficiaries
#   for (i in 1:nrow(df)) {
#     dts <- df$data[[i]]
#     # iterate through one person's enrollment dates 
#     for (j in 1:nrow(dts)) {
#       dt_1 <- dts[j,]
# 
#       # QUESTION 1: is their BEGIN DATE in the first half of the year?
#       # yes: go to QUESTION 2
#       if (year(dt_1$ENRLMT_START_DT %m+% months(6)) == which_year){
#         
#         # QUESTION 2: is this NOT the first enrollment period (i.e. first row)?
#         # yes: go to QUESTION 3
#         # no: go to QUESTION 5
#         if (j > 1){
#           dt_0 <- dts[j-1,]
#           
#           # QUESTION 3: is the previous enrollment period (i.e. previous row) continuous with the current row?
#           # yes: go to QUESTION 4
#           # no: go to QUESTiON 5
#           if (dt_0$ENRLMT_END_DT %m+% days(1) == dt_1$ENRLMT_START_DT){
#             
#             # QUESTION 4: is the previous row in the second half of the previous year?
#             # yes: RESULT 1! append the BEGIN DATE of the PREVIOUS row 
#             # reasoning: this beneficiary WAS NOT eligible to be followed for the previous year, so there is no concern of overlap with the previous year's data
#             # no: RESULT 2! append the BEGIN DATE of the PREVIOUS row PLUS 6 MONTHS
#             # reasoning: this beneficiary WAS eligible to be followed for the previous year. We need to add 6 months because we can't allow the follow up periods for this beneficiary to overlap between the current year and the previous year
#             if (year(dt_0$ENRLMT_START_DT %m+% months(6)) == which_year) {
#               # RESULT 1
#               result[nrow(result)+1,] = c(df$BENE_ID[i],
#                                   dt_0$ENRLMT_START_DT)
#               next
#               
#             } else {
#               # RESULT 2
#               result[nrow(result)+1,] = c(df$BENE_ID[i],
#                                   dt_0$ENRLMT_START_DT %m+% months(6))
#               next
#             }
#             
#           }
#           # QUESTION 5: is the current/first enrollment period continuous over at least 6 months?
#           # yes: RESULT 3! append the BEGIN DATE of the CURRENT row
#           if (dt_1$ENRLMT_START_DT %m+% months(6) <= dt_1$ENRLMT_END_DT) {
#             # RESULT 3
#             result[nrow(result)+1,] = c(df$BENE_ID[i],
#                                 dt_1$ENRLMT_START_DT)
#             next
#           }
#           
#         }
#       }
#     }
#   }
#   return (result)
# }


# sampled_df <- dts_cohorts[sample(nrow(dts_cohorts), size = 100000, replace = FALSE), ]

identify_beneficiaries <- function(df, which_year){
  # general structure:
  # iterate through people
  # within a person, iterate through their enrollment dates
  # if eligible, append the beneficiary's appropriate washout start date to the output
  # some intermediate variables:
  # dt_1 refers to the CURRENT enrollment period (current row)
  # dt_0 refers to the PREVIOUS enrollment period (previous row)
  
  # Set the number of cores (adjust as needed)
  num_cores <- 10
  
  # Register parallel backend
  cl <- makeCluster(num_cores)
  registerDoParallel(cl)
  

  # iterate through beneficiaries
  result_list <- foreach(i = 1:nrow(df), .packages = c("lubridate"), .combine = "rbind") %dopar% {
    dts <- df$data[[i]]

        # iterate through one person's enrollment dates 
    for (j in 1:nrow(dts)) {
      dt_1 <- dts[j,]
      
      # QUESTION 1: is their BEGIN DATE in the first half of the year?
      # yes: go to QUESTION 2
      if (year(dt_1$ENRLMT_START_DT %m+% months(6)) == which_year){
        
        # QUESTION 2: is this NOT the first enrollment period (i.e. first row)?
        # yes: go to QUESTION 3
        # no: go to QUESTION 5
        if (j > 1){
          dt_0 <- dts[j-1,]
          
          # QUESTION 3: is the previous enrollment period (i.e. previous row) continuous with the current row?
          # yes: go to QUESTION 4
          # no: go to QUESTiON 5
          if (dt_0$ENRLMT_END_DT %m+% days(1) == dt_1$ENRLMT_START_DT){
            
            # QUESTION 4: is the previous row in the second half of the previous year?
            # yes: RESULT 1! append the BEGIN DATE of the PREVIOUS row 
            # reasoning: this beneficiary WAS NOT eligible to be followed for the previous year, so there is no concern of overlap with the previous year's data
            # no: RESULT 2! append the BEGIN DATE of the PREVIOUS row PLUS 6 MONTHS
            # reasoning: this beneficiary WAS eligible to be followed for the previous year. We need to add 6 months because we can't allow the follow up periods for this beneficiary to overlap between the current year and the previous year
            if (year(dt_0$ENRLMT_START_DT %m+% months(6)) == which_year) {
              # RESULT 1
              return (data.frame(BENE_ID = df$BENE_ID[i], washout_start_dt = dt_0$ENRLMT_START_DT))
              
            } else {
              # RESULT 2
              return (data.frame(BENE_ID = df$BENE_ID[i], washout_start_dt = dt_0$ENRLMT_START_DT %m+% months(6)))
            }
          }
        }
        # QUESTION 5: is the current/first enrollment period continuous over at least 6 months?
        # yes: RESULT 3! append the BEGIN DATE of the CURRENT row
        if (dt_1$ENRLMT_START_DT %m+% months(6) <= dt_1$ENRLMT_END_DT) {
          # RESULT 3
          return (data.frame(BENE_ID = df$BENE_ID[i], washout_start_dt = dt_1$ENRLMT_START_DT))
        }
      } else {
        next
      }
    }
  }
  # Stop the parallel backend
  stopCluster(cl)
  # Combine results into a single data frame
  # result <- do.call(rbind, result_list)
  return(result_list)
}


for (year in c(2016)) {
  cohort_x <- identify_beneficiaries(dts_cohorts, year)
  saveRDS(cohort_x, file.path("/mnt/general-data/disability/post_surgery_opioid_use/tmp", 
                              paste0("cohort_",year,".rds")))
}
