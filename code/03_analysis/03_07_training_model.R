
library(tictoc)
library(tidyverse)
library(lubridate)
library(mlr3)
library(mlr3learners)
library(mlr3extralearners) 
library(mlr3superlearner)
library(purrr)
library(glue)

options(future.globals.maxSize = 1e9)  # Increase to 1 GB

save_dir <- "/mnt/general-data/disability/post_surgery_opioid_use/tmp"

# pain groups
pain_groups <- c("chronic pain only", "disability and chronic pain")
# pain_groups <- c(˛"disability only")

year <- 2017

treatments <- c(
  "has_opioid",
  "has_nonopioid_pain_rx",
  "has_any_pharma",
  "mean_daily_dose_mme_outpatient",
  "opioid_days_covered_outpatient",
  "has_physical_therapy",
  "has_counseling",
  "has_chiropractic",
  "has_acupuncture",
  "has_acu_chiro",
  "has_intervention",
  "has_any_treatment"
)
treatment_type <- c(
  "binomial",
  "binomial",
  "binomial",
  "continuous",
  "continuous",
  "binomial",
  "binomial",
  "binomial",
  "binomial",
  "binomial",
  "binomial",
  "binomial"
)

learners <- c("mean", "glm", "lightgbm", "earth")
learners_workaround <- c("glm","lightgbm", "mean")

set.seed(72)
cohort <- readRDS(file.path(save_dir, year, paste0(year, "_analysis_cohort.rds"))) |>
  as.data.frame()

for (pain in pain_groups){
  # if (pain == "neither"){
  #   learners = learners_workaround
  # } else {
  #   learners = learners_standard
  # }
  fits <- list()
  analysis_cohort <- cohort |>
    filter(disability_pain_cal == pain)
  
  tic(paste0("Year: ", year, ", Pain group: ", pain))
  
  for (i in 1:11){
    # if (i == 4){
    #   learners = learners_workaround
    # }
    # print(treatments[i])
    nrare = sum(analysis_cohort[,treatments[i]])
    neff = min(nrow(analysis_cohort), 5*nrare)
    cv_folds <- ifelse(neff >= 10000, 2, 5)

    fit <- mlr3superlearner(data = analysis_cohort[, c("age_enrollment",
                                                       "SEX_M",
                                                       "region_west",
                                                       "region_midwest",
                                                       "region_northeast",
                                                       "RUCC_2013",
                                                       "RUCC_missing",
                                                       "race_multi_na",
                                                       "race_black",
                                                       "race_hispanic",
                                                       "race_aian_hpi",
                                                       "race_asian",
                                                       treatments[i])],
                            discrete = F,
                            target = treatments[i],
                            library = learners,
                            outcome_type = treatment_type[i],
                            folds = cv_folds)
    fit <- list(fit=fit,year=year,disability_pain=pain,treatment=treatments[i])
    fits <- append(fits, list(fit))
  }
  toc()

  saveRDS(fits, file.path(save_dir, year, glue("{year}_{pain}_mlr3_results.rds")))
}


