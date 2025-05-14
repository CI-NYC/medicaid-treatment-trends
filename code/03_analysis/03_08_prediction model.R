
library(glue)
library(tictoc)
library(dplyr)
library(mlr3)
library(mlr3superlearner)


save_dir <- "/mnt/general-data/disability/post_surgery_opioid_use/tmp"

my_year <- 2019
pain_groups <- c("chronic pain only", "disability and chronic pain")
# pain_groups <- c("chronic pain only", "disability only", "disability and chronic pain")
# pain_groups <- c("neither")

race_groups <- c("race_white", 
                 "race_multi_na",
                 "race_black", 
                 "race_hispanic",
                 "race_aian_hpi",
                 "race_asian")

  
treatments <- c("has_opioid",
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
                "has_any_treatment")



cohort <- readRDS(file.path(save_dir, my_year, paste0(my_year, "_analysis_cohort.rds")))

for (pain in pain_groups){
  fit <- append(readRDS(file.path(save_dir, my_year, glue("{my_year}_{pain}_mlr3_results.rds"))),
                        readRDS(file.path(save_dir, my_year, glue("{my_year}_{pain}_mlr3_results_any_treatment.rds"))))
  predictions_pain_group <- data.frame()
  
  analysis_cohort <- cohort |>
    filter(disability_pain_cal == pain)
  
  tic(paste0("Year: ", my_year, ", Pain group: ", pain))
  for (race in race_groups){
    
    # preallocating dataframe for storing the predicted outcomes for each race
    predictions_race_group <- data.frame(matrix(NA, nrow = nrow(analysis_cohort), ncol = 15))
    colnames(predictions_race_group) <-
      c(
        "year","pain_or_disability","race_ethnicity", 
        "has_opioid","has_nonopioid_pain_rx","has_any_pharma",
        "mean_daily_dose_mme_outpatient","opioid_days_covered_outpatient",
        "has_physical_therapy","has_counseling",
        "has_chiropractic","has_acupuncture","has_acu_chiro",
        "has_intervention","has_any_treatment"
        )
    predictions_race_group <- predictions_race_group |>
      mutate(year = my_year,
             pain_or_disability = pain,
             race_ethnicity = race)
    
    if (race == "race_white") {
      analysis_cohort <- analysis_cohort |>
        mutate(across(all_of(race_groups[2:6]), ~ 0))
    } else {
      analysis_cohort <- analysis_cohort |>
        mutate(across(all_of(race_groups[2:6]), ~ 0)) |>
        mutate(!!race := 1)
    }
    

    for (i in 1:12){
      predictions_race_group[,i+3] <- predict(fit[[i]]$fit, analysis_cohort)
    }
    
    predictions_pain_group <- rbind(predictions_pain_group, predictions_race_group)
  }
  toc()
  saveRDS(predictions_pain_group, file.path(save_dir, my_year, glue("{my_year}_{pain}_mlr3_predictions.rds")))
}

