# -------------------------------------
# Script: opioid_plots
# Author: Anton Hung 2024-05-16
# Purpose:
# Notes:
# -------------------------------------

library(data.table)
library(ggplot2)
library(dplyr)
library(arrow)
library(lubridate)

save_dir <- "/mnt/general-data/disability/post_surgery_opioid_use/tmp"
result_dir <- "~/medicaid/medicaid_treatment_trends/output"

race <- readRDS("/mnt/general-data/disability/create_cohort/final/desc_cohort.rds") |>
  select(BENE_ID, dem_race_cond)

# dems <- open_dataset("/mnt/general-data/disability/create_cohort/intermediate/tafdebse/dem_df.parquet")
# dems <-
#   dems |> 
#   select(all_of(names(dems)))  |>
#   collect()

# Merging variables to each cohort
merge_cohort <- function(my_year){
  cohort <- readRDS(file.path(save_dir, my_year, paste0("cohort_",my_year,"_full.rds")))
  disability_or_pain <- readRDS(file.path(save_dir, my_year, paste0(my_year, "_cohort_disability_or_pain.rds")))
  mme <- readRDS(file.path(save_dir, my_year, paste0(my_year, "_mean_daily_dose_mme.rds")))
  proportion_days_covered <- readRDS(file.path(save_dir, my_year, paste0(my_year, "_proportion_days_covered.rds")))
  out <- cohort |>
    left_join(disability_or_pain) |>
    left_join(race) |>
    left_join(mme) |>
    left_join(proportion_days_covered) |>
    mutate(year = my_year) |>
    select(BENE_ID, year, disability_pain_cal, dem_race_cond, mediator_mean_daily_dose_mme, mediator_opioid_days_covered)
}

cohort <- rbind(merge_cohort(2016),
                merge_cohort(2017),
                merge_cohort(2018),
                merge_cohort(2019))
setDT(cohort)

cohort$mediator_mean_daily_dose_mme <- pmin(cohort$mediator_mean_daily_dose_mme,100)
# tail(sort(cohort$mediator_mean_daily_dose_mme), 20)
# hist(cohort[cohort$mediator_mean_daily_dose_mme > 0, ]$mediator_mean_daily_dose_mme)

# cohort <- cohort[, c("BENE_ID",
#                      "washout_start_dt",
#                      "washout_12mos_end_dt",
#                      "dem_race_cond",
#                      "disability_pain_12mos_cal",
#                      "opioid_pain_washout_12mos_cal"
# )]

# cohort <- cohort |>
#   mutate(race_ethnicity = ifelse(dem_race == "American Indian and Alaska Native (AIAN), non-Hispanic", "AIAN, non-Hispanic", dem_race))


# MME <- readRDS(file.path(save_dir, "trends_mean_daily_dose_mme.rds"))
# days_covered <- readRDS(file.path(save_dir, "trends_proportion_days_covered.rds"))



# Merge MME with the cohort
# cohort <- merge(MME, cohort, all.y = TRUE, by = "BENE_ID")

# Convert NAs to 0 for observations in the cohort that didn't have a claim
# cohort[, mediator_mean_daily_dose_mme := fifelse(is.na(mediator_mean_daily_dose_mme), 0, mediator_mean_daily_dose_mme)]


# Merge days covered with the cohort
# cohort <- merge(days_covered, cohort, all.y = TRUE, by = "BENE_ID")
# setDT(cohort)

# Convert NAs to 0 for observations in the cohort that didn't have a claim
# cohort[, mediator_opioid_days_covered := fifelse(is.na(mediator_opioid_days_covered), 0, mediator_opioid_days_covered)]


### Computing Results:
my_func <- function(data, which_race, which_year, which_pain_or_disability) {
  subset <- data[dem_race_cond == which_race &
                   year == which_year &
                   disability_pain_cal == which_pain_or_disability]
  
  num_bene <- nrow(subset)
  
  # subsetting down to just those who were prescribed an opioid during washout
  subset <- subset[mediator_opioid_days_covered > 0]
  
  opioid_proba <- nrow(subset)/num_bene
  se_opioid_proba <- sqrt((opioid_proba * (1-opioid_proba))/num_bene)
  
  average_mme <- mean(subset$mediator_mean_daily_dose_mme, na.rm=T)
  se_mme <- sd(subset$mediator_mean_daily_dose_mme, na.rm=T)/sqrt(num_bene)
  
  average_days_covered <- mean(subset$mediator_opioid_days_covered, na.rm=T)
  se_days_covered <- sd(subset$mediator_opioid_days_covered, na.rm=T)/sqrt(num_bene)
  
  
  return(c(which_race, 
           which_year, 
           which_pain_or_disability,
           num_bene,
           opioid_proba,
           se_opioid_proba,
           average_mme,
           se_mme,
           average_days_covered,
           se_days_covered
  ))
}

results <- data.frame()

for (i in c("multi_or_na", 
            "White, non-Hispanic", 
            "Black, non-Hispanic", 
            "Hispanic, all races", 
            "AIAN_or_HPI",
            "Asian, non-Hispanic")) {
  for (j in 2016:2019) {
    for (k in c("disability only", "chronic pain only", "neither", "disability and chronic pain")){
      results <- rbind(results, my_func(cohort, i, j, k))
      
    }
  }
}

colnames(results) <- c("race_ethnicity",
                       "year",
                       "pain_or_disability",
                       "number_of_beneficiaries",
                       "opioid_prop",
                       "se_opioid_prop",
                       "average_mme",
                       "se_mme",
                       "average_days_covered",
                       "se_days_covered")

results[, -c(1,3)] <- lapply(results[, -c(1,3)], as.numeric)

write.csv(results, file.path(result_dir, "results_df.csv"))

results <- read.csv(file.path(result_dir, "results_df.csv"))

p <- ggplot(results, aes(x = year, y=opioid_prop, color = race_ethnicity)) +
  geom_jitter(position=position_dodge(0.2)) +
  geom_line(aes(group = race_ethnicity), position=position_dodge(0.2)) +
  # geom_smooth()+
  ggtitle("Proportion of beneficiaries who were prescribed an opioid") +
  ylab("Proportion") +
  facet_wrap(~factor(pain_or_disability, levels = c("chronic pain only","disability only","disability and chronic pain","neither"))) +
  geom_errorbar(aes(ymin = opioid_prop - 1.96*se_opioid_prop, 
                    ymax = opioid_prop + 1.96*se_opioid_prop),
                width = 1, position = position_dodge(0.2)) +
  labs(color = 'Race and Ethnicity') +
  theme_classic()
p
ggsave(file = file.path(result_dir, "opioid_prop.pdf"), width = 6, height = 4.5)


p <- ggplot(results, aes(x = year, y=average_mme, color = race_ethnicity)) +
  geom_jitter(position=position_dodge(0.2)) +
  geom_line(aes(group = race_ethnicity), position=position_dodge(0.2)) +
  ggtitle("Average daily MME") +
  ylab("MME per day") +
  facet_wrap(~factor(pain_or_disability, levels = c("chronic pain only","disability only","disability and chronic pain","neither"))) +
  geom_errorbar(aes(ymin = average_mme - 1.96*se_mme, 
                    ymax = average_mme + 1.96*se_mme),
                width = 1, position = position_dodge(0.2)) +
  labs(color = 'Race and Ethnicity') +
  theme_classic()
p
ggsave(file = file.path(result_dir, "average_mme.pdf"), width = 6, height = 4.5)



p <- ggplot(results, aes(x = year, y=average_days_covered, color = race_ethnicity)) +
  geom_jitter(position=position_dodge(0.2)) +
  geom_line(aes(group = race_ethnicity), position=position_dodge(0.2)) +
  ggtitle("Average proportion of days covered for opioids") +
  ylab("Proportion days covered") +
  facet_wrap(~factor(pain_or_disability, levels = c("chronic pain only","disability only","disability and chronic pain","neither"))) +
  geom_errorbar(aes(ymin = average_days_covered - 1.96*se_days_covered, 
                    ymax = average_days_covered + 1.96*se_days_covered),
                width = 1, position = position_dodge(0.2)) +
  labs(color = 'Race and Ethnicity') +
  theme_classic()
p
ggsave(file = file.path(result_dir, "average_days_covered.pdf"), width = 6, height = 4.5)
