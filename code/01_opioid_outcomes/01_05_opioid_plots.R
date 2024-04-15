# -------------------------------------
# Script:
# Author:
# Purpose:
# Notes:
# -------------------------------------

library(data.table)
library(ggplot2)

load_dir <- "/mnt/general-data/disability/create_cohort/final"
save_dir <- "/mnt/general-data/disability/post_surgery_opioid_use/tmp"
result_dir <- "/home/amh2389/medicaid/medicaid_treatment_trends/output"
cohort <- readRDS(file.path(load_dir, "desc_cohort.rds"))
setDT(cohort)

cohort <- cohort[, c("BENE_ID",
                     "washout_start_dt",
                     "washout_12mos_end_dt",
                     "dem_race_cond",
                     "disability_pain_12mos_cal",
                     "opioid_pain_washout_12mos_cal"
)]


MME <- readRDS(file.path(save_dir, "trends_mean_daily_dose_mme.rds"))
days_covered <- readRDS(file.path(save_dir, "trends_proportion_days_covered.rds"))



# Merge MME with the cohort
cohort <- merge(MME, cohort, all.y = TRUE, by = "BENE_ID")

# Convert NAs to 0 for observations in the cohort that didn't have a claim
cohort[, mediator_mean_daily_dose_mme := fifelse(is.na(mediator_mean_daily_dose_mme), 0, mediator_mean_daily_dose_mme)]


# Merge days covered with the cohort
cohort <- merge(days_covered, cohort, all.y = TRUE, by = "BENE_ID")
setDT(cohort)

# Convert NAs to 0 for observations in the cohort that didn't have a claim
cohort[, mediator_opioid_days_covered := fifelse(is.na(mediator_opioid_days_covered), 0, mediator_opioid_days_covered)]


### Computing Results:
my_func <- function(data, which_race, which_year, which_pain_or_disability) {
  subset <- data[dem_race_cond == which_race &
                   year(washout_start_dt) == which_year &
                   disability_pain_12mos_cal == which_pain_or_disability]
  
  num_bene <- nrow(subset)
  
  # subsetting down to just those who were prescribed an opioid during washout
  subset <- subset[mediator_opioid_days_covered > 0]
  
  opioid_proba <- round(nrow(subset)/num_bene, 3)
  se_opioid_proba <- round(sqrt((opioid_proba * (1-opioid_proba))/num_bene),3)
  
  average_mme <- round(mean(subset$mediator_mean_daily_dose_mme, na.rm=T),3)
  se_mme <- round(sd(subset$mediator_mean_daily_dose_mme, na.rm=T)/sqrt(num_bene),3)
  
  average_days_covered <- round(mean(subset$mediator_opioid_days_covered, na.rm=T),3)
  se_days_covered <- round(sd(subset$mediator_opioid_days_covered, na.rm=T)/sqrt(num_bene),3)
  
  
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

p <- ggplot(results, aes(x = year, y=opioid_prop, color = race_ethnicity)) +
  geom_jitter(position=position_dodge(0.2)) +
  geom_line(aes(group = race_ethnicity), position=position_dodge(0.2)) +
  # geom_smooth()+
  ggtitle("Proportion of beneficiaries who were prescribed an opioid, by pain/disability status, race/ethnicity, and year") +
  ylab("Proportion") +
  facet_wrap(~factor(pain_or_disability, levels = c("chronic pain only","disability only","disability and chronic pain","neither"))) +
  geom_errorbar(aes(ymin = opioid_prop - 1.96*se_opioid_prop, 
                    ymax = opioid_prop + 1.96*se_opioid_prop),
                width = 1, position = position_dodge(0.2))
p
ggsave(file = file.path(result_dir, "opioid_prop.pdf"), width = 10, height = 7)


p <- ggplot(results, aes(x = year, y=average_mme, color = race_ethnicity)) +
  geom_jitter(position=position_dodge(0.2)) +
  geom_line(aes(group = race_ethnicity), position=position_dodge(0.2)) +
  ggtitle("Average MME per month, by pain/disability status, race/ethnicity, and year") +
  ylab("MME per month") +
  facet_wrap(~factor(pain_or_disability, levels = c("chronic pain only","disability only","disability and chronic pain","neither"))) +
  geom_errorbar(aes(ymin = average_mme - 1.96*se_mme, 
                    ymax = average_mme + 1.96*se_mme),
                width = 1, position = position_dodge(0.2))
p
ggsave(file = file.path(result_dir, "average_mme.pdf"), width = 10, height = 7)



p <- ggplot(results, aes(x = year, y=average_days_covered, color = race_ethnicity)) +
  geom_jitter(position=position_dodge(0.2)) +
  geom_line(aes(group = race_ethnicity), position=position_dodge(0.2)) +
  ggtitle("Average proportion of days covered over 12-month span, by pain/disability status, race/ethnicity, and year") +
  ylab("Proportion days covered") +
  facet_wrap(~factor(pain_or_disability, levels = c("chronic pain only","disability only","disability and chronic pain","neither"))) +
  geom_errorbar(aes(ymin = average_days_covered - 1.96*se_days_covered, 
                    ymax = average_days_covered + 1.96*se_days_covered),
                width = 1, position = position_dodge(0.2))
p
ggsave(file = file.path(result_dir, "average_days_covered.pdf"), width = 10, height = 7)