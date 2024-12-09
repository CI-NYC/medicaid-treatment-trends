# -------------------------------------
# Script:
# Author:
# Purpose:
# Notes:
# -------------------------------------
library(glue)
library(ggplot2)

save_dir <- "/mnt/general-data/disability/post_surgery_opioid_use/tmp"
result_dir <- "~/medicaid/medicaid_treatment_trends/output/adjusted_plots"

years <- c(2016:2019)
# pain_groups <- c("chronic pain only", "disability and chronic pain")
pain_groups <- c("chronic pain only", "disability only", "disability and chronic pain", "neither")

df <- data.frame()
for (my_year in years){
  for (pain in pain_groups){
    df <- rbind(df, readRDS(file.path(save_dir, my_year, glue("{my_year}_{pain}_mlr3_predictions.rds"))))
  }
}

df <- df |>
  group_by(year,pain_or_disability,race_ethnicity) |>
  summarise(opioid_yn = mean(opioid_yn),
            mediator_mean_daily_dose_mme = mean(mediator_mean_daily_dose_mme),
            mediator_opioid_days_covered = mean(mediator_opioid_days_covered),
            has_acupuncture = mean(has_acupuncture),
            has_physical_therapy = mean(has_physical_therapy),
            has_chiropractic = mean(has_chiropractic),
            has_counseling = mean(has_counseling),
            has_nonopioid_pain_rx = mean(has_nonopioid_pain_rx))

plot_function <- function(df, treatment_category, title, ylabel){
  p <- ggplot(df, aes(x = year, y=!!sym(treatment_category), color = race_ethnicity)) +
    geom_jitter(position=position_dodge(0.2)) +
    geom_line(aes(group = race_ethnicity), position=position_dodge(0.2)) +
    ggtitle(title) +
    ylab(ylabel) +
    facet_wrap(~factor(pain_or_disability, levels = c("chronic pain only","disability only","disability and chronic pain","neither"))) +
    labs(color = 'Race and Ethnicity') +
    theme_light()
  p
  ggsave(file = file.path(result_dir, paste0(treatment_category,".pdf")), width = 6, height = 4.5)
}


plot_function(df,"opioid_yn","Proportion of beneficiaries who were prescribed an opioid","Proportion")

plot_function(df,"mediator_mean_daily_dose_mme","Average daily MME","MME per day")

plot_function(df,"mediator_opioid_days_covered","Average proportion of days covered for opioids","Proportion days covered")

plot_function(df,"has_counseling","Proportion of beneficiaries receiving counseling","Proportion")

plot_function(df,"has_nonopioid_pain_rx","Proportion of beneficiaries receiving non-opioid pain medication","Proportion")

# plot_function(df,"has_acupuncture","Proportion of beneficiaries receiving acupuncture","Proportion")

plot_function(df,"has_physical_therapy","Proportion of beneficiaries receiving physical therapy","Proportion")

plot_function(df,"has_chiropractic","Proportion of beneficiaries receiving chiropractic work","Proportion")

