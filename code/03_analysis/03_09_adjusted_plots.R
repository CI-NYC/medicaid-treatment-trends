# -------------------------------------
# Script:
# Author:
# Purpose:
# Notes:
# -------------------------------------
library(glue)
library(tidyverse)

save_dir <- "/mnt/general-data/disability/post_surgery_opioid_use/tmp"
result_dir <- "~/medicaid/medicaid_treatment_trends/output/adjusted_plots"
my_palette <- c("#b2182b","#ef8a62","#fddbc7","#d1e5f0","#67a9cf","#2166ac")

years <- c(2016:2019)
# pain_groups <- c("chronic pain only", "disability and chronic pain")
pain_groups <- c("chronic pain only", "disability only", "disability and chronic pain", "neither")

race_names <- data.frame(race_ethnicity = c("race_white", "race_black", "race_asian", "race_hispanic", "race_multi_na", "race_aian_hpi"),
                         dem_race_cond = c("White, non-Hispanic", "Black, non-Hispanic", "Asian, non-Hispanic", "Hispanic, all races", "Multiracial, non-Hispanic", "AIAN or HPI"))

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
            has_nonopioid_pain_rx = mean(has_nonopioid_pain_rx)) |>
  left_join(race_names)

# Cleaning race names
races <- data.frame(race_ethnicity = c("race_multi_na","race_white","race_black","race_hispanic","race_aian_hpi","race_asian"),
                    race_name = c("Multiracial or missing","White, non-Hispanic","Black, non-Hispanic","Hispanic, all races","AIAN or HPI","Asian, non-Hispanic"))

results <- df |>
  left_join(races) |>
  select(-race_ethnicity, race_ethnicity=race_name)

plot_function <- function(df, treatment_category, title, ylabel){
  p <- ggplot(df, aes(x = year, y=!!sym(treatment_category), color = race_ethnicity)) +
    geom_jitter(position=position_dodge(0.2)) +
    geom_line(aes(group = race_ethnicity), position=position_dodge(0.2)) +
    # ggtitle(title) +
    ylab(ylabel) +
    facet_wrap(~ str_to_title(factor(pain_or_disability, levels = c("chronic pain only", "disability only", "disability and chronic pain", "neither")))) +
    scale_color_manual(values = my_palette) +
    labs(color = 'Race and Ethnicity') +
    theme_light() +
    theme(strip.text = element_text(color = "black")) # Set facet text to black
  p
  ggsave(file = file.path(result_dir, paste0(treatment_category,".pdf")), width = 6, height = 4.5)
}


plot_function(results,"opioid_yn","Proportion of beneficiaries who were prescribed an opioid","Proportion (95% CI)")

plot_function(results,"mediator_mean_daily_dose_mme","Average daily MME","MME per day (95% CI)")

plot_function(results,"mediator_opioid_days_covered","Average proportion of days covered for opioids","Proportion days covered (95% CI)")

plot_function(results,"has_counseling","Proportion of beneficiaries receiving counseling","Proportion (95% CI)")

plot_function(results,"has_nonopioid_pain_rx","Proportion of beneficiaries receiving non-opioid pain medication","Proportion (95% CI)")

plot_function(results,"has_acupuncture","Proportion of beneficiaries receiving acupuncture","Proportion (95% CI)")

plot_function(results,"has_physical_therapy","Proportion of beneficiaries receiving physical therapy","Proportion (95% CI)")

plot_function(results,"has_chiropractic","Proportion of beneficiaries receiving chiropractic work","Proportion (95% CI)")

