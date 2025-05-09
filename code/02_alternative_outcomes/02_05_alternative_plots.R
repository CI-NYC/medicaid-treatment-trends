# -------------------------------------
# Script: alternative_plots
# Author: Anton Hung
# Purpose:
# Notes:
# -------------------------------------

library(data.table)
library(ggplot2)
library(dplyr)
library(arrow)
library(lubridate)

save_dir <- "/mnt/general-data/disability/post_surgery_opioid_use/tmp"
result_dir <- "/home/amh2389/medicaid/medicaid_treatment_trends/output"

race <- readRDS("/mnt/general-data/disability/create_cohort/final/desc_cohort.rds") |>
  select(BENE_ID, dem_race_cond)

merge_cohort <- function(my_year){
  cohort <- readRDS(file.path(save_dir, my_year, paste0("cohort_",my_year,"_full.rds")))
  disability_or_pain <- readRDS(file.path(save_dir, my_year, paste0(my_year, "_cohort_disability_or_pain.rds")))
  alternative_treaments <- readRDS(file.path(save_dir, my_year, paste0(my_year, "_cohort_has_alternative_treatments.rds")))
  non_opioid_rx <- readRDS(file.path(save_dir, my_year, paste0(my_year, "_cohort_has_non_opioid_rx.rds")))
  out <- cohort |>
    left_join(disability_or_pain) |>
    left_join(race) |>
    left_join(alternative_treaments) |>
    left_join(non_opioid_rx) |>
    mutate(year = my_year) |>
    select(-c("washout_start_dt","followup_start_dt","followup_end_dt"))
}

cohort <- rbind(merge_cohort(2016),
                merge_cohort(2017),
                merge_cohort(2018),
                merge_cohort(2019))
setDT(cohort)

my_func <- function(data, which_race, which_year, which_pain_or_disability) {
  subset <- data[dem_race_cond == which_race &
                   year == which_year &
                   disability_pain_cal == which_pain_or_disability]
  
  num_bene <- nrow(subset)
  
  # subsetting down to just those who were prescribed an opioid during washout
  counselling_prop <- sum(subset$has_counseling)/num_bene
  counselling_se <- sqrt((counselling_prop * (1-counselling_prop))/num_bene)
  
  physical_therapy_prop <- sum(subset$has_physical_therapy)/num_bene
  physical_therapy_se <- sqrt((physical_therapy_prop * (1-physical_therapy_prop))/num_bene)
  
  acupuncture_prop <- sum(subset$has_acupuncture)/num_bene
  acupuncture_se <- sqrt((acupuncture_prop * (1-acupuncture_prop))/num_bene)
  
  chiropractic_prop <- sum(subset$has_chiropractic)/num_bene
  chiropractic_se <- sqrt((chiropractic_prop * (1-chiropractic_prop))/num_bene)
  
  nonopioid_pain_prop <- sum(subset$has_nonopioid_pain_rx)/num_bene
  nonopioid_pain_se <- sqrt((nonopioid_pain_prop * (1-nonopioid_pain_prop))/num_bene)
  
  # multimodal_treatment_prop <- sum(subset$has_multimodal_pain_treatment)/num_bene
  # multimodal_treatment_se <- sqrt((multimodal_treatment_prop * (1-multimodal_treatment_prop))/num_bene)
  
  
  return(c(which_race, 
           which_year, 
           which_pain_or_disability,
           num_bene,
           counselling_prop,
           counselling_se,
           physical_therapy_prop,
           physical_therapy_se,
           acupuncture_prop,
           acupuncture_se,
           chiropractic_prop,
           chiropractic_se,
           nonopioid_pain_prop,
           nonopioid_pain_se
           # multimodal_treatment_prop,
           # multimodal_treatment_se
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
                       "counselling_prop",
                       "counselling_se",
                       "physical_therapy_prop",
                       "physical_therapy_se",
                       "acupuncture_prop",
                       "acupuncture_se",
                       "chiropractic_prop",
                       "chiropractic_se",
                       "nonopioid_pain_prop",
                       "nonopioid_pain_se"
                       # "multimodal_treatment_prop",
                       # "multimodal_treatment_se"
)

results[, -c(1,3)] <- lapply(results[, -c(1,3)], as.numeric)

write.csv(results, file.path(result_dir, "additional_outcomes_results.csv"))

results <- read.csv(file.path(result_dir, "additional_outcomes_results.csv"))
my_palette <- c("#b2182b","#ef8a62","#fddbc7","#d1e5f0","#67a9cf","#2166ac")

# Cleaning race names
races <- data.frame(race_ethnicity = c("multi_or_na","White, non-Hispanic","Black, non-Hispanic","Hispanic, all races","AIAN_or_HPI","Asian, non-Hispanic"),
                    race_name = c("Multiracial or missing","White, non-Hispanic","Black, non-Hispanic","Hispanic, all races","AIAN or HPI","Asian, non-Hispanic"))

results <- results |>
  left_join(races) |>
  select(-X, -race_ethnicity, race_ethnicity=race_name)

p <- ggplot(results, aes(x = year, y=counselling_prop, color = race_ethnicity)) +
  geom_jitter(position=position_dodge(0.2)) +
  geom_line(aes(group = race_ethnicity), position=position_dodge(0.2)) +
  # geom_smooth()+
  # ggtitle("Proportion of beneficiaries receiving counseling") +
  ylab("Proportion (95% CI)") +
  facet_wrap(~ str_to_title(factor(pain_or_disability, levels = c("chronic pain only", "disability only", "disability and chronic pain", "neither")))) +
  scale_color_manual(values = my_palette) +
  geom_errorbar(aes(ymin = counselling_prop - 1.96*counselling_se, 
                    ymax = counselling_prop + 1.96*counselling_se),
                width = 1, position = position_dodge(0.2)) +
  labs(color = 'Race and Ethnicity') +
  theme_light() +
  theme(strip.text = element_text(color = "black")) # Set facet text to black
p
ggsave(file = file.path(result_dir, "counselling_prop.pdf"), width = 6, height = 4.5)


p <- ggplot(results, aes(x = year, y=physical_therapy_prop, color = race_ethnicity)) +
  geom_jitter(position=position_dodge(0.2)) +
  geom_line(aes(group = race_ethnicity), position=position_dodge(0.2)) +
  # geom_smooth()+
  # ggtitle("Proportion of beneficiaries receiving physical therapy") +
  ylab("Proportion (95% CI)") +
  facet_wrap(~ str_to_title(factor(pain_or_disability, levels = c("chronic pain only", "disability only", "disability and chronic pain", "neither")))) +
  scale_color_manual(values = my_palette) +
  geom_errorbar(aes(ymin = physical_therapy_prop - 1.96*physical_therapy_se, 
                    ymax = physical_therapy_prop + 1.96*physical_therapy_se),
                width = 1, position = position_dodge(0.2)) +
  labs(color = 'Race and Ethnicity') +
  theme_light() +
  theme(strip.text = element_text(color = "black")) # Set facet text to black
p
ggsave(file = file.path(result_dir, "physical_therapy_prop.pdf"), width = 6, height = 4.5)



p <- ggplot(results, aes(x = year, y=acupuncture_prop, color = race_ethnicity)) +
  geom_jitter(position=position_dodge(0.2)) +
  geom_line(aes(group = race_ethnicity), position=position_dodge(0.2)) +
  # geom_smooth()+
  # ggtitle("Proportion of beneficiaries receiving acupuncture") +
  ylab("Proportion (95% CI)") +
  facet_wrap(~ str_to_title(factor(pain_or_disability, levels = c("chronic pain only", "disability only", "disability and chronic pain", "neither")))) +
  scale_color_manual(values = my_palette) +
  geom_errorbar(aes(ymin = pmax(acupuncture_prop - 1.96*acupuncture_se, 0), 
                    ymax = acupuncture_prop + 1.96*acupuncture_se),
                width = 1, position = position_dodge(0.2)) +
  labs(color = 'Race and Ethnicity') +
  theme_light() +
  theme(strip.text = element_text(color = "black")) # Set facet text to black
p
ggsave(file = file.path(result_dir, "acupuncture_prop.pdf"), width = 6, height = 4.5)



p <- ggplot(results, aes(x = year, y=chiropractic_prop, color = race_ethnicity)) +
  geom_jitter(position=position_dodge(0.2)) +
  geom_line(aes(group = race_ethnicity), position=position_dodge(0.2)) +
  # geom_smooth()+
  # ggtitle("Proportion of beneficiaries receiving chiropractic work") +
  ylab("Proportion (95% CI)") +
  facet_wrap(~ str_to_title(factor(pain_or_disability, levels = c("chronic pain only", "disability only", "disability and chronic pain", "neither")))) +
  scale_color_manual(values = my_palette) +
  geom_errorbar(aes(ymin = pmax(chiropractic_prop - 1.96*chiropractic_se, 0), 
                    ymax = chiropractic_prop + 1.96*chiropractic_se),
                width = 1, position = position_dodge(0.2)) +
  labs(color = 'Race and Ethnicity') +
  theme_light() +
  theme(strip.text = element_text(color = "black")) # Set facet text to black
p
ggsave(file = file.path(result_dir, "chiropractic_prop.pdf"), width = 6, height = 4.5)




p <- ggplot(results, aes(x = year, y=nonopioid_pain_prop, color = race_ethnicity)) +
  geom_jitter(position=position_dodge(0.2)) +
  geom_line(aes(group = race_ethnicity), position=position_dodge(0.2)) +
  # geom_smooth()+
  # ggtitle("Proportion of beneficiaries receiving non-opioid pain medication") +
  ylab("Proportion (95% CI)") +
  facet_wrap(~ str_to_title(factor(pain_or_disability, levels = c("chronic pain only", "disability only", "disability and chronic pain", "neither")))) +
  scale_color_manual(values = my_palette) +
  geom_errorbar(aes(ymin = pmax(nonopioid_pain_prop - 1.96*nonopioid_pain_se, 0), 
                    ymax = nonopioid_pain_prop + 1.96*nonopioid_pain_se),
                width = 1, position = position_dodge(0.2)) +
  labs(color = 'Race and Ethnicity') +
  theme_light() +
  theme(strip.text = element_text(color = "black")) # Set facet text to black
p
ggsave(file = file.path(result_dir, "nonopioid_pain_prop.pdf"), width = 6, height = 4.5)




# p <- ggplot(results, aes(x = year, y=multimodal_treatment_prop, color = race_ethnicity)) +
#   geom_jitter(position=position_dodge(0.2)) +
#   geom_line(aes(group = race_ethnicity), position=position_dodge(0.2)) +
#   # geom_smooth()+
#   ggtitle("Proportion of beneficiaries receiving multimodal treatment for pain, by pain/disability status, race/ethnicity, and year") +
#   ylab("proportion") +
#   facet_wrap(~factor(pain_or_disability, levels = c("chronic pain only","disability only","disability and chronic pain","neither"))) +
#   geom_errorbar(aes(ymin = pmax(multimodal_treatment_prop - 1.96*multimodal_treatment_se, 0), 
#                     ymax = multimodal_treatment_prop + 1.96*multimodal_treatment_se),
#                 width = 1, position = position_dodge(0.2))
# p
# 
# ggsave(file = file.path(result_dir, "multimodal_treatment_prop.pdf"), width = 10, height = 7)