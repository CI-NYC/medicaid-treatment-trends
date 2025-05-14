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
library(grid)
library(gridExtra)
library(gtable)

save_dir <- "/mnt/general-data/disability/post_surgery_opioid_use/tmp"
result_dir <- "~/medicaid/medicaid-treatment-trends/output"

merge_cohort <- function(my_year){
  cohort <- readRDS(file.path(save_dir, my_year, paste0("cohort_",my_year,"_pain_only.rds")))
  race <- readRDS(file.path(save_dir, my_year, paste0("cohort_",my_year,"_race.rds"))) |>
    mutate(dem_race_cond = ifelse(dem_race_cond %in% c("Multiracial, non-Hispanic", "Missing"), "Multiracial or Missing", dem_race_cond))
  
  opioid <- readRDS(file.path(save_dir, my_year, paste0(my_year, "_mean_daily_dose_mme_outpatient.rds"))) |>
    mutate(has_opioid_outpatient = as.numeric(mean_daily_dose_mme_outpatient>0)) |>
    select(BENE_ID, has_opioid_outpatient)
  
  non_opioid_pharma <- readRDS(file.path(save_dir, my_year, paste0(my_year, "_cohort_has_non_opioid_rx.rds"))) |>
    mutate(has_nonopioid_rx_outpatient = as.numeric(has_nonopioid_pain_rx_otl_opatient + has_nonopioid_pain_rx_rxl >0)) |>
    select(BENE_ID, has_nonopioid_rx_outpatient)
  
  physical_therapy <- readRDS(file.path(save_dir, my_year, paste0(my_year, "_cohort_has_physical_therapy.rds"))) |>
    mutate(has_physical_therapy_acute = as.numeric(has_physical_therapy_iph + has_physical_therapy_otl_acute > 0),
           has_physical_therapy_outpatient = has_physical_therapy_otl_outpatient) |>
    select(BENE_ID, has_physical_therapy_acute, has_physical_therapy_outpatient)
  
  counseling <- readRDS(file.path(save_dir, my_year, paste0(my_year, "_cohort_has_counseling.rds"))) |>
    mutate(has_counseling_acute = as.numeric(has_counseling_iph + has_counseling_otl_acute > 0),
           has_counseling_outpatient = has_counseling_otl_outpatient) |>
    select(BENE_ID, has_counseling_acute, has_counseling_outpatient)
  
  chiropractic <- readRDS(file.path(save_dir, my_year, paste0(my_year, "_cohort_has_chiropractic.rds"))) |>
    mutate(has_chiropractic_acute = as.numeric(has_chiropractic_iph + has_chiropractic_otl_acute > 0),
           has_chiropractic_outpatient = has_chiropractic_otl_outpatient) |>
    select(BENE_ID, has_chiropractic_acute, has_chiropractic_outpatient)
  
  acupuncture <- readRDS(file.path(save_dir, my_year, paste0(my_year, "_cohort_has_acupuncture.rds"))) |>
    mutate(has_acupuncture_acute = as.numeric(has_acupuncture_iph + has_acupuncture_otl_acute > 0),
           has_acupuncture_outpatient = has_acupuncture_otl_outpatient) |>
    select(BENE_ID, has_acupuncture_acute, has_acupuncture_outpatient)
  
  intervention <- readRDS(file.path(save_dir, my_year, paste0(my_year, "_cohort_has_intervention.rds"))) |>
    mutate(has_intervention_acute = as.numeric(has_intervention_iph + has_intervention_otl_acute > 0),
           has_intervention_outpatient = has_intervention_otl_outpatient) |>
    select(BENE_ID, has_intervention_acute, has_intervention_outpatient)
  
  out <- cohort |>
    left_join(race) |>
    left_join(opioid) |>
    left_join(non_opioid_pharma) |>
    left_join(physical_therapy) |>
    left_join(counseling) |>
    left_join(chiropractic) |>
    left_join(acupuncture) |>
    left_join(intervention) |>
    mutate(year = my_year,
           has_any_nonopioid = as.numeric(has_physical_therapy_outpatient +
                                            has_counseling_outpatient +
                                            has_chiropractic_outpatient +
                                            has_acupuncture_outpatient +
                                            has_intervention_outpatient > 0),
           has_any_treatment = as.numeric(has_opioid_outpatient + has_nonopioid_rx_outpatient + has_any_nonopioid > 0))
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
  
  n_any_treatment <- sum(subset$has_any_treatment)
  n_any_nonopioid <- sum(subset$has_any_nonopioid)
  # n_physical_therapy_acute <- ifelse(sum(subset$has_physical_therapy_acute > 0) >= 11, sum(subset$has_physical_therapy_acute > 0), "*")
  n_physical_therapy_outpatient <- ifelse(sum(subset$has_physical_therapy_outpatient > 0) >= 11, sum(subset$has_physical_therapy_outpatient > 0), "*")
  # n_counseling_acute <- ifelse(sum(subset$has_counseling_acute > 0) >= 11, sum(subset$has_counseling_acute > 0), "*")
  n_counseling_outpatient <- ifelse(sum(subset$has_counseling_outpatient > 0) >= 11, sum(subset$has_counseling_outpatient > 0), "*")
  # n_acupuncture_acute <- ifelse(sum(subset$has_acupuncture_acute > 0) >= 11, sum(subset$has_acupuncture_acute > 0), "*")
  n_acupuncture_outpatient <- ifelse(sum(subset$has_acupuncture_outpatient > 0) >= 11, sum(subset$has_acupuncture_outpatient > 0), "*")
  # n_chiropractic_acute <- ifelse(sum(subset$has_chiropractic_acute > 0) >= 11, sum(subset$has_chiropractic_acute > 0), "*")
  n_chiropractic_outpatient <- ifelse(sum(subset$has_chiropractic_outpatient > 0) >= 11, sum(subset$has_chiropractic_outpatient > 0), "*")
  # n_intervention_acute <- ifelse(sum(subset$has_intervention_acute > 0) >= 11, sum(subset$has_intervention_acute > 0), "*")
  n_intervention_outpatient <- ifelse(sum(subset$has_intervention_outpatient > 0) >= 11, sum(subset$has_intervention_outpatient > 0), "*")
  
  
  any_treatment <- mean(subset$has_any_treatment)
  se_any_treatment <- sqrt((any_treatment * (1-any_treatment))/num_bene)

  # physical_therapy_prop_acute <- mean(subset$has_physical_therapy_acute)
  # se_physical_therapy_prop_acute <- sqrt((physical_therapy_prop_acute * (1-physical_therapy_prop_acute))/num_bene)
  
  physical_therapy_prop_outpatient <- mean(subset$has_physical_therapy_outpatient)
  se_physical_therapy_prop_outpatient <- sqrt((physical_therapy_prop_outpatient * (1-physical_therapy_prop_outpatient))/num_bene)
  
  # counseling_prop_acute <- mean(subset$has_counseling_acute)
  # se_counseling_prop_acute <- sqrt((counseling_prop_acute * (1-counseling_prop_acute))/num_bene)
  
  counseling_prop_outpatient <- mean(subset$has_counseling_outpatient)
  se_counseling_prop_outpatient <- sqrt((counseling_prop_outpatient * (1-counseling_prop_outpatient))/num_bene)
  
  # acupuncture_prop_acute <- mean(subset$has_acupuncture_acute)
  # se_acupuncture_prop_acute <- sqrt((acupuncture_prop_acute * (1-acupuncture_prop_acute))/num_bene)
  
  acupuncture_prop_outpatient <- mean(subset$has_acupuncture_outpatient)
  se_acupuncture_prop_outpatient <- sqrt((acupuncture_prop_outpatient * (1-acupuncture_prop_outpatient))/num_bene)
  
  # chiropractic_prop_acute <- mean(subset$has_chiropractic_acute)
  # se_chiropractic_prop_acute <- sqrt((chiropractic_prop_acute * (1-chiropractic_prop_acute))/num_bene)
  
  chiropractic_prop_outpatient <- mean(subset$has_chiropractic_outpatient)
  se_chiropractic_prop_outpatient <- sqrt((chiropractic_prop_outpatient * (1-chiropractic_prop_outpatient))/num_bene)
  
  acu_chiro_prop_outpatient <- mean(subset$has_chiropractic_outpatient | subset$has_acupuncture_outpatient)
  se_acu_chiro_prop_outpatient <- sqrt((acu_chiro_prop_outpatient * (1-acu_chiro_prop_outpatient))/num_bene)
  
  # intervention_prop_acute <- mean(subset$has_intervention_acute)
  # se_intervention_prop_acute <- sqrt((intervention_prop_acute * (1-intervention_prop_acute))/num_bene)
  
  intervention_prop_outpatient <- mean(subset$has_intervention_outpatient)
  se_intervention_prop_outpatient <- sqrt((intervention_prop_outpatient * (1-intervention_prop_outpatient))/num_bene)
  
  
  return(c(which_race, 
           which_year, 
           which_pain_or_disability,
           num_bene,
           n_any_treatment,
           n_any_nonopioid,
           any_treatment,
           se_any_treatment,
           # n_physical_therapy_acute,
           n_physical_therapy_outpatient,
           # n_counseling_acute,
           n_counseling_outpatient,
           # n_acupuncture_acute,
           n_acupuncture_outpatient,
           # n_chiropractic_acute,
           n_chiropractic_outpatient,
           # n_intervention_acute,
           n_intervention_outpatient,
           # physical_therapy_prop_acute,
           # se_physical_therapy_prop_acute,
           physical_therapy_prop_outpatient,
           se_physical_therapy_prop_outpatient,
           # counseling_prop_acute,
           # se_counseling_prop_acute,
           counseling_prop_outpatient,
           se_counseling_prop_outpatient,
           # acupuncture_prop_acute,
           # se_acupuncture_prop_acute,
           acupuncture_prop_outpatient,
           se_acupuncture_prop_outpatient,
           # chiropractic_prop_acute,
           # se_chiropractic_prop_acute,
           chiropractic_prop_outpatient,
           se_chiropractic_prop_outpatient,
           acu_chiro_prop_outpatient,
           se_acu_chiro_prop_outpatient,
           # intervention_prop_acute,
           # se_intervention_prop_acute,
           intervention_prop_outpatient,
           se_intervention_prop_outpatient
  ))
}

results <- data.frame()

for (i in c("White, non-Hispanic", 
            "Black, non-Hispanic", 
            "Hispanic, all races", 
            "Asian, non-Hispanic",
            "AIAN_or_HPI",
            "Multiracial or Missing")) {
  for (j in 2016:2019) {
    for (k in c("chronic pain only", "disability and chronic pain")){
      results <- rbind(results, my_func(cohort, i, j, k))
      
    }
  }
}

colnames(results) <- c("race_ethnicity",
                       "year",
                       "pain_or_disability",
                       "number_of_beneficiaries",
                       "n_any_treatment",
                       "n_any_nonopioid",
                       "any_treatment",
                       "se_any_treatment",
                       # "n_physical_therapy_acute",
                       "n_physical_therapy_outpatient",
                       # "n_counseling_acute",
                       "n_counseling_outpatient",
                       # "n_acupuncture_acute",
                       "n_acupuncture_outpatient",
                       # "n_chiropractic_acute",
                       "n_chiropractic_outpatient",
                       # "n_intervention_acute",
                       "n_intervention_outpatient",
                       # "physical_therapy_prop_acute",
                       # "se_physical_therapy_prop_acute",
                       "physical_therapy_prop_outpatient",
                       "se_physical_therapy_prop_outpatient",
                       # "counseling_prop_acute",
                       # "se_counseling_prop_acute",
                       "counseling_prop_outpatient",
                       "se_counseling_prop_outpatient",
                       # "acupuncture_prop_acute",
                       # "se_acupuncture_prop_acute",
                       "acupuncture_prop_outpatient",
                       "se_acupuncture_prop_outpatient",
                       # "chiropractic_prop_acute",
                       # "se_chiropractic_prop_acute",
                       "chiropractic_prop_outpatient",
                       "se_chiropractic_prop_outpatient",
                       "acu_chiro_prop_outpatient",
                       "se_acu_chiro_prop_outpatient",
                       # "intervention_prop_acute",
                       # "se_intervention_prop_acute",
                       "intervention_prop_outpatient",
                       "se_intervention_prop_outpatient"
)

# results <- results |>
#   mutate(
#     across(contains("physical_therapy_prop_acute"), ~ ifelse(n_physical_therapy_acute == "*", NA, .)),
#     across(contains("counseling_prop_acute"), ~ ifelse(n_counseling_acute == "*", NA, .)),
#     across(contains("acupuncture_prop_acute"), ~ ifelse(n_acupuncture_acute == "*", NA, .)),
#     across(contains("acupuncture_prop_outpatient"), ~ ifelse(n_acupuncture_outpatient == "*", NA, .)),
#     across(contains("chiropractic_prop_acute"), ~ ifelse(n_chiropractic_acute == "*", NA, .)),
#     across(contains("intervention_prop_acute"), ~ ifelse(n_intervention_acute == "*", NA, .))
#   )

write.csv(results, file.path(result_dir, "additional_outcomes_results_25_05_13.csv"))




# PLOTTING ############################################

results <- read.csv(file.path(result_dir, "additional_outcomes_results_25_05_13.csv"))
my_palette <- c("#b2182b","#ef8a62","#fddbc7","#67a9cf","#2166ac","#053061")

# Cleaning race names
races <- data.frame(race_ethnicity = c("White, non-Hispanic","Black, non-Hispanic","Hispanic, all races","Asian, non-Hispanic","AIAN_or_HPI","Multiracial or Missing"),
                    race_name = c("White, non-Hispanic","Black, non-Hispanic","Hispanic, all races","Asian, non-Hispanic","AIAN or HPI","Multiracial or Missing"))

results <- results |>
  left_join(races) |>
  select(-X, -race_ethnicity, race_ethnicity=race_name)

g_legend <- function(a.gplot) {
  # Convert plot to grob
  tmp <- ggplotGrob(a.gplot)
  # Extract the legend (guide-box)
  leg <- gtable_filter(tmp, "guide-box")
  return(leg)
}

p <- ggplot(results, aes(x = year, y=any_treatment, color = race_ethnicity)) +
  geom_jitter(position=position_dodge(0.2)) +
  geom_line(aes(group = race_ethnicity), position=position_dodge(0.2)) +
  facet_wrap(~factor(pain_or_disability,levels = c("chronic pain only","disability and chronic pain")),ncol=1) +
  geom_errorbar(aes(ymin = any_treatment - 1.96*se_any_treatment, 
                    ymax = any_treatment + 1.96*se_any_treatment),
                width = 1, position = position_dodge(0.2)) +
  scale_color_manual(values = my_palette) +
  labs(color='Race and Ethnicity') +
  ylab("Proportion (95% CI)") +
  xlab("Year") +
  # ylim(0,1) +
  theme_light() +
  theme(strip.text = element_text(color = "black"),
        plot.title = element_text(size = 10, face = "bold"))

ggsave(file = file.path(result_dir, "any_treatment_prop.pdf"), p, width = 4.5, height = 4.9)

p <- ggplot(results, aes(x = year, y=physical_therapy_prop_outpatient, color = race_ethnicity)) +
  geom_jitter(position=position_dodge(0.2)) +
  geom_line(aes(group = race_ethnicity), position=position_dodge(0.2)) +
  facet_wrap(~factor(pain_or_disability,levels = c("chronic pain only","disability and chronic pain")),ncol=1) +
  geom_errorbar(aes(ymin = physical_therapy_prop_outpatient - 1.96*se_physical_therapy_prop_outpatient, 
                    ymax = physical_therapy_prop_outpatient + 1.96*se_physical_therapy_prop_outpatient),
                width = 1, position = position_dodge(0.2)) +
  scale_color_manual(values = my_palette) +
  labs(color='Race and Ethnicity') +
  ylab("Proportion (95% CI)") +
  xlab("Year") +
  # ylim(0,1) +
  theme_light() +
  theme(strip.text = element_text(color = "black"),
        plot.title = element_text(size = 10, face = "bold"))

ggsave(file = file.path(result_dir, "physical_therapy_prop.pdf"), p, width = 4.5, height = 4.9)


p <- ggplot(results, aes(x = year, y=counseling_prop_outpatient, color = race_ethnicity)) +
  geom_jitter(position=position_dodge(0.2)) +
  geom_line(aes(group = race_ethnicity), position=position_dodge(0.2)) +
  facet_wrap(~factor(pain_or_disability,levels = c("chronic pain only","disability and chronic pain")),ncol=1) +
  geom_errorbar(aes(ymin = counseling_prop_outpatient - 1.96*se_counseling_prop_outpatient, 
                    ymax = counseling_prop_outpatient + 1.96*se_counseling_prop_outpatient),
                width = 1, position = position_dodge(0.2)) +
  scale_color_manual(values = my_palette) +
  labs(color='Race and Ethnicity') +
  ylab("Proportion (95% CI)") +
  xlab("Year") +
  # ylim(0,1) +
  theme_light() +
  theme(strip.text = element_text(color = "black"),
        plot.title = element_text(size = 10, face = "bold"))

ggsave(file = file.path(result_dir, "counseling_prop.pdf"), p, width = 4.5, height = 4.9)


# Interventions - acute vs outpatient

p1 <- ggplot(results, aes(x = year, y=intervention_prop_outpatient, color = race_ethnicity)) +
  geom_jitter(position=position_dodge(0.2)) +
  geom_line(aes(group = race_ethnicity), position=position_dodge(0.2)) +
  facet_wrap(~factor(pain_or_disability,levels = c("chronic pain only","disability and chronic pain")),ncol=1) +
  geom_errorbar(aes(ymin = intervention_prop_outpatient - 1.96*se_intervention_prop_outpatient, 
                    ymax = intervention_prop_outpatient + 1.96*se_intervention_prop_outpatient),
                width = 1, position = position_dodge(0.2)) +
  scale_color_manual(values = my_palette) +
  # ggtitle("Outpatient") +
  labs(color='Race and Ethnicity') +
  ylab("Proportion (95% CI)") +
  xlab("Year") +
  # ylim(0,0.16) +
  theme_light() +
  theme(strip.text = element_text(color = "black"),
        plot.title = element_text(size = 10, face = "bold"))

# legend <- g_legend(p1)
# p1 <- p1 + theme(legend.position = "none")
# 
# p2 <- ggplot(results, aes(x = year, y=intervention_prop_acute, color = race_ethnicity)) +
#   geom_jitter(position=position_dodge(0.2)) +
#   geom_line(aes(group = race_ethnicity), position=position_dodge(0.2)) +
#   facet_wrap(~factor(pain_or_disability,levels = c("chronic pain only","disability and chronic pain")),ncol=1) +
#   geom_errorbar(aes(ymin = intervention_prop_acute - 1.96*se_intervention_prop_acute, 
#                     ymax = intervention_prop_acute + 1.96*se_intervention_prop_acute),
#                 width = 1, position = position_dodge(0.2)) +
#   scale_color_manual(values = my_palette) +
#   ggtitle("Acute") +
#   labs(x=NULL,y=NULL,color='Race and Ethnicity') +
#   ylim(0,0.16) +
#   theme_light() +
#   theme(strip.text = element_text(color = "black"),
#         plot.title = element_text(size = 10, face = "bold"),
#         legend.position = "none")

# combined_plots <- arrangeGrob(
#   p1, p2,
#   ncol = 2
# )
pdf("~/medicaid/medicaid-treatment-trends/output/intervention_prop.pdf", width = 4.5, height = 4.9)
# grid.arrange(
#   arrangeGrob(
#     combined_plots,
#     legend,
#     ncol = 2,
#     widths = unit(c(2, 1), "null")  # Adjust these numbers to give more space to either the plots or the legend.
#   ),
#   left = textGrob("Proportion (95% CI)", rot = 90, gp = gpar(fontsize = 12)),
#   bottom = textGrob("Year", gp = gpar(fontsize = 12))
# )
p1
dev.off()



p1 <- ggplot(results, aes(x = year, y=acu_chiro_prop_outpatient, color = race_ethnicity)) +
  geom_jitter(position=position_dodge(0.2)) +
  geom_line(aes(group = race_ethnicity), position=position_dodge(0.2)) +
  facet_wrap(~factor(pain_or_disability,levels = c("chronic pain only","disability and chronic pain")),ncol=1) +
  geom_errorbar(aes(ymin = acu_chiro_prop_outpatient - 1.96*se_acu_chiro_prop_outpatient, 
                    ymax = acu_chiro_prop_outpatient + 1.96*se_acu_chiro_prop_outpatient),
                width = 1, position = position_dodge(0.2)) +
  scale_color_manual(values = my_palette) +
  ggtitle("Chiropractic or acupuncture") +
  labs(x=NULL,y=NULL,color='Race and Ethnicity') +
  ylim(0,0.16) +
  theme_light() +
  theme(strip.text = element_text(color = "black"),
        plot.title = element_text(size = 10, face = "bold"),
        legend.position = "none")

p2 <- ggplot(results, aes(x = year, y=chiropractic_prop_outpatient, color = race_ethnicity)) +
  geom_jitter(position=position_dodge(0.2)) +
  geom_line(aes(group = race_ethnicity), position=position_dodge(0.2)) +
  facet_wrap(~factor(pain_or_disability,levels = c("chronic pain only","disability and chronic pain")),ncol=1) +
  geom_errorbar(aes(ymin = chiropractic_prop_outpatient - 1.96*se_chiropractic_prop_outpatient, 
                    ymax = chiropractic_prop_outpatient + 1.96*se_chiropractic_prop_outpatient),
                width = 1, position = position_dodge(0.2)) +
  scale_color_manual(values = my_palette) +
  ggtitle("Chiropractic") +
  labs(x=NULL,y=NULL,color='Race and Ethnicity') +
  ylim(0,0.16) +
  theme_light() +
  theme(strip.text = element_text(color = "black"),
        plot.title = element_text(size = 10, face = "bold"),
        legend.position = "none")

p3 <- ggplot(results, aes(x = year, y=acupuncture_prop_outpatient, color = race_ethnicity)) +
  geom_jitter(position=position_dodge(0.2)) +
  geom_line(aes(group = race_ethnicity), position=position_dodge(0.2)) +
  facet_wrap(~factor(pain_or_disability,levels = c("chronic pain only","disability and chronic pain")),ncol=1) +
  geom_errorbar(aes(ymin = acupuncture_prop_outpatient - 1.96*se_acupuncture_prop_outpatient, 
                    ymax = acupuncture_prop_outpatient + 1.96*se_acupuncture_prop_outpatient),
                width = 1, position = position_dodge(0.2)) +
  scale_color_manual(values = my_palette) +
  ggtitle("Acupuncture") +
  labs(x=NULL,y=NULL,color='Race and Ethnicity') +
  ylim(0,0.16) +
  theme_light() +
  theme(strip.text = element_text(color = "black"),
        plot.title = element_text(size = 10, face = "bold"),
        legend.position = "none")

combined_plots <- arrangeGrob(
  p1, p2, p3,
  ncol = 3
)
pdf("~/medicaid/medicaid-treatment-trends/output/acu_chiro_prop.pdf", width = 10, height = 5)
grid.arrange(
  arrangeGrob(
    combined_plots,
    legend,
    ncol = 2,
    widths = unit(c(3, 1), "null")  # Adjust these numbers to give more space to either the plots or the legend.
  ),
  left = textGrob("Proportion (95% CI)", rot = 90, gp = gpar(fontsize = 12)),
  bottom = textGrob("Year", gp = gpar(fontsize = 12))
)
dev.off()
