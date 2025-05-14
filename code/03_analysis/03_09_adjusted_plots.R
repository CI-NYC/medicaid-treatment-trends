# -------------------------------------
# Script:
# Author:
# Purpose:
# Notes:
# -------------------------------------
library(glue)
library(tidyverse)

save_dir <- "/mnt/general-data/disability/post_surgery_opioid_use/tmp"
result_dir <- "~/medicaid/medicaid-treatment-trends/output/adjusted_plots"
my_palette <- c("#b2182b","#ef8a62","#fddbc7","#67a9cf","#2166ac","#053061")

years <- c(2016:2019)
pain_groups <- c("chronic pain only", "disability and chronic pain")
# pain_groups <- c("chronic pain only", "disability only", "disability and chronic pain", "neither")

race_names <- data.frame(race_ethnicity = c("race_white", "race_black", "race_asian", "race_hispanic", "race_multi_na", "race_aian_hpi"),
                         dem_race_cond = c("White, non-Hispanic", "Black, non-Hispanic", "Asian, non-Hispanic", "Hispanic, all races", "Multiracial or Missing", "AIAN or HPI"))

df <- data.frame()
for (my_year in years){
  for (pain in pain_groups){
    df <- rbind(df, readRDS(file.path(save_dir, my_year, glue("{my_year}_{pain}_mlr3_predictions.rds"))))
  }
}


  
df <- df |>
  group_by(year,pain_or_disability,race_ethnicity) |>
  summarise(has_opioid = mean(has_opioid),
            has_nonopioid_pain_rx = mean(has_nonopioid_pain_rx),
            has_any_pharma = mean(has_any_pharma),
            mean_daily_dose_mme_outpatient = mean(mean_daily_dose_mme_outpatient),
            opioid_days_covered_outpatient = mean(opioid_days_covered_outpatient),
            has_acupuncture = mean(has_acupuncture),
            has_physical_therapy = mean(has_physical_therapy),
            has_chiropractic = mean(has_chiropractic),
            has_counseling = mean(has_counseling),
            has_acu_chiro = mean(has_acu_chiro),
            has_intervention = mean(has_intervention),
            has_any_treatment = mean(has_any_treatment)) |>
  
  left_join(race_names)

results <- df |>
  select(-race_ethnicity, race_ethnicity=dem_race_cond)

plot_function <- function(df, treatment_category, title, ylabel){
  p <- ggplot(df, aes(x = year, y=!!sym(treatment_category), color = race_ethnicity)) +
    geom_jitter(position=position_dodge(0.2)) +
    geom_line(aes(group = race_ethnicity), position=position_dodge(0.2)) +
    # ggtitle(title) +
    ylab(ylabel) +
    facet_wrap(~ str_to_title(factor(pain_or_disability, levels = c("chronic pain only", "disability and chronic pain"))), ncol=1) +
    scale_color_manual(values = my_palette) +
    labs(color = 'Race and Ethnicity') +
    # ylim(0,0.16) +
    theme_light() +
    theme(strip.text = element_text(color = "black")) # Set facet text to black
  p
  ggsave(file = file.path(result_dir, paste0(treatment_category,".pdf")), width = 4.5, height = 4.9)
}


# plot_function(results,"has_opioid","Proportion of beneficiaries who were prescribed an opioid","Proportion (95% CI)")

plot_function(results,"has_any_treatment","Average daily MME","Proportion")

plot_function(results,"mean_daily_dose_mme_outpatient","Average daily MME","Mean daily MME")

plot_function(results,"opioid_days_covered_outpatient","Average proportion of days covered for opioids","Proportion days covered")

plot_function(results,"has_counseling","Proportion of beneficiaries receiving counseling","Proportion")

# plot_function(results,"has_nonopioid_pain_rx","Proportion of beneficiaries receiving non-opioid pain medication","Proportion")

# plot_function(results,"has_acupuncture","Proportion of beneficiaries receiving acupuncture","Proportion")

plot_function(results,"has_physical_therapy","Proportion of beneficiaries receiving physical therapy","Proportion")

# plot_function(results,"has_chiropractic","Proportion of beneficiaries receiving chiropractic work","Proportion")

plot_function(results,"has_intervention","Proportion of beneficiaries receiving physical therapy","Proportion")



##### Proportion opioid and non-opioid

p1 <- ggplot(results, aes(x = year, y=has_any_pharma, color = race_ethnicity)) +
  geom_jitter(position=position_dodge(0.2)) +
  geom_line(aes(group = race_ethnicity), position=position_dodge(0.2)) +
  facet_wrap(~factor(pain_or_disability,levels = c("chronic pain only","disability and chronic pain")),ncol=1) +
  scale_color_manual(values = my_palette) +
  ggtitle("Any pharmacologic") +
  labs(x=NULL,y=NULL,color='Race and Ethnicity') +
  ylim(0.15,1) +
  theme_light() +
  theme(strip.text = element_text(color = "black"),
        plot.title = element_text(size = 10, face = "bold"))

legend <- g_legend(p1)
p1 <- p1 + theme(legend.position = "none")

p2 <- ggplot(results, aes(x = year, y=has_opioid, color = race_ethnicity)) +
  geom_jitter(position=position_dodge(0.2)) +
  geom_line(aes(group = race_ethnicity), position=position_dodge(0.2)) +
  facet_wrap(~factor(pain_or_disability,levels = c("chronic pain only","disability and chronic pain")),ncol=1) +
  scale_color_manual(values = my_palette) +
  ggtitle("Opioid") +
  labs(x=NULL,y=NULL,color='Race and Ethnicity') +
  ylim(0.15,1) +
  theme_light() +
  theme(strip.text = element_text(color = "black"),
        legend.position = "none",
        plot.title = element_text(size = 10, face = "bold"))

p3 <- ggplot(results, aes(x = year, y=has_nonopioid_pain_rx, color = race_ethnicity)) +
  geom_jitter(position=position_dodge(0.2)) +
  geom_line(aes(group = race_ethnicity), position=position_dodge(0.2)) +
  facet_wrap(~factor(pain_or_disability,levels = c("chronic pain only","disability and chronic pain")),ncol=1) +
  scale_color_manual(values = my_palette) +
  ggtitle("Non-Opioid") +
  labs(x=NULL,y=NULL,color='Race and Ethnicity') +
  ylim(0.15,1) +
  theme_light() +
  theme(strip.text = element_text(color = "black"),
        legend.position = "none",
        plot.title = element_text(size = 10, face = "bold"))

combined_plots <- arrangeGrob(
  p1, p2, p3,
  ncol = 3
)
pdf("~/medicaid/medicaid-treatment-trends/output/adjusted_plots/pharma_prop.pdf", width = 10, height = 5)
grid.arrange(
  arrangeGrob(
    combined_plots,
    legend,
    ncol = 2,
    widths = unit(c(3, 1), "null")  # Adjust these numbers to give more space to either the plots or the legend.
  ),
  left = textGrob("Proportion", rot = 90, gp = gpar(fontsize = 12)),
  bottom = textGrob("Year", gp = gpar(fontsize = 12))
)
dev.off()


###### Acupuncture and chiropractic

p1 <- ggplot(results, aes(x = year, y=has_acu_chiro, color = race_ethnicity)) +
  geom_jitter(position=position_dodge(0.2)) +
  geom_line(aes(group = race_ethnicity), position=position_dodge(0.2)) +
  facet_wrap(~factor(pain_or_disability,levels = c("chronic pain only","disability and chronic pain")),ncol=1) +
  scale_color_manual(values = my_palette) +
  ggtitle("Chiropractic or acupuncture") +
  labs(x=NULL,y=NULL,color='Race and Ethnicity') +
  ylim(0,0.125) +
  theme_light() +
  theme(strip.text = element_text(color = "black"),
        plot.title = element_text(size = 10, face = "bold"),
        legend.position = "none")

p2 <- ggplot(results, aes(x = year, y=has_chiropractic, color = race_ethnicity)) +
  geom_jitter(position=position_dodge(0.2)) +
  geom_line(aes(group = race_ethnicity), position=position_dodge(0.2)) +
  facet_wrap(~factor(pain_or_disability,levels = c("chronic pain only","disability and chronic pain")),ncol=1) +
  scale_color_manual(values = my_palette) +
  ggtitle("Chiropractic") +
  labs(x=NULL,y=NULL,color='Race and Ethnicity') +
  ylim(0,0.125) +
  theme_light() +
  theme(strip.text = element_text(color = "black"),
        plot.title = element_text(size = 10, face = "bold"),
        legend.position = "none")

p3 <- ggplot(results, aes(x = year, y=has_acupuncture, color = race_ethnicity)) +
  geom_jitter(position=position_dodge(0.2)) +
  geom_line(aes(group = race_ethnicity), position=position_dodge(0.2)) +
  facet_wrap(~factor(pain_or_disability,levels = c("chronic pain only","disability and chronic pain")),ncol=1) +
  scale_color_manual(values = my_palette) +
  ggtitle("Acupuncture") +
  labs(x=NULL,y=NULL,color='Race and Ethnicity') +
  ylim(0,0.125) +
  theme_light() +
  theme(strip.text = element_text(color = "black"),
        plot.title = element_text(size = 10, face = "bold"),
        legend.position = "none")

combined_plots <- arrangeGrob(
  p1, p2, p3,
  ncol = 3
)
pdf("~/medicaid/medicaid-treatment-trends/output/adjusted_plots/acu_chiro_prop.pdf", width = 10, height = 5)
grid.arrange(
  arrangeGrob(
    combined_plots,
    legend,
    ncol = 2,
    widths = unit(c(3, 1), "null")  # Adjust these numbers to give more space to either the plots or the legend.
  ),
  left = textGrob("Proportion", rot = 90, gp = gpar(fontsize = 12)),
  bottom = textGrob("Year", gp = gpar(fontsize = 12))
)
dev.off()

