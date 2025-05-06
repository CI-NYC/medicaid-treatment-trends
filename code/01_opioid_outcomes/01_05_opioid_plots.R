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
library(stringr)
library(grid)
library(gridExtra)
library(gtable)

save_dir <- "/mnt/general-data/disability/post_surgery_opioid_use/tmp"
result_dir <- "~/medicaid/medicaid-treatment-trends/output"

race <- readRDS("/mnt/general-data/disability/create_cohort/final/desc_cohort.rds") |>
  select(BENE_ID, dem_race_cond)

# dems <- open_dataset("/mnt/general-data/disability/create_cohort/intermediate/tafdebse/dem_df.parquet")
# dems <-
#   dems |> 
#   select(all_of(names(dems)))  |>
#   collect()

# Merging variables to each cohort
merge_cohort <- function(my_year){
  cohort <- readRDS(file.path(save_dir, my_year, paste0("cohort_",my_year,"_pain_only.rds")))
  mme_acute <- readRDS(file.path(save_dir, my_year, paste0(my_year, "_mean_daily_dose_mme_acute.rds")))
  mme_outpatient <- readRDS(file.path(save_dir, my_year, paste0(my_year, "_mean_daily_dose_mme_outpatient.rds")))
  proportion_days_covered_acute <- readRDS(file.path(save_dir, my_year, paste0(my_year, "_proportion_days_covered_acute.rds")))
  proportion_days_covered_outpatient <- readRDS(file.path(save_dir, my_year, paste0(my_year, "_proportion_days_covered_outpatient.rds")))
  nonopioid_rx <- readRDS(file.path(save_dir, my_year, paste0(my_year, "_cohort_has_non_opioid_rx.rds"))) |>
    mutate(has_nonopioid_pain_rx_acute = as.numeric(has_nonopioid_pain_rx_ipl + has_nonopioid_pain_rx_otl_acute > 0),
           has_nonopioid_pain_rx_outpatient = as.numeric(has_nonopioid_pain_rx_otl_opatient + has_nonopioid_pain_rx_rxl > 0))
  
  out <- cohort |>
    left_join(race) |>
    left_join(mme_acute) |>
    left_join(mme_outpatient) |>
    left_join(proportion_days_covered_acute) |>
    left_join(proportion_days_covered_outpatient) |>
    left_join(nonopioid_rx) |>
    mutate(year = my_year)
}

cohort <- rbind(merge_cohort(2016),
                merge_cohort(2017),
                merge_cohort(2018),
                merge_cohort(2019))
setDT(cohort)

cohort$mean_daily_dose_mme_acute <- pmin(cohort$mean_daily_dose_mme_acute,100)
cohort$mean_daily_dose_mme_outpatient <- pmin(cohort$mean_daily_dose_mme_outpatient,100)

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
  
  n_opioid_acute <- ifelse(sum(subset$opioid_days_covered_acute > 0) >= 11, sum(subset$opioid_days_covered_acute > 0), "*")
  n_opioid_outpatient <- ifelse(sum(subset$opioid_days_covered_outpatient > 0) >= 11, sum(subset$opioid_days_covered_outpatient > 0), "*")
  n_nonopioid_acute <- ifelse(sum(subset$has_nonopioid_pain_rx_acute > 0) >= 11, sum(subset$has_nonopioid_pain_rx_acute > 0), "*")
  n_nonopioid_outpatient <- ifelse(sum(subset$has_nonopioid_pain_rx_outpatient > 0) >= 11, sum(subset$has_nonopioid_pain_rx_outpatient > 0), "*")
  
  # subsetting down to just those who were prescribed an opioid during washout
  subset_acute <- subset[opioid_days_covered_acute > 0]
  subset_outpatient <- subset[opioid_days_covered_outpatient > 0]
  
  pharma_prop_acute <- mean(subset$opioid_days_covered_acute>0 | subset$has_nonopioid_pain_rx_acute)
  se_pharma_prop_acute <- sqrt((pharma_prop_acute * (1-pharma_prop_acute))/num_bene)
  
  pharma_prop_outpatient <- nrow(subset_outpatient)/num_bene
  se_pharma_prop_outpatient <- sqrt((pharma_prop_outpatient * (1-pharma_prop_outpatient))/num_bene)
  
  opioid_prop_acute <- nrow(subset_acute)/num_bene
  se_opioid_prop_acute <- sqrt((opioid_prop_acute * (1-opioid_prop_acute))/num_bene)
  
  opioid_prop_outpatient <- nrow(subset_outpatient)/num_bene
  se_opioid_prop_outpatient <- sqrt((opioid_prop_outpatient * (1-opioid_prop_outpatient))/num_bene)
  
  nonopioid_prop_acute <- mean(subset$has_nonopioid_pain_rx_acute)
  se_nonopioid_prop_acute <- sqrt((nonopioid_prop_acute * (1-nonopioid_prop_acute))/num_bene)
  
  nonopioid_prop_outpatient <- mean(subset$has_nonopioid_pain_rx_outpatient)
  se_nonopioid_prop_outpatient <- sqrt((nonopioid_prop_outpatient * (1-nonopioid_prop_outpatient))/num_bene)
  
  average_mme_acute <- mean(subset_acute$mean_daily_dose_mme_acute, na.rm=T)
  se_mme_acute <- sd(subset_acute$mean_daily_dose_mme_acute, na.rm=T)/sqrt(num_bene)
  
  average_mme_outpatient <- mean(subset_outpatient$mean_daily_dose_mme_outpatient, na.rm=T)
  se_mme_outpatient <- sd(subset_outpatient$mean_daily_dose_mme_outpatient, na.rm=T)/sqrt(num_bene)
  
  average_days_covered_acute <- mean(subset_acute$opioid_days_covered_acute, na.rm=T)
  se_days_covered_acute <- sd(subset_acute$opioid_days_covered_acute, na.rm=T)/sqrt(num_bene)
  
  average_days_covered_outpatient <- mean(subset_outpatient$opioid_days_covered_outpatient, na.rm=T)
  se_days_covered_outpatient <- sd(subset_outpatient$opioid_days_covered_outpatient, na.rm=T)/sqrt(num_bene)
  
  
  return(c(which_race, 
           which_year, 
           which_pain_or_disability,
           num_bene,
           n_opioid_acute,
           n_opioid_outpatient,
           n_nonopioid_acute,
           n_nonopioid_outpatient,
           pharma_prop_acute,
           se_pharma_prop_acute,
           pharma_prop_outpatient,
           se_pharma_prop_outpatient,           
           opioid_prop_acute,
           se_opioid_prop_acute,
           opioid_prop_outpatient,
           se_opioid_prop_outpatient,
           nonopioid_prop_acute,
           se_nonopioid_prop_acute,
           nonopioid_prop_outpatient,
           se_nonopioid_prop_outpatient,
           average_mme_acute,
           se_mme_acute,
           average_mme_outpatient,
           se_mme_outpatient,
           average_days_covered_acute,
           se_days_covered_acute,
           average_days_covered_outpatient,
           se_days_covered_outpatient
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
    for (k in c("chronic pain only", "disability and chronic pain")){
      results <- rbind(results, my_func(cohort, i, j, k))
      
    }
  }
}

colnames(results) <- c("race_ethnicity",
                       "year",
                       "pain_or_disability",
                       "number_of_beneficiaries",
                       "n_opioid_acute",
                       "n_opioid_outpatient",
                       "n_nonopioid_acute",
                       "n_nonopioid_outpatient",
                       "pharma_prop_acute",
                       "se_pharma_prop_acute",
                       "pharma_prop_outpatient",
                       "se_pharma_prop_outpatient",           
                       "opioid_prop_acute",
                       "se_opioid_prop_acute",
                       "opioid_prop_outpatient",
                       "se_opioid_prop_outpatient",
                       "nonopioid_prop_acute",
                       "se_nonopioid_prop_acute",
                       "nonopioid_prop_outpatient",
                       "se_nonopioid_prop_outpatient",
                       "average_mme_acute",
                       "se_mme_acute",
                       "average_mme_outpatient",
                       "se_mme_outpatient",
                       "average_days_covered_acute",
                       "se_days_covered_acute",
                       "average_days_covered_outpatient",
                       "se_days_covered_outpatient")

results[, -c(1,3)] <- lapply(results[, -c(1,3)], as.numeric)

opioid_names <- c("pharma_prop_acute","se_pharma_prop_acute","opioid_prop_acute","se_opioid_prop_acute","average_mme_acute","se_mme_acute","average_days_covered_acute","se_days_covered_acute")
nonopioid_names <- c("pharma_prop_acute","se_pharma_prop_acute","nonopioid_prop_acute","se_nonopioid_prop_acute")

results <- results |>
  mutate(
    across(opioid_names, ~ ifelse(n_opioid_acute == "*", NA, .)),
    across(nonopioid_names, ~ ifelse(n_nonopioid_acute == "*", NA, .))
  )

write.csv(results, file.path(result_dir, "results_250505.csv"))


# PLOTTING ############################################

results <- read.csv(file.path(result_dir, "results_250505.csv"))

my_palette <- c("#b2182b","#ef8a62","#fddbc7","#d1e5f0","#67a9cf","#2166ac")

# Cleaning race names
races <- data.frame(race_ethnicity = c("multi_or_na","White, non-Hispanic","Black, non-Hispanic","Hispanic, all races","AIAN_or_HPI","Asian, non-Hispanic"),
                    race_name = c("Multiracial or missing","White, non-Hispanic","Black, non-Hispanic","Hispanic, all races","AIAN or HPI","Asian, non-Hispanic"))

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

# Proportion ############################################
# Outpatient
p1 <- ggplot(results, aes(x = year, y=pharma_prop_outpatient, color = race_ethnicity)) +
  geom_jitter(position=position_dodge(0.2)) +
  geom_line(aes(group = race_ethnicity), position=position_dodge(0.2)) +
  facet_wrap(~factor(pain_or_disability,levels = c("chronic pain only","disability and chronic pain")),ncol=1) +
  geom_errorbar(aes(ymin = pharma_prop_outpatient - 1.96*se_pharma_prop_outpatient, 
                    ymax = pharma_prop_outpatient + 1.96*se_pharma_prop_outpatient),
                width = 1, position = position_dodge(0.2)) +
  scale_color_manual(values = my_palette) +
  ggtitle("Any outpatient pharmaceutical") +
  labs(x=NULL,y=NULL,color='Race and Ethnicity') +
  ylim(0,1) +
  theme_light() +
  theme(strip.text = element_text(color = "black"))

legend <- g_legend(p1)
p1 <- p1 + theme(legend.position = "none")

p2 <- ggplot(results, aes(x = year, y=opioid_prop_outpatient, color = race_ethnicity)) +
  geom_jitter(position=position_dodge(0.2)) +
  geom_line(aes(group = race_ethnicity), position=position_dodge(0.2)) +
  facet_wrap(~factor(pain_or_disability,levels = c("chronic pain only","disability and chronic pain")),ncol=1) +
  geom_errorbar(aes(ymin = opioid_prop_outpatient - 1.96*se_opioid_prop_outpatient, 
                    ymax = opioid_prop_outpatient + 1.96*se_opioid_prop_outpatient),
                width = 1, position = position_dodge(0.2)) +
  scale_color_manual(values = my_palette) +
  ggtitle("Opioid (outpatient)") +
  labs(x=NULL,y=NULL,color='Race and Ethnicity') +
  ylim(0,1) +
  theme_light() +
  theme(strip.text = element_text(color = "black"),
        legend.position = "none")

p3 <- ggplot(results, aes(x = year, y=nonopioid_prop_outpatient, color = race_ethnicity)) +
  geom_jitter(position=position_dodge(0.2)) +
  geom_line(aes(group = race_ethnicity), position=position_dodge(0.2)) +
  facet_wrap(~factor(pain_or_disability,levels = c("chronic pain only","disability and chronic pain")),ncol=1) +
  geom_errorbar(aes(ymin = nonopioid_prop_outpatient - 1.96*se_nonopioid_prop_outpatient, 
                    ymax = nonopioid_prop_outpatient + 1.96*se_nonopioid_prop_outpatient),
                width = 1, position = position_dodge(0.2)) +
  scale_color_manual(values = my_palette) +
  ggtitle("Non-Opioid (outpatient)") +
  labs(x=NULL,y=NULL,color='Race and Ethnicity') +
  ylim(0,1) +
  theme_light() +
  theme(strip.text = element_text(color = "black"),
        legend.position = "none")

# Acute
p4 <- ggplot(results, aes(x = year, y=pharma_prop_acute, color = race_ethnicity)) +
  geom_jitter(position=position_dodge(0.2)) +
  geom_line(aes(group = race_ethnicity), position=position_dodge(0.2)) +
  facet_wrap(~factor(pain_or_disability,levels = c("chronic pain only","disability and chronic pain")),ncol=1) +
  geom_errorbar(aes(ymin = pharma_prop_acute - 1.96*se_pharma_prop_acute, 
                    ymax = pharma_prop_acute + 1.96*se_pharma_prop_acute),
                width = 1, position = position_dodge(0.2)) +
  scale_color_manual(values = my_palette) +
  ggtitle("Any acute care pharmaceutical") +
  labs(x=NULL,y=NULL,color='Race and Ethnicity') +
  ylim(0,1) +
  theme_light() +
  theme(strip.text = element_text(color = "black"),
        legend.position = "none")

p5 <- ggplot(results, aes(x = year, y=opioid_prop_acute, color = race_ethnicity)) +
  geom_jitter(position=position_dodge(0.2)) +
  geom_line(aes(group = race_ethnicity), position=position_dodge(0.2)) +
  facet_wrap(~factor(pain_or_disability,levels = c("chronic pain only","disability and chronic pain")),ncol=1) +
  geom_errorbar(aes(ymin = opioid_prop_acute - 1.96*se_opioid_prop_acute, 
                    ymax = opioid_prop_acute + 1.96*se_opioid_prop_acute),
                width = 1, position = position_dodge(0.2)) +
  scale_color_manual(values = my_palette) +
  ggtitle("Opioid (acute)") +
  labs(x=NULL,y=NULL,color='Race and Ethnicity') +
  ylim(0,1) +
  theme_light() +
  theme(strip.text = element_text(color = "black"),
        legend.position = "none")

p6 <- ggplot(results, aes(x = year, y=nonopioid_prop_acute, color = race_ethnicity)) +
  geom_jitter(position=position_dodge(0.2)) +
  geom_line(aes(group = race_ethnicity), position=position_dodge(0.2)) +
  facet_wrap(~factor(pain_or_disability,levels = c("chronic pain only","disability and chronic pain")),ncol=1) +
  geom_errorbar(aes(ymin = nonopioid_prop_acute - 1.96*se_nonopioid_prop_acute, 
                    ymax = nonopioid_prop_acute + 1.96*se_nonopioid_prop_acute),
                width = 1, position = position_dodge(0.2)) +
  scale_color_manual(values = my_palette) +
  ggtitle("Non-Opioid (acute)") +
  labs(x=NULL,y=NULL,color='Race and Ethnicity') +
  ylim(0,1) +
  theme_light() +
  theme(strip.text = element_text(color = "black"),
        legend.position = "none")

pdf("pharma_prop.pdf", width = 10, height = 16)
combined_plots <- arrangeGrob(
  p1, p2, p3, p4, p5, p6,
  ncol = 3
)
grid.arrange(
  arrangeGrob(
    combined_plots,
    legend,
    ncol = 2,
    widths = unit(c(3, 1), "null")  # Adjust these numbers to give more space to either the plots or the legend.
  ),
  left = textGrob("Proportion", rot = 90, gp = gpar(fontsize = 18)),
  bottom = textGrob("Year", gp = gpar(fontsize = 18))
)
dev.off()


# MME ############################################
# Outpatient
p1 <- ggplot(results, aes(x = year, y=average_mme_outpatient, color = race_ethnicity)) +
  geom_jitter(position=position_dodge(0.2)) +
  geom_line(aes(group = race_ethnicity), position=position_dodge(0.2)) +
  ggtitle("Outpatient Opioids") +
  #   ylab("Mean daily MME") +
  facet_wrap(~factor(pain_or_disability, levels = c("chronic pain only","disability and chronic pain"))) +
  geom_errorbar(aes(ymin = average_mme_outpatient - 1.96*se_mme_outpatient, 
                    ymax = average_mme_outpatient + 1.96*se_mme_outpatient),
                width = 1, position = position_dodge(0.2)) +
  scale_color_manual(values = my_palette) +
  labs(x=NULL,y=NULL,color='Race and Ethnicity') +
  theme_light() +
  theme(strip.text = element_text(color = "black"))

# Acute
p2 <- ggplot(results, aes(x = year, y=average_mme_acute, color = race_ethnicity)) +
  geom_jitter(position=position_dodge(0.2)) +
  geom_line(aes(group = race_ethnicity), position=position_dodge(0.2)) +
  ggtitle("Acute Opioids") +
  #   ylab("Mean daily MME") +
  facet_wrap(~factor(pain_or_disability, levels = c("chronic pain only","disability and chronic pain"))) +
  geom_errorbar(aes(ymin = average_mme_acute - 1.96*se_mme_acute, 
                    ymax = average_mme_acute + 1.96*se_mme_acute),
                width = 1, position = position_dodge(0.2)) +
  scale_color_manual(values = my_palette) +
  labs(x=NULL,y=NULL,color='Race and Ethnicity') +
  theme_light() +
  theme(strip.text = element_text(color = "black"))

pdf("opioid_mme.pdf", width = 5, height = 16)
grid.arrange(p1, p2, ncol = 1,
             left = textGrob("Average Daily MME", rot = 90, gp = gpar(fontsize = 12)),
             bottom = textGrob("Year", gp = gpar(fontsize = 12)))
dev.off()


# Prop days ############################################
# outpatient
p1 <- ggplot(results, aes(x = year, y=average_days_covered_outpatient, color = race_ethnicity)) +
  geom_jitter(position=position_dodge(0.2)) +
  geom_line(aes(group = race_ethnicity), position=position_dodge(0.2)) +
  ggtitle("Outpatient opioids") +
  #   ylab("Proportion days covered") +
  facet_wrap(~factor(pain_or_disability, levels = c("chronic pain only","disability and chronic pain"))) +
  geom_errorbar(aes(ymin = average_days_covered_outpatient - 1.96*se_days_covered_outpatient, 
                    ymax = average_days_covered_outpatient + 1.96*se_days_covered_outpatient),
                width = 1, position = position_dodge(0.2)) +
  scale_color_manual(values = my_palette) +
  labs(x=NULL,y=NULL,color='Race and Ethnicity') +
  theme_light() +
  theme(strip.text = element_text(color = "black"))

# Acute
p2 <- ggplot(results, aes(x = year, y=average_days_covered_acute, color = race_ethnicity)) +
  geom_jitter(position=position_dodge(0.2)) +
  geom_line(aes(group = race_ethnicity), position=position_dodge(0.2)) +
  ggtitle("Acute opioids") +
  #   ylab("Proportion days covered") +
  facet_wrap(~factor(pain_or_disability, levels = c("chronic pain only","disability and chronic pain"))) +
  geom_errorbar(aes(ymin = average_days_covered_acute - 1.96*se_days_covered_acute, 
                    ymax = average_days_covered_acute + 1.96*se_days_covered_acute),
                width = 1, position = position_dodge(0.2)) +
  scale_color_manual(values = my_palette) +
  labs(x=NULL,y=NULL,color='Race and Ethnicity') +
  theme_light() +
  theme(strip.text = element_text(color = "black"))

pdf("opioid_days_covered.pdf", width = 5, height = 16)
grid.arrange(p1, p2, ncol = 1,
             left = textGrob("Proportion days covered", rot = 90, gp = gpar(fontsize = 12)),
             bottom = textGrob("Year", gp = gpar(fontsize = 12)))
dev.off()