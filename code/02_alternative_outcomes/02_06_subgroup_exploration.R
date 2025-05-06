
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

# Comparison 1:
# TREATMENT Counseling + chiropractic
# AMONG White 
# VS Asian
# WITH CHRONIC PAIN
# IN THE YEAR 2019

# Comparison 2:
# TREATMENT Acupuncture + physical therapy
# AMONG ASIAN 
# VS WHITE
# WITH CHRONIC PAIN
# IN THE YEAR 2019

cohort <- merge_cohort(2019)
setDT(cohort)


cohort <- cohort |>
  mutate(counseling_or_chiro = as.numeric(has_counseling+has_chiropractic>0),
         acu_or_pt = as.numeric(has_acupuncture+has_physical_therapy>0))

white_cohort <- cohort |>
  filter(dem_race_cond=="White, non-Hispanic",
         disability_pain_cal=="chronic pain only")

asian_cohort <- cohort |>
  filter(dem_race_cond=="Asian, non-Hispanic",
         disability_pain_cal=="chronic pain only")

(mean(white_cohort$counseling_or_chiro)*100)/(mean(asian_cohort$counseling_or_chiro)*100)


(mean(asian_cohort$acu_or_pt)*100)/(mean(white_cohort$acu_or_pt)*100)


