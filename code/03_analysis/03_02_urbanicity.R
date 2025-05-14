
library(dplyr)
library(data.table)
library(arrow)

save_dir <- "/mnt/general-data/disability/post_surgery_opioid_use/tmp"

drv <- "/mnt/general-data/disability/disenrollment/tafdebse"

urbanicity <- open_dataset(paste0(drv, "/dem_df.parquet")) |>
  select(BENE_ID, RUCC_2013) |>
  filter(!is.na(RUCC_2013)) |>
  collect()

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

for (my_year in 2016:2019) {
  cohort <- readRDS(file.path(save_dir, my_year, paste0("cohort_",my_year,"_pain_only.rds")))
  
  cohort <- cohort |>
    left_join(urbanicity) |>
    mutate(RUCC_missing = as.numeric(is.na(RUCC_2013)),
           RUCC_2013 = fifelse(RUCC_missing == 1, Mode(RUCC_2013), RUCC_2013)) |>
    select(BENE_ID, RUCC_2013, RUCC_missing)
  
  saveRDS(cohort, file.path(save_dir, my_year, paste0(my_year, "_cohort_urbanicity.rds")))
}
  