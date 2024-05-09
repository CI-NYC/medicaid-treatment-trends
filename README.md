# medicaid_treatment_trends

## Causal question:

How does the trend of the administration of opioid and non-opioid treatments change over the years 2016-2019 across different race and ethnicities, and how does it differ depending on pain or disability group?

## Cohort definition:

Using Kat Hoffmans cohort, <a href="https://github.com/CI-NYC/disability/blob/4a9cb21be99b54a53f6716281277a6821ca7352b/projects/create_cohort/scripts/07_combine_cohort/merge_final_cohort.R#L287">desc_cohort.rds</a> (exclusions already applied), beneficiaries were separated into four cohorts, one for each year. 

Eligibility for each of the four cohorts:
- continuous enrollment for a 6-month washout period, where the entirety of the 6-month follow-up period must be contained within the given year.
- a beneficiary may be assigned to multiple cohorts if they meet the continuous enrollment criteria for multiple years.

## Procedure

1. Starting with the cohort used in the total effects paper (first author, Kat Hoffman) ~ 2.4 million. We will call this the “cohort”. Each individual in the cohort can be indexed by i.

2. The cohort will be separated into many groups based on three variables:
    1. race/ethnicity
    2. year
    3. chronic pain or physical disability

3. To group the cohort into these subsets,
    1. First, separate the cohort by each race/ethinicity individually 
    2. For every race group, subset the observations based on year
    - 2016
    - 2017
    - 2018
    - 2019
    3. For every race-year group, further subset the group based on the following ailments:
    - chronic pain and physical disability
    - chronic pain alone
    - physical disability alone 
    - neither



Definitions for chronic pain and physical disability are included in the following table

| Variable | Definition |
|---|---|
| Chronic pain | Defined <a href="https://github.com/CI-NYC/disability-chronic-pain/blob/main/scripts/04_define_comorbidity_vars/define_chronic_pain.R">here</a> |
| Physical disability | Defined <a href="https://github.com/CI-NYC/disability-chronic-pain/blob/bf2c9f7c0359524eccbf2b65b6401882c7998a92/scripts/02_clean_tafdebse.R#L346">here</a> Called “<code>disability_washout_cal"</code> |
|  |  |


3. For each race-year-pain/disability group:
    1. Estimate the likelihood of being prescribed an opioid. Do this by finding the proportion of patients in the group who were prescribed an opioid.

4. For each race-year-pain/disability group AND among only those who were prescribed an opioid:
    1. Estimate the average MME

    2. among those prescribed an opioid, estimate the average days covered during rolling 6 month windows
    - from enrollment date, `Washout_start_dt`, to 6 months later, `washout_cal_end_dt`, calculate days covered
    - move the start date ahead 1 month and repeat. Keep calculating days covered for every 6 month window until the last day of enrollment
    - across all 6-month windows, calculate the average days covered.




Find definitions for computations in the following table

| Variable | Definition |
|---|---|
| MME | defined <a href="https://github.com/CI-NYC/disability/blob/6e79d09036ee743c6be5c989812d18d3ae9e5e0c/projects/mediation_unsafe_pain_mgmt/01_create_mediators/11_mediator_dose_mme.R">here</a> |
| Days covered |  |

## Still to do:
0. new script for computing follow-up start dates and follow-up end dates

1. Collect opioid outcome separately for each cohort
- for loop (for my_year in 2016:2019){
    - use file.path(save_dir, my_year, filename) for loading and saving files
    - hopefully the rest will automate smoothly
}
- result: 4 cohorts with mean daily dose mme and proportion of days covered

2. rbind the 4 cohorts together and (doesn't matter that BENE_ID can be repeated?):
3. make a df summarizing opioid outcomes across year, pain/disability, race/ethnicity
- create a table of strata sizes

4. non-opioid treatments - same, run separately for all 4 cohorts. rbind at the end, summarize, then make plots
- need to reorgnize code to skip multimodal_pain_treatment,
- counseling is contained with multimodal_pain_treatment
