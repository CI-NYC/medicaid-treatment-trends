# medicaid_treatment_trends

## Causal question:

How does the trend of the administration of opioid and non-opioid treatments change over the years 2016-2019 across different race and ethnicities, and how does it differ depending on pain or disability group?

## Cohort definition:

Using Kat Hoffmans cohort, <a href="https://github.com/CI-NYC/disability/blob/4a9cb21be99b54a53f6716281277a6821ca7352b/projects/create_cohort/scripts/07_combine_cohort/merge_final_cohort.R#L287">desc_cohort.rds</a> (exclusions already applied), beneficiaries were separated into four cohorts, one for each year. 

Washout period: Beneficiaries are required to be continuously enrolled for a 6-month washout period.

Follow-up period: Beneficiaries will be followed for 6 months directly after washout. All strata/exposures and outcomes are measured within this follow-up period.

There are 4 "cohorts". Eligiblity criteria explained below:
- Each cohort contains beneficiaries who are eligible for one of the 4 years, 2016-2019.
- continuous enrollment for a 6-month washout period, where the entirety of the 6-month follow-up period must be contained within the given year.
- a beneficiary may be assigned to multiple cohorts if they meet the continuous enrollment criteria for multiple years.

## Procedure

1. Starting with the cohort used in the total effects paper (first author, Kat Hoffman) ~ 13.5 million.

2. Individuals in this original cohort will need to be assigned to one or more of the 4 cohorts defined above. They will be assigned on the basis of whether they meet the continuous enrollment criteria for a given year.

3. An additional exclusion criteria: for all years, beneficiaries from the state Rhode Island need to be excluded. Also, for the years 2017-2018, beneficiaries from Michigan need to be excluded.

4. For each year, cohorts will also need to be stratified into many groups based on:
    1. race/ethnicity
    2. chronic pain or physical disability

5. To subset the cohorts:
    1. Race and ethnicity has been recorded in the variable `dem_race_cond` and has 6 levels:
       AIAN_or_HPI (stands for American Indian, Alaska Native, Hawaiian or Pacific Islander),
       Asian, non-Hispanic,
       Black, non-Hispanic,
       Hispanic, all races,
       multi_or_na,
       White, non-hispanic.
       
    2. Year can take on any value from 2016-2019. Beneficiaries are eligible to be included in a given year if they have a 6-month followup period contained entirely within the year, which is defined by being continuously enrolled for a 6-month washout period directly before the follow-up period.

    3. Chronic pain and physical disability groups are determined based on whether a beneficiary has:
    - both chronic pain and physical disability
    - chronic pain alone
    - physical disability alone 
    - neither
      
    - Code for defining the 4 groups for pain and disability mentioned above is found <a href="[https://github.com/CI-NYC/disability-chronic-pain/blob/93bbeb9d2edff361bf622a9889c7e1d811f0f238/scripts/04_define_comorbidity_vars/define_chronic_pain.R#L5](https://github.com/CI-NYC/disability-chronic-pain/blob/93bbeb9d2edff361bf622a9889c7e1d811f0f238/scripts/07_combine_cohort/merge_final_cohort.R#L103-L106)">here</a>. This requires two components, i) chronic pain, and ii) physical disability. Explanations for where to find code that defines them in the table below:

Definitions for chronic pain and physical disability are included in the following table

| Variable | Definition |
|---|---|
| Physical disability | Code for defining physical disability is found <a href="https://github.com/CI-NYC/disability-chronic-pain/blob/93bbeb9d2edff361bf622a9889c7e1d811f0f238/scripts/02_clean_tafdebse.R#L341-L347">here</a>. I will need to adjust the date period I search within. |
| Chronic pain | Code for defining chronic pain is found <a href="https://github.com/CI-NYC/disability-chronic-pain/blob/93bbeb9d2edff361bf622a9889c7e1d811f0f238/scripts/04_define_comorbidity_vars/define_chronic_pain.R#L5">here</a>. Original script searches over 18 rolling 6-month windows (from month 0 to month 17). My modified code will only need to search within months 7-12 (the follow-up period), and thus, does not require rolling windows. |
|  |  |

6. For each race-year-pain/disability group:
    1. Estimate the likelihood of being prescribed an opioid. Do this by finding the proportion of patients in the group who were prescribed an opioid during the 6-month follow-up period.

7. For each race-year-pain/disability group AND among only those who were prescribed an opioid:
    1. Estimate the daily average MME

    2. among those prescribed an opioid, estimate the average days covered for an opioid during the 6-month follow-up period

Find definitions for computations in the following table

| Variable | Definition |
|---|---|
| MME | MME dose conversion is defined <a href="https://github.com/CI-NYC/disability/blob/6e79d09036ee743c6be5c989812d18d3ae9e5e0c/projects/mediation_unsafe_pain_mgmt/01_create_mediators/11_mediator_dose_mme.R">here</a> <p> mean daily dose of MME is calculated <a href="https://github.com/CI-NYC/disability/blob/4a9cb21be99b54a53f6716281277a6821ca7352b/projects/mediation_unsafe_pain_mgmt/01_create_mediators/15_mediator_max_daily_dose_mme.R">here</a> using the function "calculate_mean_daily_dose". <p> Modifications will need to be made to find the "mean" MME instead of "max" |
| Days covered | Defined <a href="https://github.com/CI-NYC/disability/blob/4a9cb21be99b54a53f6716281277a6821ca7352b/projects/mediation_unsafe_pain_mgmt/01_create_mediators/31_mediator_proportion_days_covered.R">here</a> <p> only adjustments to the date period will need to be made|


8. Addendum: additional outcomes for non-opioid treatments will be measured. These include:
- <a href="https://github.com/CI-NYC/disability/blob/4a9cb21be99b54a53f6716281277a6821ca7352b/projects/mediation_unsafe_pain_mgmt/01_create_mediators/23_mediator_counseling.R">Counseling</a> 
- <a href="https://github.com/CI-NYC/disability/blob/4a9cb21be99b54a53f6716281277a6821ca7352b/projects/mediation_unsafe_pain_mgmt/01_create_mediators/29_mediator_physical_therapy.R">Physical therapy</a>
- <a href="https://github.com/CI-NYC/disability/blob/4a9cb21be99b54a53f6716281277a6821ca7352b/projects/mediation_unsafe_pain_mgmt/01_create_mediators/22_mediator_chiropractic.R">Chiropractic work</a>
- <a href="https://github.com/CI-NYC/disability/blob/4a9cb21be99b54a53f6716281277a6821ca7352b/projects/mediation_unsafe_pain_mgmt/01_create_mediators/19_mediator_acupuncture.R">Acupuncture</a>
- <a href="https://github.com/CI-NYC/disability/blob/4a9cb21be99b54a53f6716281277a6821ca7352b/projects/mediation_unsafe_pain_mgmt/01_create_mediators/03_mediator_nonopioid_pain_rx.R">Non-opioid medication</a>

9. Like before, for every strata, we need to find the proportion of beneficiaries who received each of the non-opioid treatments.


## Still to do:
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
