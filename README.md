# medicaid_treatment_trends

## Causal question:

How does the trend of the administration of opioid and non-opioid treatments change over the years 2016-2019 across different race and ethnicities, and how does it differ depending on pain or disability group?

## Cohort definition:

Using Kat Hoffmans cohort, <a href="https://github.com/CI-NYC/disability/blob/4a9cb21be99b54a53f6716281277a6821ca7352b/projects/create_cohort/scripts/07_combine_cohort/merge_final_cohort.R#L287">desc_cohort.rds<\a> (exclusions already applied), beneficiaries were separated into four cohorts, one for each year. 

Eligibility for each of the four cohorts:
- continuous enrollment for a 6-month washout period, where the entirety of the 6-month follow-up period must be contained within the given year.
- a beneficiary may be assigned to multiple cohorts if they meet the continuous enrollment criteria for multiple years.


## Steps to do:
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
