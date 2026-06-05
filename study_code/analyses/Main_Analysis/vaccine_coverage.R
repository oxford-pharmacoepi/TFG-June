# # Creation of a table that contains the corresponding 
# number of eligibles indicating if they where vaccinated or not stratified by age_group, sex,
# ethnicity, immunosuppression in the campaign, immunosuppression before the campaign, region, 
# IMD and prior dose

sex_imd_eth_reg_immuno_ag_pd <- cdm$all_campaigns |>
  group_by(cohort_name, sex, immunosuppressed,
           age_group, ethnicity, imd, region, prior_dose, vaccinated) |>
  tally()

sex_imd_eth_reg_immuno_ag_pd_time <- cdm$all_campaigns |>
  group_by(cohort_name, sex, immunosuppressed,
           age_group, ethnicity, imd, region, prior_dose, vaccinated, cohort_start_date) |>
  tally()