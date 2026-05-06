# Vaccinated people within the vaccination campaigns of interest -either for being immunosuppressed or by age- 
# stratified by age, ethnicity, IMD, sex and region. Will be used for overall attrition and chronology,
# since it has the real cohort_start_ and _end dates
cdm$vaccinated_within_campaigns <- cdm$all_campaigns |>
  filter(vaccinated != 0) |>
  compute(name = "vaccinated_within_campaigns") |>
  recordCohortAttrition(reason = "Vaccinated within campaigns of interest") 

# For sensitivity analysis
cdm$vaccinated_within_campaigns_sens <- cdm$all_campaigns |>
  filter(vaccinated != 0) |>
  compute(name = "vaccinated_within_campaigns_sens") |>
  recordCohortAttrition(reason = "Vaccinated within campaigns of interest") 


