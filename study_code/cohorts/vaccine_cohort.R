# Vaccinated people within the vaccination campaigns of interest -either for being immunosuppressed or by age- 
# stratified by age, ethnicity, IMD, sex and region. Will be used for overall attrition and chronology,
# since it has the real cohort_start_ and _end dates
cdm$vaccinated_within_campaigns <- cdm$all_campaigns |>
  filter(vaccinated == 1) |>
  compute(name = "vaccinated_within_campaigns") |>
  recordCohortAttrition(reason = "Vaccinated within campaigns of interest")|>
  addComorbidities(name = "vaccinated_within_campaigns")



# For sensitivity analysis
cdm$vaccinated_within_campaigns_sens <- cdm$all_campaigns_sens |>
  left_join(cdm$vaccinated_within_campaigns, 
            by = intersect(
              colnames(cdm$all_campaigns_sens),
              colnames(cdm$vaccinated_within_campaigns)
              )
            )|>
  filter(vaccinated == 1) |>
  compute(name = "vaccinated_within_campaigns_sens") |>
  recordCohortAttrition(reason = "Vaccinated within campaigns of interest") 


