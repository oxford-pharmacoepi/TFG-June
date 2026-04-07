# Vaccinated people within the vaccination campaigns of interest -either for being immunosuppressed or by age- 
# stratified by age, ethnicity, IMD, sex and region. Will be used for overall attrition and chronology,
# since it has the real cohort_start_ and _end dates
cdm$vaccinated_within_campaigns <-cdm$all_campaigns |>
  #for sensitivity analysis
  #requireInDateRange(dateRange =c(NA, "2021-01-01"), name = "vaccinated_within_campaigns")|>
  filter(vaccinated != 0) |>
  compute(name = "vaccinated_within_campaigns") |>
  recordCohortAttrition(reason = "Vaccinated within campaigns of interest") |>
  select( -cohort_start_date, -cohort_end_date) |>
  left_join(
    cdm$vaccine_90|>
      select(-cohort_definition_id, -dose), 
    by = c("subject_id", "vaccination_campaign")
  ) |>
 compute(name = "vaccinated_within_campaigns")


