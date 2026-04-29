# Preparing vaccinated people's cohort

# Vaccine records
cdm$vaccine <- conceptCohort(cdm = cdm,
                             conceptSet = diagnosis["covid_vaccine"],
                             name = "vaccine"
)

# Vaccine record with 90 days washout  +
# Addition of the number of dose from the received vaccines for each subject +
# Addition of the campaign the vaccine corresponds to 
cdm$vaccine_90 <- cdm$vaccine |>
  requireCohortIntersect(
    targetCohortTable = "vaccine", 
    window = c(-90, -1), #CAREFUL!! The cohort can't be windowed
    intersections = 0,
    name="vaccine_90"
  ) |>
  group_by(subject_id) |>
  arrange(cohort_start_date) |>
  mutate(dose = row_number()) |>
  ungroup() |>
  addCampaigns()

# Objective 1: Characterisation

# Other vaccines: 

cdm$immunosuppresed <- cdm$vaccine_90 |>
  addImmunosuppresed(name = "immunosuppresed") 

cdm$othervaccines <- conceptCohort(cdm = cdm, 
                                   name = "othervaccines", 
                                   conceptSet = vaccines,
                                   useSourceFields = TRUE,
                                   exit = "event_start_date")

# Comorbidities
cdm$comorbidities <- conceptCohort(cdm = cdm, 
                                   name = "comorbidities", 
                                   conceptSet = comorbidities)
