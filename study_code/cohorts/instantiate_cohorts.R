# Preparing vaccinated people's cohort

# Vaccine records
cdm$vaccine <- conceptCohort(cdm = cdm,
                             conceptSet = diagnosis["covid_vaccine"],
                             name = "vaccine"
                             )

# Vaccine record with 21 days washout between the first and second dose
cdm$vaccine_washout1 <- cdm$vaccine |> 
  requireCohortIntersect(
    targetCohortTable = "vaccine", #CAREFUL!! The cohort can't be windowed
    window = c(1, 21),
    intersections = 0,
    atFirst = TRUE,
    name = "vaccine_washout1"
  ) 

# Analysing if indeed there were 1 and 2 dose vaccines administered after 
# 21 days and before 90 days (general washout)
cdm$vaccine_dose1  <- cdm$vaccine_washout1 |>
  requireIsFirstEntry(name = "vaccine_dose1") 

cdm$vaccine_dose2more <- cdm$vaccine_washout1 |>
  requireCohortIntersect(targetCohortTable = "vaccine_dose1",
                         window = c(0,0),
                         intersections = 0,
                         name = "vaccine_dose2more")

# Number of subject fulfilling the former    
cdm$vaccine_dose1_study <- cdm$vaccine_dose1 |>
  addCohortIntersectDays(targetCohortTable = "vaccine_dose2more",
                         order = "first",
                         window = c(0, Inf),
                         nameStyle = "days_from_dose1",
                         name = "vaccine_dose1_study") |>
  filter(days_from_dose1 <= 90) |>
  tally() 
                         
cdm$vaccine_dose2  <- cdm$vaccine_dose2more |>
  requireIsFirstEntry(name = "vaccine_dose2") 

cdm$vaccine_dose3more <- cdm$vaccine_dose2more |>
  requireCohortIntersect(targetCohortTable = "vaccine_dose2",
                         window = c(0,0),
                         intersections = 0,
                         name = "vaccine_dose3more")

cdm$vaccine_washout <- cdm$vaccine_washout1 |>
  requireCohortIntersect(
    targetCohortTable = "vaccine_dose3more", 
    window = c(-90, -1), 
    intersections = 0,
    name="vaccine_washout") |>
    group_by(subject_id) |>
    arrange(cohort_start_date) |>
    mutate(dose = row_number()) |>
  addCampaigns(name = "vaccine_washout")

cdm$demo <- demographicsCohort(cdm, name = "demo") |>
  # to consider to add this with demo cohort
  addRegion() |>
  addIMD() |>
  addEthnicity(name="demo") 
  
  
# Denominator for sensitivity analysis  
cdm$demo_sens <- cdm$demo |>
  requireInDateRange(dateRange = as.Date(c(NA, "2021-01-01")), name = "demo_sens")

# Other vaccines:
cdm$othervaccines <- conceptCohort(cdm = cdm, 
                                   name = "othervaccines", 
                                   conceptSet = vaccines,
                                   useSourceFields = TRUE,
                                   exit = "event_start_date",
                                   subsetCohort = "demo")
# Comorbidities
cdm$comorbidities <- conceptCohort(cdm = cdm, 
                                   name = "comorbidities", 
                                   conceptSet = comorbidities,
                                   subsetCohort = "demo")

# Immunosuppressed 
cdm$immunosuppressed <- conceptCohort(cdm = cdm, 
                                      name = "immunosuppressed", 
                                      conceptSet = immuno,
                                      exit = "event_start_date",
                                      subsetCohort = "demo")

cdm$immunosuppressed <- cdm$immunosuppressed |>
  addCohortName()|>
  compute(name = "immunosuppressed")


