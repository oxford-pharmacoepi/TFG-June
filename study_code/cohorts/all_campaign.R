# Select the individuals to be included for the coverage assessment
cdm$demo <- demographicsCohort(cdm, name = "demo") |>
  # to consider to add this with demo cohort
  addRegion() |>
  addIMD() |>
  addEthnicity(name="demo") 

campaigns <- c("a_2023", "s_2024", "a_2024", "s_2025")

for (campaign in campaigns){
cdm[[campaign]] <- cdm$demo |>
  copyCohorts(n = 1, name = campaign) |>
  trimDatesIntoCampaign(campaign) |>
  addVaccinatedInCampaign() |>
  addCohortIntersectField(cdm$immunosuppressed,
                             immunosuppressed,
                             window = list(c(0, Inf)),
                             name = campaign) |>
  addDemographics(age =TRUE,
                  sex = TRUE,
                  name = campaign) |>
  filter(if_else(campaign == "a_2023", 
                 age >= 75L | immunosuppressed == 1L, 
                 age >= 65L | immunosuppressed == 1L)) |>
  compute(name = campaign)|>
  recordCohortAttrition(reason = "Eligible for vaccination") |>
  addDosePriorCampaign(name = campaign) |>
  filter(prior_dose>=2L) |>
  compute(name = campaign)|>
  recordCohortAttrition(reason = "At least 2 doses at campaign start") |>
  addAgeEligibility(campaign = campaign)
}

cdm<- bind(
  cdm$a_2023 |> renameCohort("a_2023"),
  cdm$s_2024 |> renameCohort("s_2024"),
  cdm$a_2024 |> renameCohort("a_2024"),
  cdm$s_2025 |> renameCohort("s_2025"),
  name = "all_campaigns"
) 

# We change the cohort_start_date by the actual vaccination day if vaccinated, 
# and leave it as it was, if not
cdm$all_campaigns <- cdm$all_campaigns |>
  addCohortName() |>
  addAge(ageGroup = list(
    "=<34"=c(0, 34), "35-45"=c(35, 44), "45-54"=c(45,54), 
    "55-64"=c(55,64), "65-74"=c(65,74), "75-84"=c(75,84),
    "85-94"=c(85,94), ">=95"=c(95,120)),
    name = "all_campaigns")


  
