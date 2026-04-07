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
  addImmunosuppressed() |>
  addAge(name = campaign) |>
  filter(if_else(campaign == "a_2023", 
                 age >= 75L | immunosuppressed == 1L, 
                 age >= 65L | immunosuppressed == 1L)) |>
  compute(name = campaign)|>
  recordCohortAttrition(reason = "Eligible for vaccination") |>
  addDoseCampaign() |>
  addDosePriorCampaign(name = campaign) |>
  filter(prior_dose>=2L) |>
  compute(name = campaign)|>
  recordCohortAttrition(reason = "At least 2 doses at campaign start") |>
  addAgeEligibility() |>
  addSex(name = campaign)
}

cdm<- bind(
  cdm$a_2023 |> renameCohort("a_2023"),
  cdm$s_2024 |> renameCohort("s_2024"),
  cdm$a_2024 |> renameCohort("a_2024"),
  cdm$s_2025 |> renameCohort("s_2025"),
  name = "all_campaigns"
) 
  
cdm$all_campaigns <- cdm$all_campaigns |>
 addCohortName()