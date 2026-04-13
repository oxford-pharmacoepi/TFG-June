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
  addAgeEligibility(campaign = campaign) |>
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

#Add comorbidity and other vaccine intersection, after changing the cohort_start_date 
# to the actual vaccination day

cdm$all_campaigns <- cdm$all_campaigns |>
  select( -cohort_start_date, -cohort_end_date) |>
  left_join(
    cdm$vaccine_90|>
      select(-cohort_definition_id, -dose) |>
      rename(cohort_name=vaccination_campaign), 
    by = c("subject_id", "cohort_name")
  ) |>
  compute(name = "all_campaigns") |>
  addConceptIntersectFlag(
    conceptSet = vaccines,
    window = list("prior"=c(-Inf, -1), "on_index"=c(0, 0)),
    nameStyle = "{concept_name}_flag_{window_name}",
    name ="all_campaigns"
  ) |>
  addConceptIntersectCount(
    conceptSet = vaccines,
    window = list("prior"=c(-Inf, -1)),
    nameStyle = "{concept_name}_count_{window_name}",
    name ="all_campaigns"
  ) |>
  addConceptIntersectFlag(
    conceptSet = vaccines,
    window = list(),
    nameStyle = "{concept_name}_flag_{window_name}",
    name ="all_campaigns"
  ) |>
  addConceptIntersectCount(
    conceptSet = comorbidities,
    window = list("prior"=c(-Inf, -1)),
    nameStyle = "{concept_name}_count_{window_name}",
    name ="all_campaigns"
  ) |>
  addAge(ageGroup = list("=<34"=c(0, 34.9), "35-45"=c(35, 44.9), "45-54"=c(45,54.9), 
                    "55-65"=c(55,64.9), "65-74"=c(65,74.9), "75-84"=c(75,84.9),
                    "85-94"=c(85,94.9), ">=95"=c(95,120)), 
         name = "all_campaigns")
  
