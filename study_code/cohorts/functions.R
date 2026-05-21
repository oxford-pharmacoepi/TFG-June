#functions to be used for the cohort creation
addIMD <- function(cohort, name = tableName(cohort)){
  cohort|>
    left_join(cdm$measurement |>     
  filter(measurement_concept_id== "715996")|>
  select(person_id, value_as_number)|>
  rename(subject_id=person_id, imd=value_as_number)|>
  mutate(imd = case_when(
    imd %in% c(1,2) ~ "Q1",
    imd %in% c(3,4) ~ "Q2",
    imd %in% c(5,6) ~ "Q3",
    imd %in% c(7,8) ~ "Q4",
    imd %in% c(9,10) ~ "Q5")
    ), 
  by="subject_id") |>
    mutate(imd = coalesce(imd, "missing")) |>
    compute(name = name) 
  }

addEthnicity <- function(cohort, name = tableName(cohort)){
  cohort|>
    left_join(cdm$person|>
                rename(subject_id=person_id, 
                       ethnicity=race_source_value)|>
                select(subject_id, ethnicity),
              by="subject_id") |>
    mutate(ethnicity = coalesce(ethnicity, "missing")) |>
    compute(name = name)
}

addRegion <- function(cohort,  name = tableName(cohort)) {
  cohort |>
    left_join(
      cdm$location |>
        select(location_source_value, location_id) |>
        inner_join(
          cdm$care_site |>
            select(location_id, care_site_id),
          by = "location_id"
        ) |>
        left_join(
          cdm$person |>
            select(person_id, care_site_id),
          by = "care_site_id"
        ) |>
        select(location_source_value, person_id) |>
        rename(subject_id = person_id, region = location_source_value)|>
        mutate(region = if_else(
          region %in% c("Scotland", "Wales", "Northern Ireland"),
          region,
          "England"
        )), 
      by="subject_id"
    ) |>
    mutate(region = coalesce(region, "missing")) |>
    compute(name = name)
}

addCampaigns <- function(cohort, name = tableName(cohort)){
  cohort|>
    #filter(cohort_start_date>as.Date("2023-10-02") & cohort_start_date<as.Date("2026-01-31"))|>
    mutate(vaccination_campaign = case_when(
      cohort_start_date>=as.Date("2023-10-02") & cohort_start_date<=as.Date("2024-01-31") ~ "a_2023",
      cohort_start_date>=as.Date("2024-04-15") & cohort_start_date<=as.Date("2024-06-30") ~ "s_2024",
      cohort_start_date>=as.Date("2024-10-03") & cohort_start_date<=as.Date("2025-01-31") ~ "a_2024",
      cohort_start_date>=as.Date("2025-04-01") & cohort_start_date<=as.Date("2025-06-17") ~ "s_2025",
      cohort_start_date>=as.Date("2025-09-01") & cohort_start_date<=as.Date("2026-01-31") ~ "a_2025",
      TRUE ~ NA_character_)
    ) |>
    mutate(vaccination_campaign = coalesce(vaccination_campaign, "None")) |>
    compute(name = name)
}

addImmunosuppressed <-  function(cohort, name = tableName(cohort)) {
  cohort |>
    addCohortIntersectFlag(
      targetCohortTable = "immunosuppressed",
      targetCohortId = c(
        "cancerexcludnonmelaskincancer", "hiv_aids", "intrinsec_immune",
        "autoimmune", "transplant", "scid"
      ),
      window = c(-365, 0),
      nameStyle = "{cohort_name}",
      name = name
    ) |>
    addCohortIntersectFlag(
      targetCohortTable = "immunosuppressed",
      targetCohortId = c(
        "immunos_antineo", "immunos_antineo_exclude", "syst_corticosteriods"
      ),
      window = c(-183, -1),
      nameStyle = "{cohort_name}",
      name = name
    ) |>
    mutate(immunosuppressed = case_when(
      cancerexcludnonmelaskincancer + hiv_aids + intrinsec_immune +
        immunos_antineo + immunos_antineo_exclude + scid > 0L ~ 1L,
      syst_corticosteriods == 1L & autoimmune + transplant > 0L ~ 1L,
      TRUE ~  0L
    )) |>
    select(c(colnames(cohort),"immunosuppressed")) |>
    compute(name = name)
}

addCampaigns <- function(cohort, name = tableName(cohort)){
  cohort|>
    #filter(cohort_start_date>as.Date("2023-10-02") & cohort_start_date<as.Date("2026-01-31"))|>
    mutate(vaccination_campaign = case_when(
      cohort_start_date>=as.Date("2023-10-02") & cohort_start_date<=as.Date("2024-01-31") ~ "a_2023",
      cohort_start_date>=as.Date("2024-04-15") & cohort_start_date<=as.Date("2024-06-30") ~ "s_2024",
      cohort_start_date>=as.Date("2024-10-03") & cohort_start_date<=as.Date("2025-01-31") ~ "a_2024",
      cohort_start_date>=as.Date("2025-04-01") & cohort_start_date<=as.Date("2025-06-17") ~ "s_2025",
      cohort_start_date>=as.Date("2025-09-01") & cohort_start_date<=as.Date("2026-01-31") ~ "a_2025",
      TRUE ~ NA_character_)
    ) |>
    mutate(vaccination_campaign = coalesce(vaccination_campaign, "None")) |>
  compute(name = name)
}

addDosePriorCampaign <- function(cohort, name = tableName(cohort)) {
  cohort|>
    addCohortIntersectField(
      targetCohortTable = "vaccine_washout",
      field = "dose",
      indexDate = "cohort_start_date", # start of vaccination campaign
      order = "last",
      window = list(c(-Inf, -1)),
      nameStyle = "prior_dose",
      name = name
    )|>
    mutate(prior_dose = as.integer(prior_dose)) |>
    # time since last dose
    addCohortIntersectDays(
      targetCohortTable = "vaccine_washout",
      window = c(-Inf, -1),
      order = "last",
      nameStyle = "last_dose_days",
      name = name
    )
}

addVaccinatedInCampaign <- function(cohort, name = tableName(cohort)){
  cohort |> 
    addCohortIntersectFlag(
    targetCohortTable = "vaccine_washout",
    indexDate = "cohort_start_date",
    censorDate = "cohort_end_date",
    window = list(c(0, Inf)),
    nameStyle = "vaccinated",
    name = name
   ) |>
    left_join(
      cdm$vaccine_washout |>
        select(-cohort_definition_id) |>
        filter(vaccination_campaign == campaign),
      by = c("subject_id")
    ) |>
    select(-vaccination_campaign) |>
    mutate(cohort_start_date = if_else(
      is.na(cohort_start_date.y),
      cohort_start_date.x,
      cohort_start_date.y
    )
    ) |>
    mutate(cohort_end_date = if_else(
      is.na(cohort_end_date.y),
      cohort_end_date.x,
      cohort_end_date.y
    )
    ) |>
    select(-cohort_start_date.x, -cohort_start_date.y, -cohort_end_date.x, -cohort_end_date.y) |>
    compute(name = name)
}

trimDatesIntoCampaign <- function(cohort, campaign) {
  start <- switch(campaign,
     "a_2023" = as.Date("2023-10-02"),
     "s_2024" = as.Date("2024-04-15"),
     "a_2024" = as.Date("2024-10-03"),
     "s_2025" = as.Date("2025-04-01"),
     "a_2025" = as.Date("2025-09-01")
     )
  end <- switch(campaign,
     "a_2023" = as.Date("2024-01-31"),
     "s_2024" = as.Date("2024-06-30"),
     "a_2024" = as.Date("2025-01-31"),
     "s_2025" = as.Date("2025-06-17"),
     "a_2025" = as.Date("2026-01-31")
     )

  cohort|>
    trimToDateRange(c(start, end)) |>
    requirePriorObservation(minPriorObservation = 365)
}

addAgeEligibility <- function(cohort, name = tableName(cohort), campaign) {
    if("vaccination_campaign" %in% colnames(cohort)){
      cohort |>
        mutate(age_eligibility=case_when(
      (vaccination_campaign == "a_2023") & age >= 65 ~ 1L, 
      (vaccination_campaign == "s_2024") & age >= 75 ~ 1L,
      (vaccination_campaign == "a_2024") & age >= 75 ~ 1L,
      (vaccination_campaign == "s_2025") & age >= 75 ~ 1L,
      (vaccination_campaign == "a_2025") & age >= 75 ~ 1L,
  TRUE ~ 0L)) |>
    compute(name = name)
    } 
  else {
  cohort |>
    mutate(age_eligibility=case_when(
      (campaign == "a_2023") & age >= 65 ~ 1L, 
      (campaign == "s_2024") & age >= 75 ~ 1L,
      (campaign == "a_2024") & age >= 75 ~ 1L,
      (campaign == "s_2025") & age >= 75 ~ 1L,
      (campaign == "a_2025") & age >= 75 ~ 1L,
      TRUE ~ 0L)) |>
        compute(name = name)}
}

addSensitivity <- function(cohort, name = tableName(cohort)) {
  
  cohort |>
    left_join(cdm$demo_sens |>
                select(subject_id) |>
                mutate(satisfy_sensitivity = 1L),
              by = "subject_id") |>
    mutate(
      satisfy_sensitivity = coalesce(
        satisfy_sensitivity,
        0L
      )
    ) |>
    compute(name = name)
  
}

addComorbidities <- function(cohort, name = tableName(cohort)) {
  cohort|>
    addCohortIntersectFlag(
          targetCohortTable = "comorbidities",
          window = list("flag_any_time_prior_comorbidities" = c(-Inf, -1)),
          name = name
          ) |>
    # addCohortIntersectCount(
    #   targetCohortTable = "othervaccines",
    #   window = list("count_any_time_prior_vaccination" = c(-Inf, -1),
    #                 "count_last_year_vaccination" =  c(-365, -1)),
    #   #nameStyle = "{window_name}",
    #   name = name
    # )|>
    mutate(comorbidities = if_else(if_any(c(contains("flag_any_time_prior_comorbidities")), ~.x== 1L), 
           1L, 0L))|>
    select(c(colnames(cohort),"comorbidities")) |>
    compute(name = name)
}

addOtherVaccines <- function(cohort,
                             window = list(other_vaccines_on_index = c(0, 0)),
                             name = tableName(cohort)) {
  
  window_name <- names(window)[1]
  
  cohort |>
    addCohortIntersectFlag(
      targetCohortTable = "othervaccines",
      window = window,
      name = name
    ) |>
    mutate(
      othervaccines = if_else(
        if_any(all_of(window_name), ~ .x == 1L),
        1L,
        0L
      )
    ) |>
    select(all_of(c(colnames(cohort), window_name))) |>
    compute(name = name)
}