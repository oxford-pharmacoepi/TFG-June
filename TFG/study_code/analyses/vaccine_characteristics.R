# Objective 1: Characterisation:
# All of the vaccinated people stratified for all campaigs, where the 2 dosis 
# filter is considered, as well as comorbidities and other vaccine administrations
characterisation <- cdm$vaccinated_within_campaigns |>
  summariseCharacteristics(
    ageGroup = list(
      "=<34"=c(0, 34), "35-45"=c(35, 44), "45-54"=c(45,54), 
      "55-65"=c(55,64), "65-74"=c(65,74), "75-84"=c(75,84),
      "85-94"=c(85,94), ">=95"=c(95,120)
      ), 
    tableIntersectCount = list(
      "Number visits in the prior year" = list(
        tableName = "visit_occurrence",
        window = c(-365, -1)
      )
    ),
    cohortIntersectFlag = list(
      "flag_any_time_prior_vaccination" = list(
        targetCohortTable = "othervaccines",
        window = c(-Inf, -1)
      ),
      "flag_last_year_vaccination" = list(
        targetCohortTable = "othervaccines",
        window = c(-365, -1)
      ),
      "flag_on_index_vaccination" = list(
        targetCohortTable = "othervaccines",
          window = c(0, 0)
        ),
      "flag_any_time_prior_comorbidities" = list(
        targetCohortTable = "comorbidities",
          window = c(-Inf, -1)
        )
      ),
    cohortIntersectCount = list(
      "count_any_time_prior_vaccination" = list(
        targetCohortTable = "othervaccines",
        window = c(-Inf, -1)
      ),
      "count_last_year_vaccination" = list(
        targetCohortTable = "othervaccines",
        window = c(-365, -1)
      )
    ),
    otherVariables = c("region", "ethnicity", "sex", "imd",
                                 "immunosuppressed", "age_eligibility", "prior_dose", "vaccine_dose"),
    estimates = list(immunosuppressed = c("count", "percentage"), 
                     age_eligibility = c("count", "percentage"))
)

characterisation_eligibles <- cdm$all_campaigns |>
  summariseCharacteristics(
    ageGroup = list(
      "=<34"=c(0, 34), "35-45"=c(35, 44), "45-54"=c(45,54), 
      "55-65"=c(55,64), "65-74"=c(65,74), "75-84"=c(75,84),
      "85-94"=c(85,94), ">=95"=c(95,120)
    ), 
    tableIntersectCount = list(
      "Number visits in the prior year" = list(
        tableName = "visit_occurrence",
        window = c(-365, -1)
      )
    ),
    cohortIntersectFlag = list(
      "flag_any_time_prior_vaccination" = list(
        targetCohortTable = "othervaccines",
        window = c(-Inf, -1)
      ),
      "flag_last_year_vaccination" = list(
        targetCohortTable = "othervaccines",
        window = c(-365, -1)
      ),
      "flag_on_index_vaccination" = list(
        targetCohortTable = "othervaccines",
        window = c(0, 0)
      ),
      "flag_any_time_prior_comorbidities" = list(
        targetCohortTable = "comorbidities",
        window = c(-Inf, -1)
      )
    ),
    cohortIntersectCount = list(
      "count_any_time_prior_vaccination" = list(
        targetCohortTable = "othervaccines",
        window = c(-Inf, -1)
      ),
      "count_last_year_vaccination" = list(
        targetCohortTable = "othervaccines",
        window = c(-365, -1)
      )
    ),
    otherVariables = c("region", "ethnicity", "sex", "imd",
                       "immunosuppressed", "age_eligibility", "prior_dose"),
    estimates = list(immunosuppressed = c("count", "percentage"), 
                     age_eligibility = c("count", "percentage"))
  )


