# Unique function to characterise 

VaccineCharacterisation <- function(cohort){
  cohort |>
    summariseCharacteristics(
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
                         "immunosuppressed", "age_eligibility", 
                         "prior_dose", "vaccine_dose", "age_group"),
      estimates = list(immunosuppressed = c("count", "percentage"), 
                       age_eligibility = c("count", "percentage"))
    )
}
