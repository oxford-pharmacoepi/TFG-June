# All of the vaccinated people stratified for all campaigs, where the 2 dosis 
# filter is considered
characterisation <- cdm$vaccinated_within_campaigns |>
  summariseCharacteristics(
    tableIntersectCount = list(
      "Number visits in the prior year" = list(
        tableName = "visit_occurrence",
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
    tableIntersectCount = list(
      "Number visits in the prior year" = list(
        tableName = "visit_occurrence",
        window = c(-365, -1)
      )
    ),
    otherVariables = c("region", "ethnicity", "sex", "imd",
                       "immunosuppressed", "age_eligibility", "prior_dose"),
    estimates = list(immunosuppressed = c("count", "percentage"), 
                     age_eligibility = c("count", "percentage"))
  )


