# Objective 1: Characterisation:
# All of the vaccinated people stratified for all campaigs, where the 2 dosis 
# filter is considered, as well as comorbidities and other vaccine administrations
characterisation <- cdm$vaccinated_within_campaigns |>
  VaccineCharacterisation()

characterisation_eligibles <- cdm$all_campaigns |>
  VaccineCharacterisation(estimates=c("region", "ethnicity", "imd",
                                      "immunosuppressed", "age_eligibility", 
                                      "prior_dose", "age_group", "vaccinated"))

