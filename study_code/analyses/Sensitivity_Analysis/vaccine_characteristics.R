# Objective 1: Characterisation:
# All of the vaccinated people stratified for all campaigs, where the 2 dosis 
# filter is considered, as well as comorbidities and other vaccine administrations
characterisation_sens <- cdm$vaccinated_within_campaigns_sens |>
  VaccineCharacterisation()

characterisation_eligibles_sens <- cdm$all_campaigns_sens |>
  VaccineCharacterisation(estimates=c("region", "ethnicity", "sex", "imd",
                                      "immunosuppressed", "age_eligibility", 
                                      "prior_dose", "age_group"))

