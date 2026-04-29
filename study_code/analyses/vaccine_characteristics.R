# Objective 1: Characterisation:
# All of the vaccinated people stratified for all campaigs, where the 2 dosis 
# filter is considered, as well as comorbidities and other vaccine administrations
characterisation <- cdm$vaccinated_within_campaigns |>
  VaccineCharacterisation()

characterisation_eligibles <- cdm$all_campaigns |>
  VaccineCharacterisation()
#here there is no vaccination dose

