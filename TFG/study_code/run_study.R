omopgenerics::assertNumeric(min_cell_count)

# Create a log file ----
createLogFile(logFile = here("Results", "log_{date}_{time}"))
logMessage("LOG CREATED")

# Define analysis settings -----
study_period <- c(as.Date(NA), as.Date(NA))

# Initialise list to store results as we go -----
results <- list()

# CDM modifications -----

# CDM summary -----
logMessage("Extract CDM snapshot") 
results[["snapshot"]] <- summariseOmopSnapshot(cdm)

logMessage("Extract observation period summary") 
results[["obs_period"]] <- summariseObservationPeriod(cdm$observation_period)

# Instantiate study cohorts ----
logMessage("Instantiating study cohorts")

diagnosis <- importCodelist("codelist", type = "csv")
comorbidities <- importCodelist(here::here("codelist", "comorbidities"), type = "csv")
immuno <- importCodelist(here::here("codelist", "immuno"), type = "csv")
vaccines <- importCodelist(here::here("codelist", "vaccines"), type = "csv")
source(here("functions.R"))
logMessage("Codelists and functions to be used imported")

source(here("cohorts", "instantiate_cohorts.R")) 
logMessage("Vaccinated people identified by campaign")

source(here("cohorts", "all_campaign.R")) 
logMessage("Eligibles for each of the vaccination campaigns -either for being immunosuppressed or by age- 
           stratified by age, ethnicity, IMD, sex and region. Will be used for coverage")  
logMessage("Study cohorts instantiated")

source(here("cohorts", "vaccine_cohort.R"))
logMessage("Vaccinated people within the vaccination campaigns of interest -either for being immunosuppressed or by age-") 

# Cohort counts and attrition ----
results[["attrition_vaccinated"]] <- summariseCohortAttrition(cdm$vaccinated_within_campaigns)
results[["attrition_for_coverage"]] <- summariseCohortAttrition(cdm$all_campaigns)
logMessage("Attritions by campaign and for coverage finished")

# Run analyses ----
logMessage("Run study analyses")
source(here("analyses", "vaccine_characteristics.R"))
logMessage("Analyses for the vaccinated people and eligibles for each campaign done")

source(here("analyses", "coverage.R"))
logMessage("Coverage analysis finished")

logMessage("Analyses finished")

# Capture log file ----
results[["log"]] <- summariseLogFile(cdmName = omopgenerics::cdmName(cdm))

# Finish ----
results$characterisation <- characterisation
results$characterisation_eligibles <- characterisation_eligibles
results$summary_campaigns <- summary_campaigns 

#omopgenerics::tidy(results$summary_campaign1)

results <- results |>
  vctrs::list_drop_empty() |>
  omopgenerics::bind()
exportSummarisedResult(results,
                       minCellCount = min_cell_count,
                       fileName = "results_{cdm_name}_{date}.csv",
                       path = here("Results"))

# Results to save as csv and plot ----
# Save data for the local plots of the vaccination chronology 
#(see "vaccination_chronology" for more info). Should be computed once
source(here("analyses", "vaccination_chronology.R"))
write.csv(x_dose, "Results/plot_dose.csv", row.names = FALSE)

cli::cli_alert_success("Study finished")

