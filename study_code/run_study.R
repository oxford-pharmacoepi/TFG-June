##Start Analysis
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
source(here("cohorts", "functions.R"))
logMessage("Codelists and functions to be used imported")

source(here("cohorts", "instantiate_cohorts.R")) 
logMessage("Vaccinated people identified by campaign")

source(here("cohorts", "all_campaign.R")) 
logMessage("Eligibles for each of the vaccination campaigns -either for being immunosuppressed or by age- 
           stratified by age, ethnicity, IMD, sex and region. Will be used for coverage")  

source(here("cohorts", "vaccine_cohort.R"))
logMessage("Vaccinated people within the vaccination campaigns of interest -either for being immunosuppressed or by age-") 

logMessage("Study cohorts instantiated")

# Cohort counts and attrition ----
results[["attrition_vaccinated"]] <- summariseCohortAttrition(cdm$vaccine_washout)
results[["attrition_vaccinated_within_campaigns"]] <- summariseCohortAttrition(cdm$vaccinated_within_campaigns)
results[["attrition_vaccinated_within_campaigns_sens"]] <- summariseCohortAttrition(cdm$vaccinated_within_campaigns_sens)
results[["attrition_for_coverage"]] <- summariseCohortAttrition(cdm$all_campaigns)
results[["attrition_for_coverage_sens"]] <- summariseCohortAttrition(cdm$all_campaigns_sens)
logMessage("Attritions by campaign and for coverage finished")



# Run analyses ----
logMessage("Run study analyses")
source(here("analyses", "functions.R"))
logMessage("Defining reusable function to characterise")

# Run Main Analysis
source(here("analyses/Main_Analysis", "vaccine_characteristics.R"))
logMessage("Analyses for the vaccinated people and eligibles for each campaign done")

source(here("analyses/Main_Analysis", "coverage.R"))
logMessage("Coverage analysis finished")

source(here("analyses/Main_Analysis", "linear_regression.R"))
logMessage("Linear Regression Fits finished")

# Run Sensitivity Analysis
logMessage("Within the sensitivity Analysis:")
source(here("analyses/Sensitivity_Analysis", "vaccine_characteristics.R"))
logMessage("Analyses for the vaccinated people and eligibles for each campaign done")

source(here("analyses/Sensitivity_Analysis", "coverage.R"))
logMessage("Coverage analysis finished")

source(here("analyses/Sensitivity_Analysis", "linear_regression.R"))
logMessage("Linear Regression Fits finished")

# Capture log file ----
results[["log"]] <- summariseLogFile(cdmName = omopgenerics::cdmName(cdm))

# Finish Main Analysis----
results$characterisation <- characterisation
results$characterisation_eligibles <- characterisation_eligibles
results$summary_campaigns <- summary_campaigns 

results$characterisation_sens <- characterisation_sens
results$characterisation_eligibles_sens <- characterisation_eligibles_sens
results$summary_campaigns_sens <- summary_campaigns_sens

#omopgenerics::tidy(results$summary_campaign1)

result <- results |>
  vctrs::list_drop_empty() |>
  purrr::imap(\(x, nm) {
    if (grepl("sens", nm)) {
      x <- x |>
        dplyr::mutate(group_level = paste0(group_level, "_sens"))
    }
    x
  }) |>
  omopgenerics::bind()
exportSummarisedResult(result,
                       minCellCount = min_cell_count,
                       fileName = "results_{cdm_name}_{date}.csv",
                       path = here("Results"))

# Results to save as csv and plot ----
logMessage("Save data for the local plots of the vaccination chronology") 
#(see "vaccination_chronology" for more info
source(here("analyses/Main_Analysis", "vaccination_chronology.R"))

write.csv(x_dosee, "Results/plot_dosee.csv", row.names = FALSE)
write.csv(x_dose, "Results/plot_dose.csv", row.names = FALSE)
source(here("analyses/Sensitivity_Analysis", "vaccination_chronology.R"))
write.csv(x_dosee_sens, "Results/plot_dosee_sens.csv", row.names = FALSE)

logMessage("Save data for the local plots of the linear regression fits") 
source(here("analyses/Main_Analysis", "linear_regression.R"))
write.csv(all_results, "Results/all_results.csv")
source(here("analyses/Sensitivity_Analysis", "linear_regression.R"))
write.csv(all_results_sens, "Results/all_results_sens.csv")

cli::cli_alert_success("Study finished")

