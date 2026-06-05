# Coverage and effectiveness of COVID-19 booster campaigns: a real-world analysis of UK primary care data

<img src="https://img.shields.io/badge/Study%20Status-Started-blue.svg" alt="Study Status: Started"/>

The repository is organised as follows:

-   `diagnostics_code` contains the different codelists used in the study.
-   `diagnostics_shiny` contains the PhenotypeR diagnostics code.
-   `study_code` contains the code to conduct the descriptive analysis
-   `study_shiny` contains the code to visualise the results in a ShinyApp

## Instructions to run the project

The template of this study has been generated using [OmopStudyBuilder](https://github.com/oxford-pharmacoepi/OmopStudyBuilder).

In order to conduct all the execution, download the entire repository (you can download as a zip folder using Code -\> Download ZIP, or you can use GitHub Desktop). However, each directory contains a `*.Rproj`, so that they can be run independently. Hence, rather than the full repository, specific directories can be downloaded and treated as a whole.

### Diagnostics

Conformed by **diagnostics_code** and  **diagnostics_shiny**, these repositories evaluate the codelists used in the study and integrate **PhenotypeR** package to facilitate visualisaton, respectively.

#### Steps

1)  Open the **diagnostics_code** folder.

2)  Make sure to open the `DiagnosticsCode.Rproj` in RStudio.

3)  Restore packages from `renv.lock`: with `renv::restore()`.

4)  Restart the R session.

5)  Open `code_to_run.R`, fill in the required fields (name of database, schema name with OMOP data, schema name to write results, table name stem for results to be saved in the result schema), and run the script.

6)  When finished, a results .csv file will be created in the **Results** folder. Share the .csv file when done.

7)  OPTIONAL: Visualize Results in a Shiny

    -   Navigate to the **diagnostics_shiny** folder and open the project file `PhenotypeRShiny.Rproj` in RStudio.
    -   You should see the project name in the top-right corner of your RStudio session.
    -   Copy the generated result files (in .csv format) into the `data/raw` folder located within the **diagnostics_shiny** folder.
    -   Open the `global.R` script in the `shiny` folder.
    -   Click the *Run App* button in RStudio to launch the local Shiny app for interactive exploration of the results.
    
### Main Study

Conformed by **study_code** and  **study_shiny**, these repositories evaluate the codelists used in the study and integrate the **Shiny** package to facilitate visualisaton, respectively.

#### Steps

1.  Make sure to open the `**MainStudy** project`StudyCode.Rproj` in RStudio.

2.  Restore packages from `renv.lock`: with `renv::restore()`.

3.  Restart the R session.

4.  Open `code_to_run.R`, fill in the required fields (name of database, schema name with OMOP data, schema name to write results, table name stem for results to be saved in the result schema), and run the script.

5.  Set flags as needed:

    -   `createCohorts <- TRUE` to instantiate and characterise the study cohorts
    -   `runModel <- TRUE` to run the model

6.  When finished, a ZIP file containing the result files will be created in the **Results** folder. Share the zipped folder when done.

7.  OPTIONAL: Visualize Results in Shiny

    -   Navigate to the **study_shiny** folder and open the project file `Studyshiny.Rproj` in RStudio.
    -   You should see the project name in the top-right corner of your RStudio session.
    -   Copy the generated result files (in .csv format) into the `data` folder located within the **study_shiny** directory.
    -   Open the `global.R` script.
    -   Click the *Run App* button in RStudio to launch the local Shiny app for interactive exploration of the results.
