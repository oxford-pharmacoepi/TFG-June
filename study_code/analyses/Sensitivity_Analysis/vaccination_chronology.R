# Creation of a table that contains the corresponding 
# number of dosis per subject for each of his vaccines (dose),
# the number of vaccines provided each dose for each dose (n_dose_day), and 
# the number of vaccines provided at a certain day (n)
# Since the data is part of a plot (thus, collected and plotted locally), n will be substituted by five if it falls below five
x_dose <- cdm$vaccine_washout |>
  select(cohort_start_date, dose, subject_id, vaccination_campaign) |>
  group_by(cohort_start_date, dose) |>
  add_tally()|>
  rename(n_dose_day=n) |>
  ungroup() |>
  distinct(cohort_start_date, dose, .keep_all= TRUE) |>
  arrange(cohort_start_date)|>
  select(-subject_id) |>
  mutate(n_dose_day = dplyr::if_else(n_dose_day < 5, 5L, as.integer(n_dose_day))
  ) |>
  collect(name=x_dose)

# Including only elegibles
x_dosee_sens <- cdm$vaccinated_within_campaigns_sens|>
  rename(vaccination_campaign = cohort_name) |>
  select(cohort_start_date, dose, subject_id, vaccination_campaign) |>
  group_by(cohort_start_date, dose) |>
  add_tally()|>
  rename(n_dose_day=n) |>
  ungroup() |>
  distinct(cohort_start_date, dose, .keep_all= TRUE) |>
  arrange(cohort_start_date)|>
  select(-subject_id) |>
  mutate(n_dose_day = dplyr::if_else(n_dose_day < 5, 5L, as.integer(n_dose_day))
  ) |>
  collect(name=x_dose)
