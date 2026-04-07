# Creation of a table that contains the corresponding 
# number of dosis per subject for each of his vaccines (dose),
# the number of vaccines provided each dose for each dose (n_dose_day), and 
# the number of vaccines provided at a certain day (n)
# Since the data is part of a plot (thus, collected and plotted locally), n will be substituted by five if it falls below five
x_dose <- cdm$vaccinated_within_campaigns |>
  group_by(cohort_start_date, vaccine_dose) |>
  rename(dose = vaccine_dose) |>
  add_tally()|>
  rename(n_dose_day=n) |>
  ungroup() |>
  arrange(cohort_start_date)|>
  group_by(cohort_start_date) |>
  add_tally() |>
  collect() |>
  mutate(n = dplyr::if_else(n < 5, 5L, as.integer(n))
  ) |>
  collect(name=x_dose)
