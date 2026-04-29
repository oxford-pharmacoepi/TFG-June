# Lineal Regression to analyse the relation between ethnicity, sex, region,
# age group, quintile (and number of doses??) within vaccinated and unvaccinated individuals
df <- cdm$all_campaigns |>
  select(vaccinated, age_group, immunosuppressed, imd, ethnicity, region, sex, prior_dose) |>
  collect() |>
  mutate(
    age_group = factor(age_group, levels = c("65-74", "75-84", "85-94", ">=95",
                                             "=<34", "35-45", "45-54", "55-64")),
    immunosuppressed = factor(immunosuppressed, levels = c("0", "1")),
    imd = factor(imd, levels = c("Q3", "Q1", "Q2", "Q4", "Q5")),
    ethnicity = factor(ethnicity, levels = c("white", "black", "asian", "missing")),
    region = factor(region, levels = c("Scotland", "London", "Wales", 
                                       "Northern Ireland", "South East",
                                       "Yorkshire & The Humber", "West Midlands",
                                       "North East", "North West", "East of England",
                                       "East Midlands", "South West")),
    sex = factor(sex, levels = c("Female", "Male")),
    prior_dose = factor(prior_dose, levels = as.character(2:9))
  )

for (prior in 2L:9L){
  fit_prior <- glm(
    vaccinated ~ age_group + immunosuppressed + imd + ethnicity + region + sex,
    family = binomial(link = "logit"),
    data = df_prior |>
      filter(prior_dose == prior))
}

# summary(fit)
# broom::tidy(fit)