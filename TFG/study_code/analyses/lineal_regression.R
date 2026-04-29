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
    ethnicity = factor(ethnicity, levels = c("white", "black", "asian", "mixed", "other", "missing")),
    region = factor(region, levels = c("Scotland", "England", "Wales", "Northern Ireland")),
    sex = factor(sex, levels = c("Female", "Male")),
    prior_dose = factor(prior_dose, levels = as.character(2:9))
  )

fit <- glm(
  vaccinated ~ age_group + immunosuppressed + imd + ethnicity + region + sex + prior_dose,
  family = binomial(link = "logit"),
  data = df
)

# summary(fit)
# broom::tidy(fit)