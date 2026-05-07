# Lineal Regression to analyse the relation between ethnicity, sex, region,
# age group, quintile (and number of doses??) within vaccinated and unvaccinated individuals
df <- cdm$all_campaigns |>
  select(vaccinated, age_group, immunosuppressed, imd, ethnicity, region, sex, 
         prior_dose, cohort_name) |>
  collect() |>
  mutate(
    age_group = factor(age_group, levels = c("65-74", "75-84", "85-94", ">=95",
                                             "=<34", "35-45", "45-54", "55-64")),
    immunosuppressed = factor(immunosuppressed, levels = c(0L, 1L)),
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

fits <- list()
campaigns <- c("a_2023", "s_2024", "a_2024", "s_2025")
for (campaign in campaigns){
  
  df_campaign <- df |>
    filter(cohort_name == campaign)
  
  fits[[campaign]] <- list(
    
    fit_agr = glm(
      vaccinated ~ age_group,
      family = binomial(link = "logit"),
      data = df_campaign
    ),
    
    fit_agr_sex = glm(
      vaccinated ~ age_group + sex,
      family = binomial(link = "logit"),
      data = df_campaign
    ),
    
    fit_agr_sex_eth = glm(
      vaccinated ~ age_group + ethnicity + sex,
      family = binomial(link = "logit"),
      data = df_campaign
    ),
    
    fit_agr_sex_reg = glm(
      vaccinated ~ age_group + region + sex,
      family = binomial(link = "logit"),
      data = df_campaign
    ),
    
    fit_agr_sex_imd = glm(
      vaccinated ~ age_group + imd + sex,
      family = binomial(link = "logit"),
      data = df_campaign
    ),
    
    fit_agr_sex_immuno = glm(
      vaccinated ~ age_group + immunosuppressed + sex,
      family = binomial(link = "logit"),
      data = df_campaign
    ),
    
    fit_all = glm(
      vaccinated ~ age_group + immunosuppressed + imd + region + ethnicity + sex,
      family = binomial(link = "logit"),
      data = df_campaign
    )
    
  )
}

safe_tidy <- purrr::possibly(
  function(model, cohort_name, model_name) {
    broom::tidy(model, conf.int = TRUE, exponentiate = TRUE) |>
      dplyr::mutate(
        campaign = cohort_name,
        model = model_name
      )
  },
  otherwise = NULL
)

all_results <- purrr::imap_dfr(fits, function(model_list, campaign) {
  purrr::imap_dfr(model_list, function(model, model_name) {
    safe_tidy(model, campaign, model_name)
  })
})

# summary(fit)
# broom::tidy(fit)
