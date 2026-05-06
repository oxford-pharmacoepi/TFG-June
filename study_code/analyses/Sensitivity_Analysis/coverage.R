# Denominator eligibility determined by the number of prior doses 
# (i.e., individuals will only be aligible to the dose number next to their prior's)
summary_campaigns_sens <- summariseResult(
  table = cdm$all_campaigns_sens,
  group = "cohort_name",
  strata = combineStrata(c("region", "imd", "sex", "ethnicity", "prior_dose")), 
  variables = list(c("vaccinated")),
  estimates = list(c("count", "percentage"))
  )

#visOmopResults::visOmopTable(summary_campaigns, , header = c("prior_dose"))
