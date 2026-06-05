att_vac<-summariseCohortAttrition(cdm$vaccinated_within_campaigns)
att_vac1<-summariseCohortAttrition(cdm$vaccine_washout)
summariseCohortAttrition(cdm$demo)|>plotCohortAttrition(show = c("subjects"))
attr_vac <- att_vac |> 
  filter(!additional_level %in% c(1L, 4L))|> 
  mutate(strata_level = if_else(additional_level ==3L, 
                                "In observation during campaign of interest",
                                strata_level))|> filter(group_level=="a_2023") |>
  plotCohortAttrition(show = c("subjects"))
  
  
att_vaccinecamp <-att_vac |>
  omopgenerics::splitAll() |>
  select(cohort_name, reason, variable_name,  estimate_value) |>
  tidyr:: pivot_wider(names_from = variable_name, values_from = estimate_value) |>
  mutate(
    cohort_name = factor(
      cohort_name, 
      levels = c("a_2023", "s_2024", "a_2024", "s_2025"), 
      labels = c("Autumn 2023", "Spring 2024",
                 "Autumn 2024", "Spring 2025"))) |>
  dplyr::rename(
    `Number records` = number_records,
    `Number subjects` = number_subjects,
    `Excluded records` = excluded_records,
    `Excluded subjects` = excluded_subjects,
    Reason = reason
  ) |>
  rename_with(~ gsub("_", " ", .x)) |>
  mutate(
    across(
      where(is.character),
      ~ gsub("_", " ", .x)
    )
  )
  tt(att_vaccinecamp) |>
  group_tt(i = att_vaccinecamp$`cohort name`) |>
  subset(select = -`cohort name`) |>
  # style_tt(i = 1:8, background = "#F2F2F2") |>
  # style_tt(i = 17:24, background = "#F2F2F2") |>
  style_tt(i = "groupi", align = "l", line = "b", background = "lightgray", bold = FALSE) |>
  style_tt(i = groupi - 1, line = "b") |>
  theme_latex(environment = "tblr", placement = "H", multipage = FALSE) |>
    print(output= "latex")

eligibles <-  characterisation |>
    splitAll() |>
    filter(
      variable_name %in% c("Immunosuppressed", "Age eligibility"),
      estimate_name %in% c("count", "percentage")
    ) |>
    select(result_id, cdm_name, cohort_name, variable_name, estimate_name, estimate_value) |>
    tidyr::pivot_wider(
      names_from = estimate_name,
      values_from = estimate_value
    ) |>
    mutate(
      count = as.numeric(count),
      percentage = as.numeric(percentage),
       `N(%)` = paste0(count, " (", round(percentage, 1), "%)")
    ) 

elig_tab <- eligibles |>
  group_by(cohort_name) |>
  mutate(
    cohort_name = factor(
      cohort_name, 
      levels = c("a_2023", "s_2024", "a_2024", "s_2025"), 
      labels = c("Autumn 2023", "Spring 2024",
                 "Autumn 2024", "Spring 2025")),
    cohort_name = if_else(row_number() == 1, cohort_name, "")) |>
  select(cohort_name, variable_name, `N(%)`) |>
  rename(`Cohort name` = cohort_name,
         `Variable name` = variable_name)|>
  tt()|>
  theme_striped() |>
  print("latex")
  
demo <- characterisation|>
  splitAll() |>
  filter(variable_name %in% c("Sex", "Imd", "Region", "Ethnicity"))|>select(-estimate_type) |>tidyr::pivot_wider(
  names_from = estimate_name,
  values_from = estimate_value
) |>
  mutate(
    count = as.numeric(count),
    percentage = as.numeric(percentage),
    `N(%)` = paste0(count, " (", sprintf("%.2f", percentage), "%)")
  ) |>
  mutate(variable_level = factor(
    variable_level,
    levels = c("Female", "Male",
               "Asian", "Black", "White", "Missing",
               "Northern ireland", "England", "Scotland", "Wales",
               "Q1", "Q2", "Q3", "Q4", "Q5"),
    ordered = TRUE
  )) |>
  arrange(variable_level) |>
  collect(name = "demo")

demo_table <- demo  |>
  filter(count>=5) |>
  group_by(cohort_name, variable_name) |>
  mutate(
    cohort_name = factor(
      cohort_name, 
      levels = c("a_2023", "s_2024", "a_2024", "s_2025"), 
      labels = c("Autumn 2023", "Spring 2024",
                 "Autumn 2024", "Spring 2025")),
    variable_name = if_else(row_number() == 1, variable_name, "")) |>
  ungroup() |>
  select(variable_name, variable_level, cohort_name, `N(%)`) |>
  tidyr::pivot_wider(
    names_from = cohort_name,
    values_from = `N(%)`
  ) |>
  rename(`Variable name` = variable_name,
         `Variable level` = variable_level)|>
  tt()|>
  group_tt(j= list("N(%)" =3:6)) |>
  style_tt(i = c(1: 2), background = "#EDEDED") |>
  style_tt(i = c(7: 10), background = "#EDEDED") |>
  print("latex")

vaccines <- characterisation|>
  splitAll() |>
  filter(variable_name %in% c("Flag any time prior vaccination", "Flag last year vaccination", "Flag on index vaccination"))|>
  select(-estimate_type) |>
  tidyr::pivot_wider(
    names_from = estimate_name,
    values_from = estimate_value
  ) |>
  mutate(
    count = as.numeric(count),
    percentage = as.numeric(percentage),
    `N(%)` = paste0(count, " (", sprintf("%.2f", percentage), "%)"),
    `N(%)`= if_else(count<5, "<5", `N(%)`),
    variable_name = stringr::str_remove(variable_name, "^Flag "),
    variable_name = stringr::str_to_sentence(variable_name))|>
  mutate(
    cohort_name = factor(
      cohort_name, 
      levels = c("a_2023", "s_2024", "a_2024", "s_2025"), 
      labels = c("Autumn 2023", "Spring 2024",
                 "Autumn 2024", "Spring 2025"))) |>
  ungroup() |>
  select(variable_name, variable_level, cohort_name, `N(%)`) |>
  tidyr::pivot_wider(
    names_from = cohort_name,
    values_from = `N(%)`
  ) |>
  #group_by(variable_name) |>
  #mutate(variable_name = if_else(row_number() == 1, variable_name, "")) |>
  #ungroup()|>
  rename(`Variable name` = variable_name,
         `Variable level` = variable_level)

vaccines|>
  tt()|>
  group_tt(i = vaccines$`Variable name`) |>
  subset(select = -`Variable name`) |>
  # style_tt(i = c(1: 2), background = "#EDEDED") |>
  # style_tt(i = c(7: 10), background = "#EDEDED") |>
  style_tt(i = "groupi", align = "l", line = "b", background = "#EDEDED", bold = FALSE) |>
  style_tt(i = groupi - 1, line = "b") |>
  style_tt(i = groupi - 1, line = "b") |>
  print("latex")

comorb <- characterisation|>
  splitAll() |>
  filter(variable_name == "Flag any time prior comorbidities")|>
  select(-estimate_type) |>
  tidyr::pivot_wider(
    names_from = estimate_name,
    values_from = estimate_value
  ) |>
  mutate(
    count = as.numeric(count),
    percentage = as.numeric(percentage),
    `N(%)` = paste0(count, " (", sprintf("%.2f", percentage), "%)"),
    `N(%)`= if_else(count<5, "<5", `N(%)`),
    variable_name = stringr::str_remove(variable_name, "^Flag "),
    variable_name = stringr::str_to_sentence(variable_name))|>
  mutate(
    cohort_name = factor(
      cohort_name, 
      levels = c("a_2023", "s_2024", "a_2024", "s_2025"), 
      labels = c("Autumn 2023", "Spring 2024",
                 "Autumn 2024", "Spring 2025"))) |>
  select(variable_level, cohort_name, `N(%)`) |>
  tidyr::pivot_wider(
    names_from = cohort_name,
    values_from = `N(%)`
  ) |>
  #group_by(variable_name) |>
  #mutate(variable_name = if_else(row_number() == 1, variable_name, "")) |>
  #ungroup()|>
  rename(`Comorbidities` = variable_level)

comorb|>
  tt()|>
  print("latex")
  