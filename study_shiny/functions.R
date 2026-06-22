backgroundCard <- function(fileName) {
  # read file
  content <- readLines(fileName)

  # extract yaml metadata
  # Find the positions of the YAML delimiters (----- or ---)
  yamlStart <- grep("^---|^-----", content)[1]
  yamlEnd <- grep("^---|^-----", content)[2]

  if (any(is.na(c(yamlStart, yamlEnd)))) {
    metadata <- NULL
  } else {
    # identify YAML block
    id <- (yamlStart + 1):(yamlEnd - 1)
    # Parse the YAML content
    metadata <- yaml::yaml.load(paste(content[id], collapse = "\n"))
    # eliminate yaml part from content
    content <- content[-(yamlStart:yamlEnd)]
  }

  tmpFile <- tempfile(fileext = ".md")
  writeLines(text = content, con = tmpFile)

  # metadata referring to keys
  backgroundKeywords <- list(
    header = "bslib::card_header",
    footer = "bslib::card_footer"
  )
  keys <- names(backgroundKeywords) |>
    rlang::set_names() |>
    purrr::map(\(x) {
      if (x %in% names(metadata)) {
        paste0(backgroundKeywords[[x]], "(metadata[[x]])") |>
          rlang::parse_expr() |>
          rlang::eval_tidy()
      } else {
        NULL
      }
    }) |>
    purrr::compact()

  arguments <- c(
    # metadata referring to arguments of card
    metadata[names(metadata) %in% names(formals(bslib::card))],
    # content
    list(
      keys$header,
      bslib::card_body(shiny::HTML(suppressWarnings(markdown::markdownToHTML(
        file = tmpFile, fragment.only = TRUE
      )))),
      keys$footer
    ) |>
      purrr::compact()
  )

  unlink(tmpFile)

  do.call(bslib::card, arguments)
}
pretty_labels <- function(x, nm = NULL) {
  map <- c(
    a_2023 = "Autumn 2023",
    s_2024 = "Spring 2024",
    a_2024 = "Autumn 2024",
    s_2025 = "Spring 2025",
    covid_vaccine = "Covid vaccine",
    ethnicity = "Ethnicity",
    sex = "Sex",
    prior_dose = "Prior dose",
    imd = "IMD",
    age_eligibility = "Age eligibility",
    immunosuppressed = "Immunosuppressed",
    age_group = "Age group",
    overall = "Overall",
    region = "Region",
    Q1 = "Q1 (least deprived)",
    Q5 = "Q5 (most deprived)",
    #vaccine_washout = "Vaccination records of the overall population",
    #vaccinated_within_campaigns = "Vaccinated eligibles",
    #all_campaigns = "Eligibles for vaccination",
    `0` = "No",
    `1` = "Yes",
    missing = "Missing",
    asian = "Asian",
    black = "Black",
    white = "White"
  )
  
  vals <- as.character(x)
  
  if (!is.null(nm) && nm %in% c("age_eligibility", "immunosuppressed")) {
    vals <- dplyr::recode(vals, `0` = "No", `1` = "Yes", .default = vals)
  }
  
  dplyr::recode(vals, !!!map, .default = vals)
}
summaryCdmName <- function(data) {
  if (length(data) == 0) {
    return(list("<b>CDM names</b>" = ""))
  }
  x <- data |>
    purrr::map(\(x) {
      x |>
        dplyr::group_by(.data$cdm_name) |>
        dplyr::summarise(number_rows = dplyr::n(), .groups = "drop")
    }) |>
    dplyr::bind_rows() |>
    dplyr::group_by(.data$cdm_name) |>
    dplyr::summarise(
      number_rows = as.integer(sum(.data$number_rows)),
      .groups = "drop"
    ) |>
    dplyr::mutate(label = paste0(.data$cdm_name, " (", .data$number_rows, ")")) |>
    dplyr::pull("label") |>
    rlang::set_names() |>
    as.list()
  list("<b>CDM names</b>" = x)
}
summaryPackages <- function(data) {
  if (length(data) == 0) {
    return(list("<b>Packages versions</b>" = ""))
  }
  x <- data |>
    purrr::map(\(x) {
      x |>
        omopgenerics::addSettings(
          settingsColumn = c("package_name", "package_version")
        ) |>
        dplyr::group_by(.data$package_name, .data$package_version) |>
        dplyr::summarise(number_rows = dplyr::n(), .groups = "drop") |>
        dplyr::right_join(
          omopgenerics::settings(x) |>
            dplyr::select(c("package_name", "package_version")) |>
            dplyr::distinct(),
          by = c("package_name", "package_version")
        ) |>
        dplyr::mutate(number_rows = dplyr::coalesce(.data$number_rows, 0))
    }) |>
    dplyr::bind_rows() |>
    dplyr::group_by(.data$package_name, .data$package_version) |>
    dplyr::summarise(
      number_rows = as.integer(sum(.data$number_rows)),
      .groups = "drop"
    ) |>
    dplyr::group_by(.data$package_name) |>
    dplyr::group_split() |>
    as.list()
  lab <- "<b>"
  names(x) <- x |>
    purrr::map_chr(\(x) {
      if (nrow(x) > 1) {
        lab <<- "<b style='color:red'>"
        paste0("<b style='color:red'>", unique(x$package_name), " (Multiple versions!) </b>")
      } else {
        paste0(
          x$package_name, " (version = ", x$package_version,
          "; number records = ", x$number_rows,")"
        )
      }
    })
  x <- x |>
    purrr::map(\(x) {
      if (nrow(x) > 1) {
        paste0(
          "version = ", x$package_version, "; number records = ",
          x$number_rows
        ) |>
          rlang::set_names() |>
          as.list()
      } else {
        x$package_name
      }
    })
  list(x) |>
    rlang::set_names(nm = paste0(lab, "Packages versions</b>"))
}
summaryMinCellCount <- function(data) {
  if (length(data) == 0) {
    return(list("<b>Min Cell Count Suppression</b>" = ""))
  }
  x <- data |>
    purrr::map(\(x) {
      x |>
        omopgenerics::addSettings(settingsColumn = "min_cell_count") |>
        dplyr::group_by(.data$min_cell_count) |>
        dplyr::summarise(number_rows = dplyr::n(), .groups = "drop") |>
        dplyr::right_join(
          omopgenerics::settings(x) |>
            dplyr::select("min_cell_count") |>
            dplyr::distinct(),
          by = "min_cell_count"
        ) |>
        dplyr::mutate(number_rows = dplyr::coalesce(.data$number_rows, 0))
    }) |>
    dplyr::bind_rows() |>
    dplyr::group_by(.data$min_cell_count) |>
    dplyr::summarise(
      number_rows = as.integer(sum(.data$number_rows)),
      .groups = "drop"
    ) |>
    dplyr::mutate(min_cell_count = as.integer(.data$min_cell_count)) |>
    dplyr::arrange(.data$min_cell_count) |>
    dplyr::mutate(
      label = dplyr::if_else(
        .data$min_cell_count == 0L,
        "<b style='color:red'>Not censored</b>",
        paste0("Min cell count = ", .data$min_cell_count)
      ),
      label = paste0(.data$label, " (", .data$number_rows, ")")
    ) |>
    dplyr::pull("label") |>
    rlang::set_names() |>
    as.list()
  lab <- ifelse(any(grepl("Not censored", unlist(x))), "<b style='color:red'>", "<b>")
  list(x) |>
    rlang::set_names(nm = paste0(lab, "Min Cell Count Suppression</b>"))
}
summaryPanels <- function(data) {
  if (length(data) == 0) {
    return(list("<b>Panels</b>" = ""))
  }
  
  x <- data |>
    purrr::map(\(x) {
      if (nrow(x) == 0) {
        res <- omopgenerics::settings(x) |>
          dplyr::select(!c(
            "result_id", "package_name", "package_version", "group", "strata",
            "additional", "min_cell_count"
          )) |>
          dplyr::relocate("result_type") |>
          as.list() |>
          purrr::imap(\(values, nm) {
            labels <- pretty_labels(values, nm = nm)
            out <- paste0(labels, " (number rows = ", length(values), ")")
            rlang::set_names(as.list(out), out)
          })
      } else {
        sets <- c("result_type", omopgenerics::settingsColumns(x))
        res <- x |>
          omopgenerics::addSettings(settingsColumn = sets) |>
          dplyr::relocate(dplyr::all_of(sets)) |>
          omopgenerics::splitAll() |>
          dplyr::select(!c(
            "variable_name", "variable_level", "estimate_name",
            "estimate_type", "estimate_value", "result_id"
          )) |>
          as.list() |>
          purrr::imap(\(values, nm) {
            tab <- table(values)
            labels <- pretty_labels(names(tab), nm = nm)
            out <- paste0(labels, " (number rows = ", as.integer(tab), ")")
            rlang::set_names(as.list(out), out)
          })
      }
      res
    })
  
  list(x) |>
    rlang::set_names(nm = "<b>Panels</b>")
}
simpleTable <- function(result,
                        header = character(),
                        group = character(),
                        hide = character()) {
  # initial checks
  if (length(header) == 0) header <- character()
  if (length(group) == 0) group <- NULL
  if (length(hide) == 0) hide <- character()

  if (nrow(result) == 0) {
    return(gt::gt(dplyr::tibble()))
  }

  result <- result |>
    omopgenerics::addSettings() |>
    #omopgenerics::splitAll() |>
    dplyr::select(-"result_id")

  # format estimate column
  formatEstimates <- c(
    "N (%)" = "<count> (<percentage>%)",
    "N" = "<count>",
    "median [Q25 - Q75]" = "<median> [<q25> - <q75>]",
    "mean (SD)" = "<mean> (<sd>)",
    "[Q25 - Q75]" = "[<q25> - <q75>]",
    "range" = "[<min> <max>]",
    "[Q05 - Q95]" = "[<q05> - <q95>]"
  )
  result <- result |>
    visOmopResults::formatEstimateValue(
      decimals = c(integer = 0, numeric = 1, percentage = 0)
    ) |>
    visOmopResults::formatEstimateName(estimateName = formatEstimates) |>
    suppressMessages() |>
    visOmopResults::formatHeader(header = header) |>
    dplyr::select(!dplyr::any_of(c("estimate_type", hide)))
  if (length(group) > 1) {
    id <- paste0(group, collapse = "; ")
    result <- result |>
      tidyr::unite(col = !!id, dplyr::all_of(group), sep = "; ", remove = TRUE)
    group <- id
  }
  result <- result |>
    visOmopResults::formatTable(groupColumn = group)
  return(result)
}
tidyDT <- function(x, columns, pivotEstimates) {
  # x is already split (single strata_name/strata_level per row)
  # so skip splitAll() / addSettings()
  
  # remove density
  x <- x |>
    dplyr::filter(!.data$estimate_name %in% c("density_x", "density_y"))
  
  # estimate columns
  if (pivotEstimates) {
    estCols <- unique(x$estimate_name)
    x <- x |>
      omopgenerics::pivotEstimates()
  } else {
    estCols <- c("estimate_name", "estimate_type", "estimate_value")
  }
  
  # order columns — define explicitly since splitAll() no longer provides column groups
  allCols <- list(
    "CDM name"   = "cdm_name",
    "Group"      = "group_level",
    "Strata"     = c("strata_name", "strata_level"),
    "Variable"   = c("variable_name", "variable_level")
  ) |>
    purrr::map(\(x) x[x %in% columns]) |>
    purrr::compact()
  
  allCols[["Estimates"]] <- estCols
  
  x <- x |>
    dplyr::select(dplyr::all_of(unname(unlist(allCols))))
  
  # prepare the header
  container <- shiny::tags$table(
    class = "display",
    shiny::tags$thead(
      purrr::imap(allCols, \(x, nm) shiny::tags$th(colspan = length(x), nm)) |>
        shiny::tags$tr(),
      shiny::tags$tr(purrr::map(unlist(allCols), shiny::tags$th))
    )
  )
  
  DT::datatable(
    data = x,
    filter = "top",
    container = container,
    rownames = FALSE,
    options = list(searching = FALSE)
  )
}
filterResult <- function(result, filt) {

  q <- function(nm) {
    paste0(".data$", nm, " %in% filt[[\"", nm, "\"]]") |> # "\" escapes the character mode
      rlang::parse_exprs() |>
      rlang::eval_tidy()
  }

  # filter columns
  cols <- c("cdm_name", "variable_name", "variable_level", "estimate_name")
  cols <- cols[cols %in% names(filt)]
  for (nm in cols) {
    result <- dplyr::filter(result, !!!q(nm)) # because we define nm in the loop
  } 

  # filter settings
  cols <- c(
    "result_id", "result_type", "package_name", "package_version",
    "min_cell_count", omopgenerics::settingsColumns(result = result)
  )
  cols <- cols[cols %in% names(filt)]
  for (nm in cols) {
    result <- omopgenerics::filterSettings(result, !!!q(nm))
  }

  # filter group
  cols <- omopgenerics::groupColumns(result = result)
  cols <- cols[cols %in% names(filt)]
  for (nm in cols) {
    result <- omopgenerics::filterGroup(result, !!!q(nm))
  }

  # filter strata
  cols <- omopgenerics::strataColumns(result = result)
  cols <- cols[cols %in% names(filt)]
  for (nm in cols) {
    result <- omopgenerics::filterStrata(result, !!!q(nm))
  }

  # filter additional
  cols <- omopgenerics::additionalColumns(result = result)
  cols <- cols[cols %in% names(filt)]
  for (nm in cols) {
    result <- omopgenerics::filterAdditional(result, !!!q(nm))
  }

  # correct settings
  set <- omopgenerics::settings(result)
  # remove columns that all are NA
  cols <- colnames(set) |>
    purrr::keep(\(x) any(!is.na(x)))
  set <- set |>
    dplyr::select(dplyr::all_of(cols))
  # replace NA for '-NA-'
  set <- set |>
    dplyr::mutate(dplyr::across(
      .cols = dplyr::where(is.character),
      .fns = \(x) dplyr::coalesce(x, "-NA-")
    ))

  # final result
  omopgenerics::newSummarisedResult(x = result, settings = set)
}
getValues <- function(result, resultList) {
  resultList |>
    purrr::imap(\(x, nm) {
      res <- filterResult(result, x)
      
      values <- res |>
        dplyr::select(!c("estimate_type", "estimate_value")) |>
        dplyr::distinct() |>
        omopgenerics::splitAll() |>
        dplyr::select(!"result_id") |>
        as.list() |>
        purrr::map(\(x) sort(unique(x)))
      
      valuesSettings <- omopgenerics::settings(res) |>
        dplyr::select(!dplyr::any_of(c(
          "result_id", "result_type", "package_name", "package_version",
          "group", "strata", "additional", "min_cell_count"
        ))) |>
        as.list() |>
        purrr::map(\(x) sort(unique(x[!is.na(x)]))) |>
        purrr::compact()
      
      values <- c(values, valuesSettings)
      
      values <- purrr::imap(values, \(vec, col) {
        if (col %in% c("cohort_name", "table_name", "vaccination_campaign",
                       "region", "imd", "sex", "ethnicity", "prior_dose",
                       "age_group", "age_eligibility", "immunosuppressed")) {
          pretty_labels(vec)
        } else {
          vec
        }
      })
      
      names(values) <- paste0(nm, "_", names(values))
      values
    }) |>
    purrr::flatten()
}
getSelected <- function(choices) {
  purrr::imap(choices, \(vals, nm) {
    if (grepl("_denominator_sex$", nm)) {
      if ("Both" %in% vals) return("Both")
      return(vals[[1]])
    }

    if (grepl("_denominator_age_group$", nm)) {
      bounds <- regmatches(vals, regexec("^(\\d+) to (\\d+)$", vals))
      valid <- vapply(bounds, length, integer(1)) == 3
      if (any(valid)) {
        ranges <- vapply(bounds[valid], \(x) as.numeric(x[3]) - as.numeric(x[2]), numeric(1))
        return(vals[valid][[which.max(ranges)]])
      } else {
        return(vals[[1]])
      }
    }

    if (grepl("_outcome_cohort_name$", nm)) {
      return(vals[[1]])
    }

    vals
  })
}

renderInteractivePlot <- function(plt, interactive) {
  if (interactive) {
    plotly::renderPlotly(
      plotly::ggplotly(plt) |>
        plotly::layout(height = 600) |>
        plotly::config(scrollZoom = TRUE)
    )
  } else {
    shiny::renderPlot(plt, height = 600)
  }
}

updateMessage <- shiny::div(
  style = "font-size: 8pt; color: var(--bs-danger);",
  shiny::icon("circle-exclamation"),
  "Filters have changed please consider to use the update content button!"
)

#plots for coverage

