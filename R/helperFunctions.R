setYearsToDispaly <- function(years, dataARL) {
  # Purpose: A helper function to return years to display
  # based on user input


  # If NA, then user wants program to select the years
  if (all(is.na(years) == TRUE)) {
    # Testing phrases
    # cat("\n Run condition 1")

    # Obtain years in uploaded data
    yearsInData <- dataARL$Year %>%
      unique() %>%
      sort(decreasing = FALSE)

    # Based on length of years in uploaded data, select last 5 or less years
    if (length(yearsInData) == 5 || length(yearsInData) < 4) {
      yearsToDisplay <- years
    } else if(length(yearsInData) > 5) {
      yearsToDisplay <-
        yearsInData[(length(yearsInData) - 4):length(yearsInData)]
    }
  } else if(is.numeric(years) != TRUE) {
    # Testing phrases
    # cat("\n Run condition 2")
    stop("Argument years should be set as a vector of numeric data
         containing 5 years or set to NA")
  } else if(is.vector(years) != TRUE) {
    # Testing phrases
    # cat("\n Run condition 2")
    stop("Argument years should be set as a vector of numeric data
         containing 5 years or set to NA")
  } else {
    # Testing phrases
    # cat("\n Run condition 3")
    # If more than 5 years provided by user
    if(length(years) > 5) {
      warning("More than five years provided in argument years. Most
              recent 5 years will be used.")
      yearsToDisplay <- years %>%
        unique() %>%
        sort(decreasing = FALSE) %>%
        tail(5)

    } else {
      # Obtain data for last 5 years
      yearsToDisplay <- years
    }
  }
  return(yearsToDisplay)
}

setMemebersToDispaly <- function(years) {
  # Purpose: A helper function to return members to display
  # based on user input

  # Check input
  # Obtain members in data
  if(is.character(members) != TRUE) {
    stop("The members argument should be a character vector specifying
         up to 5 ARL member institutes.")
  } else if(is.vector(members) != TRUE) {
    stop("The members argument should be a character vector specifying
         up to 5 ARL member institutes.")
  }

  # obtain unique members and
  membersInData <- members %>%
    unique() %>%
    sort()


  # If NA, then user wants program to select the years
  if (all(is.na(years) == TRUE)) {
    # Testing phrases
    # cat("\n Run condition 1")
    # Based on length of years in data, select last 5 or less years
    if (length(yearsInData) == 5 || length(yearsInData) < 4) {
      yearsToDisplay <- years
    } else if(length(yearsInData) > 5) {
      yearsToDisplay <-
        yearsInData[(length(yearsInData) - 4):length(yearsInData)]
    }
  } else if (is.numeric(years) != TRUE) {
    # Testing phrases
    # cat("\n Run condition 2")
    stop("Argument years should be set as a vector of numeric data
         containing 5 years or set to NA")
  } else {
    # Testing phrases
    # cat("\n Run condition 3")
    # If more than 5 years provided by user
    if(length(years) > 5) {
      warning("More than five years provided in argument years. Most
              recent 5 years will be used.")
      yearsTrucated <- years %>%
        unique() %>%
        sort(decreasing = FALSE) %>%
        tail(5)
      # Obtain data for last 5 years
      yearsToDisplay <-
        yearsInData[(length(yearsInData) - 4):length(yearsInData)]
    } else {
      yearsToDisplay <- sort(years, decreasing = FALSE)
    }
  }
  return(yearsToDisplay)
}

setColorPalette <- function(returnCol = TRUE) {
  # Purpose: Set the color palette

  if(returnCol == TRUE) {
    # set color palette
    colorPaletteCustom <- c(
      '#2166ac',
      'red',
      'darkgrey',
      '#c51b7d',
      '#ff7f00',
      '#8073ac',
      '#b15928',
      '#fee08b',
      '#a6cee3',
      '#dfc27d',
      '#e6f598',
      '#f1b6da',
      '#d1e5f0',
      'magenta',
      '#f4a582',
      '#5e4fa2',
      '#ccebc5',
      '#cab2d6',
      '#3288bd',
      '#fddbc7',
      'black')
    return(colorPaletteCustom)

  } else {
    # no return
  }
}

dataAdjustment <- function(dataARL, years = NA, members) {
  # Purpose: A function that takes all data supplied by the user
  # and filter it for values being displayed by the current package
  # and years selected by the user


  yearsToDisplay <- setYearsToDispaly(years = years,
                                      dataARL = dataARL)
  # Phrases for testing purposes
  # cat("\n Years provided by user are:", years, "\n")
  # cat("\n Years to analyze are:", yearsToDisplay, "\n")


  selectedData <- dataARL %>%
    dplyr::select(
      "Year",
      "Institution Name",
      "Institution type",
      "Region",
      "Rank in ARL investment index",
      "ARL investment index value",
      "Titles held",
      "Volumes held",
      "Electronic books",
      "Total library expenditures",
      "Total materials expenditures",
      "Total salaries & wages",
      "Other operating expenditures",
      "Canadian dollar exchange rate",
      "Professional staff",
      "Support staff",
      "Student assistants",
      "Total prof. + support + student staff",
      "Total salaries & wages",
      "Professional salaries & wages",
      "Support staff salaries & wages",
      "Student assistant wages",
      "Number of successful full-text article requests (journals)",
      "Total teaching faculty",
      "Total fulltime students",
      "Part-time students, undergraduate and graduate",
      "Total fulltime graduate students",
      "Part-time graduate students",
      "Doctor's degree fields",
      "Doctor's degrees awarded",
      "Group presentations",
      "Presentation participants") %>%
    dplyr::mutate_at(
      c('Titles held',
        'Volumes held',
        'Electronic books',
        'Total library expenditures',
        'Total materials expenditures',
        'Total salaries & wages',
        'Other operating expenditures',
        'Canadian dollar exchange rate',
        "Professional staff",
        "Support staff",
        "Student assistants",
        "Total prof. + support + student staff",
        "Total salaries & wages",
        "Professional salaries & wages",
        "Support staff salaries & wages",
        "Student assistant wages",
        "Number of successful full-text article requests (journals)",
        "Total teaching faculty",
        "Total fulltime students",
        "Part-time students, undergraduate and graduate",
        "Total fulltime graduate students",
        "Part-time graduate students",
        "Doctor's degree fields",
        "Doctor's degrees awarded",
        "Group presentations",
        "Presentation participants"), as.numeric) %>%
    dplyr::mutate('Total library expenditures (CAD)' = `Total library expenditures` * `Canadian dollar exchange rate`)  %>%
    dplyr::mutate('Total materials expenditures (CAD)' = `Total materials expenditures` * `Canadian dollar exchange rate`)  %>%
    dplyr::mutate('Total salaries & wages (CAD)' = `Total salaries & wages` * `Canadian dollar exchange rate`)  %>%
    dplyr::mutate('Other operating expenditures (CAD)' = `Other operating expenditures` * `Canadian dollar exchange rate`) %>%
    dplyr::filter(`Year` %in% c(yearsToDisplay))

  return(selectedData)

}
