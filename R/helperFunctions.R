setYearsToDispaly <- function(years) {
  # A helper function to return years to display
  # based on user input

  # Obtain years in data
  yearsInData <- years %>%
    unique() %>%
    sort(decreasing = FALSE)

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

  if(returnCol == TRUE) {
    # set color palette
    colorPaletteCustom <- c(
      '#33a02c',
      'red',
      '#a6cee3',
      '#b15928',
      'darkgrey',
      '#c51b7d',
      '#ff7f00',
      'darkgreen',
      '#fee08b',
      '#5e4fa2',
      '#ccebc5',
      '#e6f598',
      'black',
      '#f1b6da',
      '#66c2a5',
      'blue',
      'magenta',
      '#8dd3c7',
      '#3288bd',
      '#dfc27d',
      '#cab2d6')
    return(colorPaletteCustom)

  } else {
    # no return
  }
}

dataAdjustment <- function(dataARL, institute, years = NA) {

  yearsToDisplay <- setYearsToDispaly(years = years)
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
      "Student assistant wages") %>%
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
        "Student assistant wages"), as.numeric) %>%
    dplyr::mutate('Total library expenditures (CAD)' = `Total library expenditures` * `Canadian dollar exchange rate`)  %>%
    dplyr::mutate('Total materials expenditures (CAD)' = `Total materials expenditures` * `Canadian dollar exchange rate`)  %>%
    dplyr::mutate('Total salaries & wages (CAD)' = `Total salaries & wages` * `Canadian dollar exchange rate`)  %>%
    dplyr::mutate('Other operating expenditures (CAD)' = `Other operating expenditures` * `Canadian dollar exchange rate`) %>%
    dplyr::filter(`Year` %in% c(yearsToDisplay))

  return(selectedData)

}
