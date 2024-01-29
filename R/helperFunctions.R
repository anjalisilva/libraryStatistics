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
