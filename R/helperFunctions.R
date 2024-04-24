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
      yearsToDisplay <- yearsInData
    } else if(length(yearsInData) > 5) {
      yearsToDisplay <-
        yearsInData[(length(yearsInData) - 4):length(yearsInData)]
    }
  } else if(is.numeric(years) != TRUE) {
    # Testing phrases
    # cat("\n Run condition 2")
    stop("Argument years should be set as a vector of numeric data
         containing 5 years or set to NA.")
  } else if(is.vector(years) != TRUE) {
    # Testing phrases
    # cat("\n Run condition 2")
    stop("Argument years should be set as a vector of numeric data
         containing 5 years or set to NA.")
  } else {
    # Testing phrases
    # cat("\n Run condition 3")

    # Check if values provided by user are in the data
    if(all(years %in% dataARL$Year) == TRUE) {

      # If more than 5 years provided by user
      if(length(years) > 5) {
        warning("More than five years provided in argument years. Most
                recent 5 years will be used.")
        yearsToDisplay <- years %>%
          unique() %>%
          sort(decreasing = FALSE) %>%
          tail(5)

      } else {
        # Use members values as provided by the user
        yearsToDisplay <- years
      }
    } else {
      stop("Argument years should be a character vector specifying
         up to five years in the uploaded data. Please check and
         ensure years selected are correct.")
    }
  }
  return(yearsToDisplay)
}

setMemebersToDispaly <- function(members, dataARL) {
  # Purpose: A helper function to return members to display
  # based on user input


  # If NA, then user wants program to select the members
  if (all(is.na(members) == TRUE)) {
    # Testing phrases
    # cat("\n Run condition 1")

    # Obtain members in uploaded data
    membersInData <- dataARL$`Institution Name` %>%
      unique() %>%
      sort()

    # Based on length of members in uploaded data, select 5 random members
    if (length(membersInData) == 5 || length(membersInData) < 4) {
      membersToDisplay <- membersInData
    } else if(length(membersInData) > 5) {
      membersToDisplay <-
        membersInData[sample(x = 1:length(membersInData), size = 5, replace = FALSE)]
    }
  } else if(is.character(members) != TRUE) {
    # Testing phrases
    # cat("\n Run condition 2")
    stop("Argument members should be a character vector specifying
         up to five ARL member institutes.")
  } else if(is.vector(members) != TRUE) {
    # Testing phrases
    # cat("\n Run condition 2")
    stop("Argument members should be a character vector specifying
         up to five ARL member institutes.")
  } else {
    # Testing phrases
    # cat("\n Run condition 3")

    # Check if values provided by user are in the data
    if(all(members %in% dataARL$`Institution Name`) == TRUE) {

      # If more than 5 members provided by user
      if(length(members) > 5) {
        warning("More than five memebers provided in argument members. Last
                five will be used.")
        membersToDisplay <- members %>%
          unique() %>%
          sort() %>%
          tail(5)
      } else {
        # Use members values as provided by the user
        membersToDisplay <- members
      }
    } else {
      stop("Argument members should be a character vector specifying
         up to five ARL member institutes in the uploaded data. Please
         check and ensure member names are correct.")
    }
  }
  return(membersToDisplay)
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

dataAdjustment <- function(dataARL, years, members) {
  # Purpose: A function that takes all data supplied by the user
  # and filter it for values being displayed by the current package
  # and years selected by the user


  yearsToDisplay <- setYearsToDispaly(years = years,
                                      dataARL = dataARL)
  # Phrases for testing purposes
  # cat("\n Years provided by user are:", years, "\n")
  # cat("\n Years to analyze are:", yearsToDisplay, "\n")

  membersToDisplay <- setMemebersToDispaly(members = members,
                       dataARL = dataARL)


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
    dplyr::filter(`Year` %in% yearsToDisplay) %>%
    dplyr::filter(`Institution Name` %in% membersToDisplay)

  return(selectedData)

}
