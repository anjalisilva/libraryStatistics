

ARLDataDownload <- readr::read_csv("~/Desktop/ARL Data Download.csv")
ARLDataDownload <- readr::read_csv(
  "/Users/user/Library/CloudStorage/GoogleDrive-anjali@alumni.uoguelph.ca/My Drive/UTorontoLibrary/Shiny/ARL Data Download.csv")
ARLDataDownload$`Titles held`
pillar::glimpse(ARLDataDownload)
dataARL <- ARLDataDownload
institute <- "TORONTO" # ARLDataDownload$`Institution Name`
years <- c(2015, 2016, 2017, 2018, 2019)

#' A Bar Plot to Compare Collection Statistics
#'
#' A function to visualize collection statistics. Institution types are assumed to be
#'  of the categories: "Canadian", "Canadian Nonacademic", "Private", "State", and
#  "Nonacademic".
#'
#'@param years A numeric vector specifying up to 5 calendar years
#'  for which data should be plotted, e.g., c(2015, 2016, 2017, 2018, 2019).
#'  If no value is provided (i.e., NA), then most recent five
#'  years available in the data will be used. If more than 5 values
#'  provided, last 5 values will be selected. Default is NA.
#'

visCollectionData <- function(dataARL, institute, years = NA) {

  # Display only 5 years of data
  if (all(is.na(years) == TRUE)) {
    yearsInData <- dataARL$Year %>%
                     unique() %>%
                     sort(decreasing = FALSE)
    # Obtain data for last 5 years
    fiveYears <-
      yearsInData[(length(yearsInData) - 4):length(yearsInData)]
  } else if (is.numeric(years) != TRUE) {
    stop("Argument years should be set as a vector of numeric data
         containing 5 years or set to NA")
  } else {
    # If more than 5 years of data present
    if(length(years) > 5) {
      warning("More than five years provided in argument years. Most
              recent 5 years will be used.")
      yearsTrucated <- years %>%
        unique() %>%
        sort(decreasing = FALSE) %>%
        tail(x = 5)
      # Obtain data for last 5 years
      fiveYears <-
        yearsInData[(length(yearsInData) - 4):length(yearsInData)]
    } else {
      fiveYears <- sort(years, decreasing = FALSE)
    }
  }

  selectedData <- dataARL %>% dplyr::select(
                            "Year",
                            "Institution number",
                            "Institution Name",
                            "Institution type",
                            "Region",
                            "Member year",
                            "Rank in ARL investment index",
                            "ARL investment index value",
                            "Titles held",
                            "Volumes held",
                            "Electronic books") %>%
    dplyr::mutate_at(c('Titles held',
                       'Volumes held',
                       'Electronic books'), as.numeric)

# Comparison within institute
# 1. Over years
# Compare within institute volumes held, vs electronic books
# Institute vs rest average
# Institute vs rest average Canada vs USA

  # set color palette
  colorPaletteCustom <- c(
    '#33a02c',
    '#fee08b',
    '#5e4fa2',
    '#c51b7d',
    '#66c2a5',
    '#3288bd',
    '#e6f598',
    '#a6cee3',
    '#fde0ef',
    '#e31a1c',
    '#cab2d6',
    '#ff7f00',
    '#b15928',
    '#dfc27d',
    '#8dd3c7',
    '#ccebc5',
    '#f1b6da')


  # --- --- --- --- --- --- --- ---
  # Titles
  # Plot of titles held Canadian institutes over 5 years
  overYearsPlot <- selectedData %>%
    dplyr::filter(`Institution type` %in% c("Canadian",  "Canadian Nonacademic", ".")) %>% # for "." median?
    dplyr::filter(`Institution Name` != "MEDIAN") %>% # remove median value
    dplyr::filter(`Year` %in% c(fiveYears)) %>% # Limit to five years
    # width = .75 ensures space between groups
    ggplot2::ggplot(aes(x = `Year`,
                        y = `Titles held`,
                        fill = factor(`Institution Name`),
                        width = .75)) +
    geom_bar(position = "dodge", stat="identity") +
    ggplot2::labs(y = "Titles Held",
                  x = "Year",
                  fill = "Institute",
                  title = "Titles Held By Canadian Institutes") +
    ggplot2::theme_bw() +
    ggplot2::theme(text = element_text(size = 10),
                   axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    # ggbreak::scale_y_break(c(110000, 190000)) +
    ggplot2::scale_fill_manual(values = colorPaletteCustom)



  # ---
  # Plot of selected institute over 5 years, with median, highest in USA and Canada
  # selectedData$`Institution type` %>% unique()
  # [1] "."                    "State"                "Canadian"             "Private"
  # [5] "Nonacademic"          "Canadian Nonacademic"

  # Select Canadian institute with max titles
  CadAcademicMax <- selectedData %>%
    dplyr::filter(`Institution type` %in% c("Canadian", "Canadian Nonacademic")) %>%
    dplyr::group_by(`Year`) %>%
    dplyr::filter(`Titles held` == max(`Titles held`, na.rm = TRUE)) %>%
    dplyr::select(`Institution Name`)

  # Select non Canadian institute with max titles
  StateMax <- selectedData %>%
    #dplyr::filter(!(`Institution type` %in% c("Canadian", "."))) %>% # for "." median?
    dplyr::filter(`Institution type` %in% "State") %>%
    dplyr::group_by(`Year`) %>%
    dplyr::filter(`Titles held` == max(`Titles held`, na.rm = TRUE)) %>%
    dplyr::select(`Institution Name`)

  PrivateMax <- selectedData %>%
    dplyr::filter(`Institution type` %in% "Private") %>%
    dplyr::group_by(`Year`) %>%
    dplyr::filter(`Titles held` == max(`Titles held`, na.rm = TRUE)) %>%
    dplyr::select(`Institution Name`)

  NonacademicMax <- selectedData %>%
    dplyr::filter(`Institution type` %in% "Nonacademic") %>%
    dplyr::group_by(`Year`) %>%
    dplyr::filter(`Titles held` == max(`Titles held`, na.rm = TRUE)) %>%
    dplyr::select(`Institution Name`)

  # Join above selections together with other data for all institutes
  medianTable <- tibble(Year = fiveYears,
         `Institution Name` = rep("MEDIAN", length(fiveYears)))
  topTitlesInst <- inner_join(rbind(medianTable, CadAcademicMax, StateMax, PrivateMax, NonacademicMax),
             selectedData, by= c("Year", "Institution Name"))

  instTypePlot <- topTitlesInst %>%
    # ensure Median appear first in legend
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    dplyr::mutate(`Institution Name` = relevel(`Institution Name`, "MEDIAN")) %>%
    # ggplot2::ggplot(aes(x = reorder(factor(Year), +(`Titles held`)),
    ggplot2::ggplot(aes(x = `Year`,
                        y = `Titles held`,
                        fill = factor(`Institution Name`),
                        width = .75)) +
    geom_bar(position = "dodge", stat="identity") +
    ggplot2::labs(y = "Titles Held",
                  x = "Year",
                  fill = "Institute",
                  title = "Max Titles Held by Institute Type") +
    ggplot2::theme_bw() +
    ggplot2::theme(text = element_text(size = 10),
                   axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    # ggbreak::scale_y_break(c(110000, 190000)) +
    ggplot2::scale_fill_manual(values = colorPaletteCustom)



  # ---
  # Join above selections together with other data for academic institutes
  topTitlesAcademicInst <- inner_join(rbind(medianTable, CadAcademicMax, StateMax, PrivateMax),
                              selectedData, by= c("Year", "Institution Name"))

  academicPlot <- topTitlesAcademicInst %>%
    # ensure Median appear first in legend
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    dplyr::mutate(`Institution Name` = relevel(`Institution Name`, "MEDIAN")) %>%
    ggplot2::ggplot(aes(x = `Year`,
                        y = `Titles held`,
                        fill = factor(`Institution Name`),
                        width = .75)) +
    geom_bar(position = "dodge", stat="identity") +
    ggplot2::labs(y = "Titles Held",
                  x = "Year",
                  fill = "Institute",
                  title = "Max Titles Held by Academic Institute Type") +
    ggplot2::theme_bw() +
    ggplot2::theme(text = element_text(size = 10),
                   axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    # ggbreak::scale_y_break(c(110000, 190000)) +
    ggplot2::scale_fill_manual(values = colorPaletteCustom)


   return(list(overYearsPlot = overYearsPlot,
          instTypePlot = instTypePlot,
          academicPlot = academicPlot))
  }




# Total library expenditures
visExpenditureData <- function(dataARL, institute, years = NA) {

  # Display only 5 years of data
  if (all(is.na(years) == TRUE)) {
    yearsInData <- dataARL$Year %>%
      unique() %>%
      sort(decreasing = FALSE)
    # Obtain data for last 5 years
    fiveYears <-
      yearsInData[(length(yearsInData) - 4):length(yearsInData)]
  } else if (is.numeric(years) != TRUE) {
    stop("Argument years should be set as a vector of numeric data
         containing 5 years or set to NA")
  } else {
    # If more than 5 years of data present
    if(length(years) > 5) {
      warning("More than five years provided in argument years. Most
              recent 5 years will be used.")
      yearsTrucated <- years %>%
        unique() %>%
        sort(decreasing = FALSE) %>%
        tail(x = 5)
      # Obtain data for last 5 years
      fiveYears <-
        yearsInData[(length(yearsInData) - 4):length(yearsInData)]
    } else {
      fiveYears <- sort(years, decreasing = FALSE)
    }
  }


  # Select data
  selectedData <- dataARL %>% dplyr::select(
    "Year",
    "Institution number",
    "Institution Name",
    "Institution type",
    "Region",
    "Member year",
    "Rank in ARL investment index",
    "ARL investment index value",
    "Total library expenditures",
    "Total materials expenditures",
    "One-time resource purchases",
    "Ongoing resource purchases",
    "Collection support",
    "Professional salaries & wages",
    "Support staff salaries & wages",
    "Student assistant wages",
    "Total salaries & wages",
    "Other operating expenditures",
    "Fringe benefits, dollar amount",
    "Fringe benefits, official designated percent",
    "External expenditures for bibliographic utilities, networks, etc.") %>%
    dplyr::mutate_at(c("Total library expenditures",
                       "Total materials expenditures",
                       "Professional salaries & wages"), as.numeric)

  # set color palette
  colorPaletteCustom <- c(
    '#33a02c',
    '#fee08b',
    '#5e4fa2',
    '#c51b7d',
    '#66c2a5',
    '#3288bd',
    '#e6f598',
    '#a6cee3',
    '#fde0ef',
    '#e31a1c',
    '#cab2d6',
    '#ff7f00',
    '#b15928',
    '#dfc27d',
    '#8dd3c7',
    '#ccebc5',
    '#f1b6da')


  # --- --- --- --- --- --- --- ---
  # Total library expenditures
  # Plot of total library expenditures held Canadian institutes over 5 years
  overYearsPlotTLE <- selectedData %>%
    dplyr::filter(`Institution type` %in% c("Canadian",  "Canadian Nonacademic", ".")) %>% # for "." median?
    dplyr::filter(`Institution Name` != "MEDIAN") %>% # remove median value
    dplyr::filter(`Year` %in% c(fiveYears)) %>% # Limit to five years
    # width = .75 ensures space between groups
    ggplot2::ggplot(aes(x = `Year`,
                        y = `Total library expenditures`,
                        fill = factor(`Institution Name`),
                        width = .75)) +
    geom_bar(position = "dodge", stat="identity") +
    ggplot2::labs(y = "Total Library Expenditures",
                  x = "Year",
                  fill = "Institute",
                  title = "Total Library Expenditures By Canadian Institutes") +
    ggplot2::theme_bw() +
    ggplot2::theme(text = element_text(size = 10),
                   axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    # ggbreak::scale_y_break(c(110000, 190000)) +
    ggplot2::scale_fill_manual(values = colorPaletteCustom) +
    ggplot2::scale_y_continuous(labels=scales::dollar_format())


  # ---
  # Plot of selected institute over 5 years, with median, highest in USA and Canada
  # selectedData$`Institution type` %>% unique()
  # [1] "."                    "State"                "Canadian"             "Private"
  # [5] "Nonacademic"          "Canadian Nonacademic"

  # Select Canadian institute with max titles
  CadAcademicMaxTLE <- selectedData %>%
    dplyr::filter(`Institution type` %in% c("Canadian", "Canadian Nonacademic")) %>%
    dplyr::group_by(`Year`) %>%
    dplyr::filter(`Total library expenditures` == max(`Total library expenditures`, na.rm = TRUE)) %>%
    dplyr::select(`Institution Name`)

  # Select non Canadian institute with max titles
  StateMaxTLE <- selectedData %>%
    #dplyr::filter(!(`Institution type` %in% c("Canadian", "."))) %>% # for "." median?
    dplyr::filter(`Institution type` %in% "State") %>%
    dplyr::group_by(`Year`) %>%
    dplyr::filter(`Total library expenditures` == max(`Total library expenditures`, na.rm = TRUE)) %>%
    dplyr::select(`Institution Name`)

  PrivateMaxTLE <- selectedData %>%
    dplyr::filter(`Institution type` %in% "Private") %>%
    dplyr::group_by(`Year`) %>%
    dplyr::filter(`Total library expenditures` == max(`Total library expenditures`, na.rm = TRUE)) %>%
    dplyr::select(`Institution Name`)

  NonacademicMaxTLE <- selectedData %>%
    dplyr::filter(`Institution type` %in% "Nonacademic") %>%
    dplyr::group_by(`Year`) %>%
    dplyr::filter(`Total library expenditures` == max(`Total library expenditures`, na.rm = TRUE)) %>%
    dplyr::select(`Institution Name`)

  # Join above selections together with other data for all institutes
  medianTable <- tibble(Year = fiveYears,
                        `Institution Name` = rep("MEDIAN", length(fiveYears)))
  topTitlesInst <- inner_join(rbind(medianTable, CadAcademicMaxTLE, StateMaxTLE, PrivateMaxTLE, NonacademicMaxTLE),
                              selectedData, by= c("Year", "Institution Name"))

  instTypePlotTLE <- topTitlesInst %>%
    # ensure Median appear first in legend
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    dplyr::mutate(`Institution Name` = relevel(`Institution Name`, "MEDIAN")) %>%
    ggplot2::ggplot(aes(x = `Year`,
                        y = `Total library expenditures`,
                        fill = factor(`Institution Name`),
                        width = .75)) +
    geom_bar(position = "dodge", stat="identity") +
    ggplot2::labs(y = "Total Library Expenditures",
                  x = "Year",
                  fill = "Institute",
                  title = "Total Library Expenditures by Institute Type") +
    ggplot2::theme_bw() +
    ggplot2::theme(text = element_text(size = 10),
                   axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    # ggbreak::scale_y_break(c(110000, 190000)) +
    ggplot2::scale_fill_manual(values = colorPaletteCustom) +
    ggplot2::scale_y_continuous(labels=scales::dollar_format())



  # ---
  # Join above selections together with other data for academic institutes
  topTitlesAcademicInst <- inner_join(rbind(medianTable, CadAcademicMaxTLE, StateMaxTLE, PrivateMaxTLE),
                                      selectedData, by= c("Year", "Institution Name"))

  academicPlotTLE <- topTitlesAcademicInst %>%
    # ensure Median appear first in legend
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    dplyr::mutate(`Institution Name` = relevel(`Institution Name`, "MEDIAN")) %>%
    ggplot2::ggplot(aes(x = `Year`,
                        y = `Total library expenditures`,
                        fill = factor(`Institution Name`),
                        width = .75)) +
    geom_bar(position = "dodge", stat="identity") +
    ggplot2::labs(y = "Total Library Expenditures",
                  x = "Year",
                  fill = "Institute",
                  title = "Max Total Library Expenditures by Academic Institute Type") +
    ggplot2::theme_bw() +
    ggplot2::theme(text = element_text(size = 10),
                   axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    # ggbreak::scale_y_break(c(110000, 190000)) +
    ggplot2::scale_fill_manual(values = colorPaletteCustom) +
    ggplot2::scale_y_continuous(labels=scales::dollar_format())






  # --- --- --- --- --- --- --- ---
  # Total materials expenditures
  # Plot of total materials expenditures held Canadian institutes over 5 years
  overYearsPlotTME <- selectedData %>%
    dplyr::filter(`Institution type` %in% c("Canadian",  "Canadian Nonacademic", ".")) %>% # for "." median?
    dplyr::filter(`Institution Name` != "MEDIAN") %>% # remove median value
    dplyr::filter(`Year` %in% c(fiveYears)) %>% # Limit to five years
    # width = .75 ensures space between groups
    ggplot2::ggplot(aes(x = `Year`,
                        y = `Total materials expenditures`,
                        fill = factor(`Institution Name`),
                        width = .75)) +
    geom_bar(position = "dodge", stat="identity") +
    ggplot2::labs(y = "Total Materials Expenditures",
                  x = "Year",
                  fill = "Institute",
                  title = "Total Materials Expenditures By Canadian Institutes") +
    ggplot2::theme_bw() +
    ggplot2::theme(text = element_text(size = 10),
                   axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    # ggbreak::scale_y_break(c(110000, 190000)) +
    ggplot2::scale_fill_manual(values = colorPaletteCustom) +
    ggplot2::scale_y_continuous(labels=scales::dollar_format())


  # ---
  # Plot of selected institute over 5 years, with median, highest in USA and Canada
  # selectedData$`Institution type` %>% unique()
  # [1] "."                    "State"                "Canadian"             "Private"
  # [5] "Nonacademic"          "Canadian Nonacademic"

  # Select Canadian institute with max titles
  CadAcademicMaxTME <- selectedData %>%
    dplyr::filter(`Institution type` %in% c("Canadian", "Canadian Nonacademic")) %>%
    dplyr::group_by(`Year`) %>%
    dplyr::filter(`Total materials expenditures` == max(`Total materials expenditures`, na.rm = TRUE)) %>%
    dplyr::select(`Institution Name`)

  # Select non Canadian institute with max titles
  StateMaxTME <- selectedData %>%
    #dplyr::filter(!(`Institution type` %in% c("Canadian", "."))) %>% # for "." median?
    dplyr::filter(`Institution type` %in% "State") %>%
    dplyr::group_by(`Year`) %>%
    dplyr::filter(`Total materials expenditures` == max(`Total materials expenditures`, na.rm = TRUE)) %>%
    dplyr::select(`Institution Name`)

  PrivateMaxTME <- selectedData %>%
    dplyr::filter(`Institution type` %in% "Private") %>%
    dplyr::group_by(`Year`) %>%
    dplyr::filter(`Total materials expenditures` == max(`Total materials expenditures`, na.rm = TRUE)) %>%
    dplyr::select(`Institution Name`)

  NonacademicMaxTME <- selectedData %>%
    dplyr::filter(`Institution type` %in% "Nonacademic") %>%
    dplyr::group_by(`Year`) %>%
    dplyr::filter(`Total materials expenditures` == max(`Total materials expenditures`, na.rm = TRUE)) %>%
    dplyr::select(`Institution Name`)

  # Join above selections together with other data for all institutes
  medianTable <- tibble(Year = fiveYears,
                        `Institution Name` = rep("MEDIAN", length(fiveYears)))
  topTitlesInst <- inner_join(rbind(medianTable, CadAcademicMaxTME, StateMaxTME, PrivateMaxTME, NonacademicMaxTME),
                              selectedData, by= c("Year", "Institution Name"))

  instTypePlotTME <- topTitlesInst %>%
    # ensure Median appear first in legend
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    dplyr::mutate(`Institution Name` = relevel(`Institution Name`, "MEDIAN")) %>%
    ggplot2::ggplot(aes(x = `Year`,
                        y = `Total materials expenditures`,
                        fill = factor(`Institution Name`),
                        width = .75)) +
    geom_bar(position = "dodge", stat="identity") +
    ggplot2::labs(y = "Total Materials Expenditures",
                  x = "Year",
                  fill = "Institute",
                  title = "Total Materials Expenditures by Institute Type") +
    ggplot2::theme_bw() +
    ggplot2::theme(text = element_text(size = 10),
                   axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    # ggbreak::scale_y_break(c(110000, 190000)) +
    ggplot2::scale_fill_manual(values = colorPaletteCustom) +
    ggplot2::scale_y_continuous(labels=scales::dollar_format())



  # ---
  # Join above selections together with other data for academic institutes
  topTitlesAcademicInst <- inner_join(rbind(medianTable, CadAcademicMaxTME, StateMaxTME, PrivateMaxTME),
                                      selectedData, by= c("Year", "Institution Name"))

  academicPlotTME <- topTitlesAcademicInst %>%
    # ensure Median appear first in legend
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    dplyr::mutate(`Institution Name` = relevel(`Institution Name`, "MEDIAN")) %>%
    ggplot2::ggplot(aes(x = `Year`,
                        y = `Total materials expenditures`,
                        fill = factor(`Institution Name`),
                        width = .75)) +
    geom_bar(position = "dodge", stat="identity") +
    ggplot2::labs(y = "Total Materials Expenditures",
                  x = "Year",
                  fill = "Institute",
                  title = "Max Total Materials Expenditures by Academic Institute Type") +
    ggplot2::theme_bw() +
    ggplot2::theme(text = element_text(size = 10),
                   axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    # ggbreak::scale_y_break(c(110000, 190000)) +
    ggplot2::scale_fill_manual(values = colorPaletteCustom) +
    ggplot2::scale_y_continuous(labels=scales::dollar_format())







  # --- --- --- --- --- --- --- ---
  # Professional salaries & wages

}

