

# ARLDataDownload <- readr::read_csv("~/Desktop/ARL Data Download.csv")
# ARLDataDownload <- readr::read_csv(
#   "/Users/user/Library/CloudStorage/GoogleDrive-anjali@alumni.uoguelph.ca/My Drive/UTorontoLibrary/Shiny/ARL Data Download.csv")
# inputCountsPath <- system.file("extdata", "ARL Data Download.csv", package = "libraryStatistics")
# ARLDataDownload <- readr::read_csv(inputCountsPath)



# ---
# Helper functions
setColorPalette <- function(returnCol = TRUE) {

  if(returnCol == TRUE) {
    # set color palette
    colorPaletteCustom <- c(
      '#33a02c',
      'red',
      'blue',
      '#b15928',
      '#f1b6da',
      'darkgrey',
      '#fee08b',
      '#5e4fa2',
      '#ccebc5',
      '#c51b7d',
      '#66c2a5',
      '#e6f598',
      'black',
      '#a6cee3',
      '#ff7f00',
      '#fde0ef',
      '#3288bd',
      'darkgreen',
      '#dfc27d',
      '#8dd3c7',
      '#cab2d6')
    return(colorPaletteCustom)

  } else {
    # no return
  }
}

setYearsToDispaly <- function(years) {
  # A helper function to return years to display
  # based on user input

  # If NA, then user wants program to select the years
  if (all(is.na(years) == TRUE)) {
    # Obtain years in data
    yearsInData <- dataARL$Year %>%
      unique() %>%
      sort(decreasing = FALSE)
    # Based on length of years in data, select last 5 or less years
    if (length(yearsInData) == 5 || length(yearsInData) < 4) {
      yearsToDisplay <- years
    } else if(length(yearsInData) > 5) {
      yearsToDisplay <-
        yearsInData[(length(yearsInData) - 4):length(yearsInData)]
    }
  } else if (is.numeric(years) != TRUE) {
    stop("Argument years should be set as a vector of numeric data
         containing 5 years or set to NA")
  } else {
    # If more than 5 years provided by user
    if(length(years) > 5) {
      warning("More than five years provided in argument years. Most
              recent 5 years will be used.")
      yearsTrucated <- years %>%
        unique() %>%
        sort(decreasing = FALSE) %>%
        tail(x = 5)
      # Obtain data for last 5 years
      yearsToDisplay <-
        yearsInData[(length(yearsInData) - 4):length(yearsInData)]
    } else {
      yearsToDisplay <- sort(years, decreasing = FALSE)
    }
  }
  return(yearsToDisplay)
}

# visCollection
#' A Bar Plot to Compare Collection Statistics
#'
#' A function to visualize collection statistics. Institution types are
#' assumed to be of the categories: "Canadian", "Canadian Nonacademic",
#' "Private", "State", and "Nonacademic". For collection statistics
#' visualization, the following variables (or columns) are required
#' in the dataset: "Year", "Institution Name", "Institution type",
#' "Region", "Rank in ARL investment index", "ARL investment index value",
#' "Titles held", "Volumes held", and "Electronic books".
#'
#'@param dataARL A data frame containing data downloaded from
#'   ARL. The years should be placed along rows. The first column must
#'   be 'Year', followed by other variables in no particular order,
#'   e.g., 'Institution Name', 'Institution type', etc.
#'@param institute A character vector specifying the institute of
#'   interest, as identified in the dataset. E.g., "TORONTO" for
#'   University of Toronto Libraries.
#'@param years A numeric vector specifying up to 5 calendar years
#'   for which data should be plotted, e.g., c(2015, 2016, 2017,
#'   2018, 2019). If no value is provided (i.e., NA), then most
#'   recent five years available in the data will be used. If more
#'   than 5 values provided, last 5 values will be selected. Default
#'   is NA.
#'
#' @return Returns three bar plots showing collection statistics
#' \itemize{
#'   \item InstCanadianPlot - A bar plot comparing Canadian institutes
#'         based on titles held.
#'   \item instTypePlot - A bar plot comparing maximum title holders by
#'         institute type. Types include: "Canadian", "Private", "State",
#'         and "Nonacademic".
#'   \item academicPlot - A bar plot comparing maximum title holders by
#'         academic institute type. Types include: "Canadian" and "State".
#' }
#'
#' @examples
#' visTitlesData(dataARL = ARLDataDownload,
#'                   institute = "TORONTO",
#'                   years = c(2015, 2022, 2017, 2021))
#'
#' @export
#' @importFrom ggplot2 ggplot
#' @import magrittr
visTitlesData <- function(dataARL, institute, years = NA) {

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
                            "Electronic books") %>%
                  dplyr::mutate_at(
                     c('Titles held',
                       'Volumes held',
                       'Electronic books'), as.numeric)

  yearsToDisplay <- setYearsToDispaly(years = years)
  cat("\n Years provided by user are:", years, "\n")
  cat("\n Years to analyze are:", yearsToDisplay, "\n")

  # --- --- --- --- --- --- --- ---
  # Titles
  # Visualize institute selected with medium
  titleUserInstitute <- selectedData %>%
    dplyr::filter(`Institution Name` %in% c(institute, "MEDIAN")) %>%
    # ensure Median appear first in legend
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    dplyr::mutate(`Institution Name` = relevel(`Institution Name`, "MEDIAN")) %>%
    dplyr::filter(`Year` %in% c(yearsToDisplay)) %>% # Limit to five years
    # width = .75 ensures space between groups
    ggplot2::ggplot(aes(x = `Year`,
                        y = `Titles held`,
                        width = .75)) +
    ggplot2::geom_line(linetype = "dashed",
                       linewidth = 0.5,
                       aes(color = `Institution Name`)) +
    ggplot2::geom_point(size = 0.5, aes(color = `Institution Name`)) +
    ggplot2::scale_color_manual(values = c(setColorPalette())) +
    ggplot2::labs(y = "Titles Held",
                  x = "Year",
                  title = "Titles Held By Selected Institute") +
    ggplot2::theme_bw() +
    ggplot2::theme(text = element_text(size = 10, color = 'black'),
                   axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, color = 'black', size = 10),
                   axis.text.y = element_text(color = 'black', size = 10)) +
    ggplot2::scale_y_continuous(labels = scales::label_comma())


  # ---
  # Plot of titles held Canadian institutes over 5 years
  InstCanadianPlot <- selectedData %>%
    dplyr::filter(`Institution type` %in% c("Canadian",  "Canadian Nonacademic", ".", institute)) %>% # for "." median
    # ensure Median appear first in legend
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    dplyr::mutate(`Institution Name` = relevel(`Institution Name`, ref = institute)) %>%
    dplyr::mutate(`Institution Name` = relevel(`Institution Name`, ref = "MEDIAN")) %>%
    dplyr::filter(`Year` %in% c(yearsToDisplay)) %>% # Limit to five years
    # width = .75 ensures space between groups
    ggplot2::ggplot(aes(x = factor(`Year`),
                        y = `Titles held`,
                        fill = factor(`Institution Name`),
                        width = .75)) +
    ggplot2::geom_bar(position = "dodge", stat="identity") +
    ggplot2::labs(y = "Titles Held",
                  x = "Year",
                  fill = "Institute",
                  title = "Comparison With Titles Held By Canadian Institutes") +
    ggplot2::theme_bw() +
    ggplot2::theme(text = element_text(size = 10, color = 'black'),
                   axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, color = 'black', size = 10),
                   axis.text.y = element_text(color = 'black', size = 10)) +
    ggplot2::scale_fill_manual(values = setColorPalette()) +
    ggplot2::scale_y_continuous(labels = scales::label_comma())



  # ---
  # Plot of institute types over 5 years, with median, highest in USA and Canada
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
  medianTable <- tibble::tibble(Year = yearsToDisplay,
         `Institution Name` = rep("MEDIAN", length(yearsToDisplay)))
  userSelectTable <- tibble::tibble(Year = yearsToDisplay,
                        `Institution Name` = rep(institute, length(yearsToDisplay)))
  topTitlesInst <- dplyr::inner_join(rbind(medianTable,
                                    userSelectTable,
                                    CadAcademicMax,
                                    StateMax,
                                    PrivateMax,
                                    NonacademicMax),
             selectedData, by= c("Year", "Institution Name"))

  instTypePlot <- topTitlesInst %>%
    # ensure Median appear first in legend
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    dplyr::mutate(`Institution Name` = relevel(`Institution Name`, ref = institute)) %>%
    dplyr::mutate(`Institution Name` = relevel(`Institution Name`, ref = "MEDIAN")) %>%
    dplyr::filter(`Year` %in% c(yearsToDisplay)) %>% # Limit to five years
    # ggplot2::ggplot(aes(x = reorder(factor(Year), +(`Titles held`)),
    ggplot2::ggplot(aes(x = factor(`Year`),
                        y = `Titles held`,
                        fill = factor(`Institution Name`),
                        width = .75)) +
    ggplot2::geom_bar(position = "dodge", stat="identity") +
    ggplot2::labs(y = "Titles Held",
                  x = "Year",
                  fill = "Institute",
                  title = "Max Titles Held by Institute Type") +
    ggplot2::theme_bw() +
    ggplot2::theme(text = element_text(size = 10, color = 'black'),
                   axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, color = 'black', size = 10),
                   axis.text.y = element_text(color = 'black', size = 10)) +
    ggplot2::scale_fill_manual(values = setColorPalette()) +
    ggplot2::scale_y_continuous(labels = scales::label_comma()) +
    # Add ranking labels on bars
    ggplot2::geom_text(aes(label = `Institution type`),
                       position = position_dodge(width = 0.9),
                       vjust = 0.5,
                       angle = 90,
                       size = 2.5)



  # ---
  # Join above selections together with other data for academic institutes
  topTitlesAcademicInst <- dplyr::inner_join(rbind(medianTable,
                                            userSelectTable,
                                            CadAcademicMax,
                                            StateMax,
                                            PrivateMax),
                              selectedData, by= c("Year", "Institution Name"))

  academicPlot <- topTitlesAcademicInst %>%
    # ensure Median appear first in legend
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    dplyr::mutate(`Institution Name` = relevel(`Institution Name`, ref = institute)) %>%
    dplyr::mutate(`Institution Name` = relevel(`Institution Name`, ref = "MEDIAN")) %>%
    dplyr::filter(`Year` %in% c(yearsToDisplay)) %>% # Limit to five years
    ggplot2::ggplot(aes(x = factor(`Year`),
                        y = `Titles held`,
                        fill = factor(`Institution Name`),
                        width = .75)) +
    ggplot2::geom_bar(position = "dodge", stat="identity") +
    ggplot2::labs(y = "Titles Held",
                  x = "Year",
                  fill = "Institute",
                  title = "Comparison of Max Titles Held by Academic Institute Type") +
    ggplot2::theme_bw() +
    ggplot2::theme(text = element_text(size = 10, color = 'black'),
                   axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, color = 'black', size = 10),
                   axis.text.y = element_text(color = 'black', size = 10)) +
    ggplot2::scale_fill_manual(values = setColorPalette()) +
    ggplot2::scale_y_continuous(labels = scales::label_comma()) +
    # Add ranking labels on bars
    ggplot2::geom_text(aes(label = `Institution type`),
                       position = position_dodge(width = 0.9),
                       vjust = 0.5,
                       angle = 90,
                       size = 2.5)




  # ---
  # Plot comparing top 5 ARL ranks and their titles

  topARLRankData <- selectedData %>%
    dplyr::filter(`Rank in ARL investment index` %in% c("1", "2", "3", "4", "5"))

  selectARLRankData <- selectedData %>%
    dplyr::filter(`Institution Name` %in% c("MEDIAN", institute))

  combinedRankData <- rbind(topARLRankData, selectARLRankData)

  plotARLRankTop <- combinedRankData %>%
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    dplyr::mutate(`Rank in ARL investment index` = factor(`Rank in ARL investment index`, levels = c("1", "2", "3", "4", "5"))) %>%
    dplyr::mutate(`Institution Name` = relevel(`Institution Name`, ref = institute)) %>%
    dplyr::mutate(`Institution Name` = relevel(`Institution Name`, ref = "MEDIAN")) %>%
    dplyr::filter(`Year` %in% c(yearsToDisplay)) %>% # Limit to five years
    ggplot2::ggplot(aes(x = factor(`Year`),
                        y = `Titles held`,
                        fill = factor(`Institution Name`),
                        width = .75)) +
    ggplot2::geom_bar(position = "dodge", stat="identity") +
    ggplot2::labs(y = "Titles Held",
                  x = "Year",
                  fill = "Institute",
                  title = "Titles Held by Institutes with Highest Investment ARL Rank") +
    ggplot2::theme_bw() +
    ggplot2::theme(text = element_text(size = 10, color = 'black'),
                   axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, color = 'black', size = 10),
                   axis.text.y = element_text(color = 'black', size = 10)) +
    ggplot2::scale_fill_manual(values = setColorPalette()) +
    ggplot2::scale_y_continuous(labels = scales::label_comma()) +
    # Add ranking labels on bars
    ggplot2::geom_text(aes(label = `Rank in ARL investment index`),
                       position = position_dodge(width = 0.9), vjust = 0)


   return(list(titleUserInstitute = titleUserInstitute,
               InstCanadianPlot = InstCanadianPlot,
               instTypePlot = instTypePlot,
               academicPlot = academicPlot,
               plotARLRankTop = plotARLRankTop))
  }



#' A Bar Plot to Compare Collection Statistics
#'
#' A function to visualize collection statistics. Institution types are
#' assumed to be of the categories: "Canadian", "Canadian Nonacademic",
#' "Private", "State", and "Nonacademic". For collection statistics
#' visualization, the following variables (or columns) are required
#' in the dataset: "Year", "Institution Name", "Institution type",
#' "Region", "Rank in ARL investment index", "ARL investment index value",
#' "Titles held", "Volumes held", and "Electronic books".
#'
#'@param dataARL A data frame containing data downloaded from
#'   ARL. The years should be placed along rows. The first column must
#'   be 'Year', followed by other variables in no particular order,
#'   e.g., 'Institution Name', 'Institution type', etc.
#'@param institute A character vector specifying the institute of
#'   interest, as identified in the dataset. E.g., "TORONTO" for
#'   University of Toronto Libraries.
#'@param years A numeric vector specifying up to 5 calendar years
#'   for which data should be plotted, e.g., c(2015, 2016, 2017,
#'   2018, 2019). If no value is provided (i.e., NA), then most
#'   recent five years available in the data will be used. If more
#'   than 5 values provided, last 5 values will be selected. Default
#'   is NA.
#'
#' @return Returns three bar plots showing collection statistics
#' \itemize{
#'   \item InstCanadianPlot - A bar plot comparing Canadian institutes
#'         based on titles held.
#'   \item instTypePlot - A bar plot comparing maximum title holders by
#'         institute type. Types include: "Canadian", "Private", "State",
#'         and "Nonacademic".
#'   \item academicPlot - A bar plot comparing maximum title holders by
#'         academic institute type. Types include: "Canadian" and "State".
#' }
#'
#' @examples
#' visCollectionData(dataARL = ARLDataDownload,
#'                   institute = "TORONTO",
#'                   years = c(2015, 2016, 2017, 2018, 2019))
#'
#' @export
# Total library expenditures
visExpenditureData <- function(dataARL, institute, years = NA) {

  yearsToDisplay <- setYearsToDispaly(years = years)


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


  # --- --- --- --- --- --- --- ---
  # Total library expenditures
  # Plot of total library expenditures held Canadian institutes over 5 years
  InstCanadianPlotTLE <- selectedData %>%
    dplyr::filter(`Institution type` %in% c("Canadian",  "Canadian Nonacademic", ".", institute)) %>% # for "." median
    # ensure Median appear first in legend
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    dplyr::mutate(`Institution Name` = relevel(`Institution Name`, ref = institute)) %>%
    dplyr::mutate(`Institution Name` = relevel(`Institution Name`, ref = "MEDIAN")) %>%
    dplyr::filter(`Year` %in% c(yearsToDisplay)) %>% # Limit to five years
    # width = .75 ensures space between groups
    ggplot2::ggplot(aes(x = factor(`Year`),
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
    ggplot2::scale_fill_manual(values = setColorPalette()) +
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
  medianTable <- tibble::tibble(Year = yearsToDisplay,
                        `Institution Name` = rep("MEDIAN", length(yearsToDisplay)))
  topTLEInst <- dplyr::inner_join(rbind(medianTable, CadAcademicMaxTLE, StateMaxTLE, PrivateMaxTLE, NonacademicMaxTLE),
                              selectedData, by= c("Year", "Institution Name"))

  instTypePlotTLE <- topTLEInst %>%
    # ensure Median appear first in legend
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    dplyr::mutate(`Institution Name` = relevel(`Institution Name`, ref = institute)) %>%
    dplyr::mutate(`Institution Name` = relevel(`Institution Name`, ref = "MEDIAN")) %>%
    dplyr::filter(`Year` %in% c(yearsToDisplay)) %>% # Limit to five years
    ggplot2::ggplot(aes(x = factor(`Year`),
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
    ggplot2::scale_fill_manual(values = setColorPalette()) +
    ggplot2::scale_y_continuous(labels=scales::dollar_format())



  # ---
  # Join above selections together with other data for academic institutes
  topTLEAcademicInst <- inner_join(rbind(medianTable, CadAcademicMaxTLE, StateMaxTLE, PrivateMaxTLE),
                                      selectedData, by= c("Year", "Institution Name"))

  academicPlotTLE <- topTLEAcademicInst %>%
    # ensure Median appear first in legend
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    dplyr::mutate(`Institution Name` = relevel(`Institution Name`, ref = institute)) %>%
    dplyr::mutate(`Institution Name` = relevel(`Institution Name`, ref = "MEDIAN")) %>%
    dplyr::filter(`Year` %in% c(yearsToDisplay)) %>% # Limit to five years
    ggplot2::ggplot(aes(x = factor(`Year`),
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
    ggplot2::scale_fill_manual(values = setColorPalette()) +
    ggplot2::scale_y_continuous(labels = scales::dollar_format())






  # --- --- --- --- --- --- --- ---
  # Total materials expenditures
  # Plot of total materials expenditures held Canadian institutes over 5 years
  InstCanadianPlotTME <- selectedData %>%
    dplyr::filter(`Institution type` %in% c("Canadian",  "Canadian Nonacademic", ".", institute)) %>% # for "." median
    # ensure Median appear first in legend
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    dplyr::mutate(`Institution Name` = relevel(`Institution Name`, ref = institute)) %>%
    dplyr::mutate(`Institution Name` = relevel(`Institution Name`, ref = "MEDIAN")) %>%
    dplyr::filter(`Year` %in% c(yearsToDisplay)) %>% # Limit to five years
    # width = .75 ensures space between groups
    ggplot2::ggplot(aes(x = factor(`Year`),
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
    ggplot2::scale_fill_manual(values = setColorPalette()) +
    ggplot2::scale_y_continuous(labels=scales::dollar_format())


  # ---
  # Plot of selected institute over 5 years, with median, highest in USA and Canada
  # selectedData$`Institution type` %>% unique()
  # [1] "."                    "State"                "Canadian"             "Private"
  # [5] "Nonacademic"          "Canadian Nonacademic"

  # Select Canadian institute with max total materials expenditures
  CadAcademicMaxTME <- selectedData %>%
    dplyr::filter(`Institution type` %in% c("Canadian", "Canadian Nonacademic")) %>%
    dplyr::group_by(`Year`) %>%
    dplyr::filter(`Total materials expenditures` == max(`Total materials expenditures`, na.rm = TRUE)) %>%
    dplyr::select(`Institution Name`)

  # Select non Canadian institute with max total materials expenditures
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
  medianTable <- tibble::tibble(Year = yearsToDisplay,
                        `Institution Name` = rep("MEDIAN", length(yearsToDisplay)))
  topTMEInst <- dplyr::inner_join(rbind(medianTable, CadAcademicMaxTME, StateMaxTME, PrivateMaxTME, NonacademicMaxTME),
                              selectedData, by= c("Year", "Institution Name"))

  instTypePlotTME <- topTMEInst %>%
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    dplyr::mutate(`Institution Name` = relevel(`Institution Name`, ref = institute)) %>%
    dplyr::mutate(`Institution Name` = relevel(`Institution Name`, ref = "MEDIAN")) %>%
    dplyr::filter(`Year` %in% c(yearsToDisplay)) %>% # Limit to five years
    ggplot2::ggplot(aes(x = factor(`Year`),
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
    ggplot2::scale_fill_manual(values = setColorPalette()) +
    ggplot2::scale_y_continuous(labels=scales::dollar_format())



  # ---
  # Join above selections together with other data for academic institutes
  topTMEAcademicInst <- dplyr::inner_join(rbind(medianTable, CadAcademicMaxTME, StateMaxTME, PrivateMaxTME),
                                      selectedData, by= c("Year", "Institution Name"))

  academicPlotTME <- topTMEAcademicInst %>%
    # ensure Median appear first in legend
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    dplyr::mutate(`Institution Name` = relevel(`Institution Name`, "MEDIAN")) %>%
    dplyr::filter(`Year` %in% c(yearsToDisplay)) %>% # Limit to five years
    ggplot2::ggplot(aes(x = factor(`Year`),
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
    ggplot2::scale_fill_manual(values = setColorPalette()) +
    ggplot2::scale_y_continuous(labels=scales::dollar_format())







  # --- --- --- --- --- --- --- ---
  # Professional salaries & wages

}

visExpenditureData(dataARL = ARLDataDownload,
                  institute = "TORONTO",
                  years = c(2015, 2016, 2017, 2018, 2019))


