

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
      '#a6cee3',
      '#b15928',
      'darkgreen',
      '#c51b7d',
      'darkgrey',
      '#fee08b',
      '#5e4fa2',
      '#ccebc5',
      '#ff7f00',
      '#e6f598',
      '#66c2a5',
      'black',
      'blue',
      '#fde0ef',
      '#3288bd',
      '#f1b6da',
      '#8dd3c7',
      '#dfc27d',
      '#cab2d6')
    return(colorPaletteCustom)

  } else {
    # no return
  }
}

setYearsToDispaly <- function(years) {
  # A helper function to return years to display
  # based on user input

  if (all(is.na(years) == TRUE)) {
    # Testing phrases - if NA, then user wants program to select the years
    cat("\n Run condition 1")
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
    # Testing phrases - if years are not numeric data
    cat("\n Run condition 2")
    stop("Argument years should be set as a vector of numeric data
         containing 5 years or set to NA")
  } else {
    cat("\n Run condition 3")
    if(length(years) > 5) {
      # If more than 5 years provided by user
      warning("More than five years provided in argument years. Most
              recent 5 years will be used.")
      yearsTrucated <- years %>%
        unique() %>%
        sort(decreasing = FALSE) %>%
        tail(5)
      # Obtain data for last 5 years
      yearsToDisplay <-
        yearsTrucated
    } else {
      # if data for 5 or less years
      yearsToDisplay <- sort(years, decreasing = FALSE)
    }
  }
  return(yearsToDisplay)
}

# visCollection
#' Plots to Compare Titles in Collection Over Years
#'
#' A function to visualize title statistics using multiple plot types
#' as described below. Institution types in the input data are
#' assumed to be of the categories: "Canadian", "Canadian Nonacademic",
#' "Private", "State", and "Nonacademic". For title statistics
#' visualization, the following variables (or columns) are required
#' in the dataset: "Year", "Institution Name", "Institution type",
#' "Region", "Rank in ARL investment index", "ARL investment index value",
#' and "Titles held".
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
#'   \item titleUserInstitute - A lineplot comparing user selected
#'         institute over user selected number of years for titles
#'         held. The median line is provided for comparison.
#'   \item InstCanadianPlot - A barplot comparing Canadian institutes
#'         based on titles held, along with user selected institute over
#'         user selected number of years.
#'   \item instTypePlot - A barplot comparing maximum title holders by
#'         institute type over user selected number of years. The user
#'         selected institute is provided for comparison. Institute
#'         types include: "Canadian", "Private", "State", and "Nonacademic".
#'   \item academicPlot - A barplot comparing maximum title holders by
#'         academic institute type over user selected number of years.
#'         The user selected institute is provided for comparison.
#'         Institute types include: "Canadian" and "State".
#'   \item plotARLRankTop - A barplot comparing title holders by
#'         top 5 ARL investment ranks over user selected number of years.
#'         The user selected institute is provided for comparison.
#' }
#'
#' @examples
#' visTitlesData(dataARL = ARLDataDownload,
#'                   institute = "TEXAS STATE",
#'                   years = c(2015, 2016, 2017, 2022, 2018, 2019))
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
  # Phrases for testing purposes
  # cat("\n Years provided by user are:", years, "\n")
  # cat("\n Years to analyze are:", yearsToDisplay, "\n")

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
  InstSelectedData <- selectedData %>% # user selected institute
    dplyr::filter(`Institution Name` %in% institute)
  InstCadData <- selectedData %>% # Canadian institutes
    dplyr::filter(`Institution type` %in% c("Canadian",  "Canadian Nonacademic", "."))

  InstCanadianPlot <- rbind(InstSelectedData, InstCadData) %>%
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
    ggplot2::geom_text(aes(y = 0.5, label = `Institution type`),
                       position = position_dodge(width = 0.9),
                       angle = 90,
                       size = 4,
                       hjust = 'left')



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
    ggplot2::geom_text(aes(y = 0.5, label = `Institution type`),
                       position = position_dodge(width = 0.9),
                       angle = 90,
                       size = 4,
                       hjust = 'left')




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





