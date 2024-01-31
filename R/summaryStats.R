# summary statistics
#' Plots to Compare Summary of Regions
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
#' @return Returns three bar plots showing title statistics
#' \itemize{
#'   \item titleUserInstitute - A lineplot comparing user selected
#'         institute over user selected number of years for titles
#'         held. The median line is provided for comparison.
#'   \item titleInstCanadian - A barplot comparing Canadian institutes
#'         based on titles held, along with user selected institute over
#'         user selected number of years. The median is provided for
#'         comparison.
#'   \item titleInstType - A barplot comparing maximum title by
#'         institute type over user selected number of years. The user
#'         selected institute is provided for comparison. Institute
#'         types include: "Canadian", "Private", "State", and "Nonacademic".
#'          The median is provided for comparison.
#'   \item titleAcademicPlot - A barplot comparing maximum title by
#'         academic institute type over user selected number of years.
#'         The user selected institute is provided for comparison.
#'         Institute types include: "Canadian" and "State". The median
#'         is provided for comparison.
#'   \item titleARLRankTop - A barplot comparing titles by
#'         top 5 ARL investment ranks over user selected number of years.
#'         The user selected institute is provided for comparison.
#'         The median is provided for comparison.
#' }
#'
#' @examples
#' visSummaryAllData(dataARL = ARLDataDownload,
#'                 years = c(2015, 2016, 2017, 2022, 2018, 2019))
#'
#' @export
#' @importFrom ggplot2 ggplot
#' @import magrittr
visSummaryAllData <- function(dataARL, institute, years = NA) {

  selectedData <-
    dataAdjustment(dataARL = dataARL,
                   years = years)

  yearsToDisplay <- setYearsToDispaly(years = years)


  # --- --- --- --- --- --- --- ---
  # Region
  # Visualize institute selected with medium
  summaryRegionData <- selectedData %>%
    # ensure Median removed
    dplyr::filter(! `Region` %in% ".") %>%
    dplyr::select(`Region`, `Year`) %>%
    dplyr::mutate(
      Country = dplyr::case_when(
        Region == "Canada" ~ "Canada",
        Region != "Canada" ~ "USA")) %>%
    dplyr::mutate(`Country` = factor(`Country`)) %>%
    dplyr::mutate(`Country` = relevel(`Country`, ref = "Canada")) %>%
    dplyr::group_by(Country, `Year`) %>%
    dplyr::summarise(n = n()) %>%
    ggplot2::ggplot(aes(x = factor(`Year`),
                        y = `n`,
                        fill = factor(`Country`),
                        width = .75)) +
    ggplot2::geom_bar(position = "stack", stat = "identity") +
    ggplot2::labs(y = "Number of Institutes",
                  x = "Year",
                  fill = "Country",
                  title = "Participation by Country") +
    ggplot2::theme_bw() +
    ggplot2::theme(text = element_text(size = 15, color = 'black'),
                   axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, color = 'black', size = 15),
                   axis.text.y = element_text(color = 'black', size = 15)) +
    ggplot2::scale_fill_manual(values = c("red", "lightblue")) +
    ggplot2::scale_y_continuous(labels = scales::label_comma(),
                                breaks = scales::pretty_breaks(n = 5)) +
  # Add sample sizes
  ggplot2::geom_text(aes(label = paste0("n=",`n`)),
                     position = position_stack(vjust = .5),
                     size = 6)


  # --- --- --- --- --- --- --- ---
  # Type
  # Visualize institute selected with medium
  summaryInstTypeData <- selectedData %>%
    # ensure Median removed
    dplyr::filter(! `Institution type` %in% ".") %>%
    dplyr::select(`Institution type`, `Year`) %>%
    dplyr::group_by(`Institution type`, `Year`) %>%
    dplyr::summarise(n = n()) %>%
    ggplot2::ggplot(aes(x = factor(`Year`),
                        y = `n`,
                        fill = factor(`Institution type`),
                        width = .75)) +
    ggplot2::geom_bar(position = "stack", stat = "identity") +
    ggplot2::labs(y = "Number of Institutes",
                  x = "Year",
                  fill = "Institution Type",
                  title = "Participation by Institution Type") +
    ggplot2::theme_bw() +
    ggplot2::theme(text = element_text(size = 15, color = 'black'),
                   axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, color = 'black', size = 15),
                   axis.text.y = element_text(color = 'black', size = 15)) +
    ggplot2::scale_fill_manual(values = rev(setColorPalette())) +
    ggplot2::scale_y_continuous(labels = scales::label_comma(),
                                breaks = scales::pretty_breaks(n = 5)) +
    # Add sample sizes
    ggplot2::geom_text(aes(label = paste0("n=",`n`)),
                       position = position_stack(vjust = .5),
                       size = 6)

  return(list(summaryRegionData = summaryRegionData,
              summaryInstTypeData = summaryInstTypeData))
}

# [END]
