# Presentations to groups and participants
#' Plots to Compare Presentations and Participants Over Years
#'
#' A function to visualize the number of presentations and
#' participants attending over the years using multiple plot types
#' as described below. Institution types in the input data are
#' assumed to be of the categories: "Canadian", "Canadian Nonacademic",
#' "Private", "State", and "Nonacademic". For presentation and
#' participant statistics visualization, the following variables
#' (or columns) are required in the dataset: "Year", "Institution Name",
#' "Institution type", "Region", "Rank in ARL investment index",
#' "ARL investment index value", "Group presentations", and
#' "Presentation participants".
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
#'   \item presUserInstitute - A lineplot comparing user selected
#'         institute over user selected number of years for presentations
#'         and participants attended. The median line is provided for comparison.
#'   \item presInstCanadian - A barplot comparing Canadian institutes
#'         based on group presentations, along with user selected institute over
#'         user selected number of years. The median is provided for
#'         comparison.
#'   \item presInstType - A barplot comparing maximum presentations by
#'         institute type over user selected number of years. The user
#'         selected institute is provided for comparison. Institute
#'         types include: "Canadian", "Private", "State", and "Nonacademic".
#'         The median is provided for comparison.
#'   \item presAcademicPlot - A barplot comparing maximum presentations by
#'         academic institute type over user selected number of years.
#'         The user selected institute is provided for comparison.
#'         Institute types include: "Canadian" and "State". The median
#'         is provided for comparison.
#'   \item presARLRankTop - A barplot comparing presentations by
#'         top 5 ARL investment ranks over user selected number of years.
#'         The user selected institute is provided for comparison.
#'         The median is provided for comparison.
#'   \item pressAllData - Violin plots showing the distribution
#'         of group presentations for all institutes in the dataset uploaded by user
#'         for the years chosen by the user.
#'
#' }
#'
#' @examples
#' visPresentationData(dataARL = ARLDataDownload,
#'               institute = "TEXAS STATE",
#'               years = c(2015, 2016, 2017, 2022, 2018, 2019))
#'
#' @export
#' @importFrom ggplot2 ggplot
#' @import magrittr
visPresentationData <- function(dataARL, institute, years = NA) {

  selectedData <-
    dataAdjustment(dataARL = dataARL,
                   years = years)

  yearsToDisplay <-
    setYearsToDispaly(years = years)

  # --- --- --- --- --- --- --- ---
  # presentations
  # Visualize institute selected with medium
  presUserInstitute <- selectedData %>%
    dplyr::filter(`Institution Name` %in% c(institute, "MEDIAN")) %>%
    # ensure Median appear first in legend
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    dplyr::mutate(`Institution Name` = relevel(`Institution Name`, "MEDIAN")) %>%
    # dplyr::filter(`Year` %in% c(yearsToDisplay)) %>% # Limit to five years
    # width = .75 ensures space between groups
    ggplot2::ggplot(aes(x = factor(`Year`),
                        y = `Group presentations`,
                        width = .75)) +
    ggplot2::geom_line(linetype = "dashed",
                       linewidth = 0.5,
                       aes(group = `Institution Name`,
                           color = `Institution Name`)) +
    ggplot2::geom_point(size = 0.5, aes(color = `Institution Name`)) +
    ggplot2::scale_color_manual(values = c(setColorPalette())) +
    ggplot2::labs(y = "Group Presentations",
                  x = "Year",
                  color = "Institute",
                  title = "Group Presentations by Selected Institute") +
    ggplot2::theme_bw() +
    ggplot2::theme(text = element_text(size = 15, color = 'black'),
                   axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, color = 'black', size = 15),
                   axis.text.y = element_text(color = 'black', size = 15)) +
    ggplot2::scale_y_continuous(labels = scales::label_comma(),
                                breaks = scales::pretty_breaks(n = 5))


  # ---
  # participants
  # Visualize institute selected with medium
  partUserInstitute <- selectedData %>%
    dplyr::filter(`Institution Name` %in% c(institute, "MEDIAN")) %>%
    # ensure Median appear first in legend
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    dplyr::mutate(`Institution Name` = relevel(`Institution Name`, "MEDIAN")) %>%
    # dplyr::filter(`Year` %in% c(yearsToDisplay)) %>% # Limit to five years
    # width = .75 ensures space between groups
    ggplot2::ggplot(aes(x = factor(`Year`),
                        y = `Presentation participants`,
                        width = .75)) +
    ggplot2::geom_line(linetype = "dashed",
                       linewidth = 0.5,
                       aes(group = `Institution Name`,
                           color = `Institution Name`)) +
    ggplot2::geom_point(size = 0.5, aes(color = `Institution Name`)) +
    ggplot2::scale_color_manual(values = c(setColorPalette())) +
    ggplot2::labs(y = "Presentation Participants",
                  x = "Year",
                  color = "Institute",
                  title = "Participants by Selected Institute") +
    ggplot2::theme_bw() +
    ggplot2::theme(text = element_text(size = 15, color = 'black'),
                   axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, color = 'black', size = 15),
                   axis.text.y = element_text(color = 'black', size = 15)) +
    ggplot2::scale_y_continuous(labels = scales::label_comma(),
                                breaks = scales::pretty_breaks(n = 5))



  # ---
  # Plot of presentations held Canadian institutes over 5 years
  InstSelectedData <- selectedData %>% # user selected institute
    dplyr::filter(`Institution Name` %in% institute)
  InstCadData <- selectedData %>% # Canadian institutes
    dplyr::filter(`Institution type` %in% c("Canadian",  "Canadian Nonacademic", "."))

  presInstCanadian <- rbind(InstSelectedData, InstCadData) %>%
    # ensure Median appear first in legend
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    dplyr::mutate(`Institution Name` = relevel(`Institution Name`, ref = institute)) %>%
    dplyr::mutate(`Institution Name` = relevel(`Institution Name`, ref = "MEDIAN")) %>%
    # dplyr::filter(`Year` %in% c(yearsToDisplay)) %>% # Limit to five years
    # width = .75 ensures space between groups
    ggplot2::ggplot(aes(x = factor(`Year`),
                        y = `Group presentations`,
                        fill = factor(`Institution Name`),
                        width = .75)) +
    ggplot2::geom_bar(position = "dodge", stat = "identity") +
    ggplot2::labs(y = "Group Presentations",
                  x = "Year",
                  fill = "Institute",
                  title = "Group Presentations by Canadian Institutes") +
    ggplot2::theme_bw() +
    ggplot2::theme(text = element_text(size = 15, color = 'black'),
                   axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, color = 'black', size = 15),
                   axis.text.y = element_text(color = 'black', size = 15)) +
    ggplot2::scale_fill_manual(values = setColorPalette()) +
    ggplot2::scale_y_continuous(labels = scales::label_comma(),
                                breaks = scales::pretty_breaks(n = 5))



  # ---
  # Plot of institute types over 5 years, with median, highest in USA and Canada
  # selectedData$`Institution type` %>% unique()
  # [1] "."                    "State"                "Canadian"             "Private"
  # [5] "Nonacademic"          "Canadian Nonacademic"

  # Select Canadian institute with max articles
  CadAcademicMax <- selectedData %>%
    dplyr::filter(`Institution type` %in% c("Canadian", "Canadian Nonacademic")) %>%
    dplyr::group_by(`Year`) %>%
    dplyr::filter(`Group presentations` ==
                    max(`Group presentations`, na.rm = TRUE)) %>%
    dplyr::select(`Institution Name`)

  # Select non Canadian institute with max articles
  StateMax <- selectedData %>%
    #dplyr::filter(!(`Institution type` %in% c("Canadian", "."))) %>% # for "." median?
    dplyr::filter(`Institution type` %in% "State") %>%
    dplyr::group_by(`Year`) %>%
    dplyr::filter(`Group presentations` ==
                    max(`Group presentations`, na.rm = TRUE)) %>%
    dplyr::select(`Institution Name`)

  PrivateMax <- selectedData %>%
    dplyr::filter(`Institution type` %in% "Private") %>%
    dplyr::group_by(`Year`) %>%
    dplyr::filter(`Group presentations` ==
                    max(`Group presentations`, na.rm = TRUE)) %>%
    dplyr::select(`Institution Name`)

  NonacademicMax <- selectedData %>%
    dplyr::filter(`Institution type` %in% "Nonacademic") %>%
    dplyr::group_by(`Year`) %>%
    dplyr::filter(`Group presentations` ==
                    max(`Group presentations`, na.rm = TRUE)) %>%
    dplyr::select(`Institution Name`)

  # Join above selections together with other data for all institutes
  medianTable <- tibble::tibble(Year = yearsToDisplay,
                                `Institution Name` = rep("MEDIAN", length(yearsToDisplay)))
  userSelectTable <- tibble::tibble(Year = yearsToDisplay,
                                    `Institution Name` = rep(institute, length(yearsToDisplay)))
  topArticlesInst <- dplyr::inner_join(rbind(medianTable,
                                             userSelectTable,
                                             CadAcademicMax,
                                             StateMax,
                                             PrivateMax,
                                             NonacademicMax),
                                       selectedData, by = c("Year", "Institution Name"))

  presInstType <- topArticlesInst %>%
    # ensure Median appear first in legend
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    dplyr::mutate(`Institution Name` = relevel(`Institution Name`, ref = institute)) %>%
    dplyr::mutate(`Institution Name` = relevel(`Institution Name`, ref = "MEDIAN")) %>%
    # dplyr::filter(`Year` %in% c(yearsToDisplay)) %>% # Limit to five years
    # ggplot2::ggplot(aes(x = reorder(factor(Year), +(`Titles held`)),
    ggplot2::ggplot(aes(x = factor(`Year`),
                        y = `Group presentations`,
                        fill = factor(`Institution Name`),
                        width = .75)) +
    ggplot2::geom_bar(position = "dodge", stat = "identity") +
    ggplot2::labs(y = "Group Presentations",
                  x = "Year",
                  fill = "Institute",
                  title = "Max Group Presentations by Institute Type") +
    ggplot2::theme_bw() +
    ggplot2::theme(text = element_text(size = 15, color = 'black'),
                   axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, color = 'black', size = 15),
                   axis.text.y = element_text(color = 'black', size = 15)) +
    ggplot2::scale_fill_manual(values = setColorPalette()) +
    ggplot2::scale_y_continuous(labels = scales::label_comma(),
                                breaks = scales::pretty_breaks(n = 5)) +
    # Add ranking labels on bars
    ggplot2::geom_text(aes(y = 0.5, label = `Institution type`),
                       position = position_dodge(width = 0.9),
                       angle = 90,
                       size = 6,
                       hjust = 'left')



  # ---
  # Join above selections together with other data for academic institutes
  topArticlesAcademicInst <- dplyr::inner_join(rbind(medianTable,
                                                     userSelectTable,
                                                     CadAcademicMax,
                                                     StateMax,
                                                     PrivateMax),
                                               selectedData, by = c("Year", "Institution Name"))

  presAcademicPlot <- topArticlesAcademicInst %>%
    # ensure Median appear first in legend
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    dplyr::mutate(`Institution Name` = relevel(`Institution Name`, ref = institute)) %>%
    dplyr::mutate(`Institution Name` = relevel(`Institution Name`, ref = "MEDIAN")) %>%
    # dplyr::filter(`Year` %in% c(yearsToDisplay)) %>% # Limit to five years
    ggplot2::ggplot(aes(x = factor(`Year`),
                        y = `Group presentations`,
                        fill = factor(`Institution Name`),
                        width = .75)) +
    ggplot2::geom_bar(position = "dodge", stat = "identity") +
    ggplot2::labs(y = "Group Presentations",
                  x = "Year",
                  fill = "Institute",
                  title = "Max Group Presentations by Academic Institute Type") +
    ggplot2::theme_bw() +
    ggplot2::theme(text = element_text(size = 15, color = 'black'),
                   axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, color = 'black', size = 15),
                   axis.text.y = element_text(color = 'black', size = 15)) +
    ggplot2::scale_fill_manual(values = setColorPalette()) +
    ggplot2::scale_y_continuous(labels = scales::label_comma(),
                                breaks = scales::pretty_breaks(n = 5)) +
    # Add ranking labels on bars
    ggplot2::geom_text(aes(y = 0.5, label = `Institution type`),
                       position = position_dodge(width = 0.9),
                       angle = 90,
                       size = 6,
                       hjust = 'left')




  # ---
  # Plot comparing top 5 ARL ranks and their articles

  topARLRankData <- selectedData %>%
    dplyr::filter(`Rank in ARL investment index` %in% c("1", "2", "3", "4", "5"))

  selectARLRankData <- selectedData %>%
    dplyr::filter(`Institution Name` %in% c("MEDIAN", institute))

  combinedRankData <- rbind(topARLRankData, selectARLRankData)

  presARLRankTop <- combinedRankData %>%
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    dplyr::mutate(`Rank in ARL investment index` = factor(`Rank in ARL investment index`)) %>%
    dplyr::mutate(`Institution Name` = relevel(`Institution Name`, ref = institute)) %>%
    dplyr::mutate(`Institution Name` = relevel(`Institution Name`, ref = "MEDIAN")) %>%
    # dplyr::filter(`Year` %in% c(yearsToDisplay)) %>% # Limit to five years
    ggplot2::ggplot(aes(x = factor(`Year`),
                        y = `Group presentations`,
                        fill = factor(`Institution Name`),
                        width = .75)) +
    ggplot2::geom_bar(position = "dodge", stat = "identity") +
    ggplot2::labs(y = "Group Presentations",
                  x = "Year",
                  fill = "Institute",
                  title = "Group Presentations by Institutes with Highest Investment ARL Rank") +
    ggplot2::theme_bw() +
    ggplot2::theme(text = element_text(size = 15, color = 'black'),
                   axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, color = 'black', size = 15),
                   axis.text.y = element_text(color = 'black', size = 15)) +
    ggplot2::scale_fill_manual(values = setColorPalette()) +
    ggplot2::scale_y_continuous(labels = scales::label_comma(),
                                breaks = scales::pretty_breaks(n = 5)) +
    # Add ranking labels on bars
    ggplot2::geom_text(aes(label = `Rank in ARL investment index`),
                       position = position_dodge(width = 0.9),
                       vjust = 0,
                       size = 6)


  # ---
  # Presentations for all dataset
  presAllData <- selectedData %>%
    ggplot2::ggplot(aes(x = factor(`Year`),
                        y = `Group presentations`,
                        width = .75)) +
    ggplot2::geom_violin() +
    ggplot2::stat_summary(fun = median, geom = "point", size = 2, color = setColorPalette()[1]) +
    ggplot2::scale_color_manual(values = c(setColorPalette())) +
    ggplot2::labs(y = "Group Presentations",
                  x = "Year",
                  title = "Distribution of Group Presentations in Dataset") +
    ggplot2::theme_bw() +
    ggplot2::theme(text = element_text(size = 15, color = 'black'),
                   axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, color = 'black', size = 15),
                   axis.text.y = element_text(color = 'black', size = 15)) +
    ggplot2::scale_y_continuous(labels = scales::label_comma(),
                                breaks = scales::pretty_breaks(n = 5)) +
    EnvStats::stat_n_text(size = 6)

  # ---
  # Participants for all dataset
  participantsAllData <- selectedData %>%
    ggplot2::ggplot(aes(x = factor(`Year`),
                        y = `Presentation participants`,
                        width = .75)) +
    ggplot2::geom_violin() +
    ggplot2::stat_summary(fun = median, geom = "point", size = 2, color = setColorPalette()[1]) +
    ggplot2::scale_color_manual(values = c(setColorPalette())) +
    ggplot2::labs(y = "Presentation Participants",
                  x = "Year",
                  title = "Distribution of Participants in Dataset") +
    ggplot2::theme_bw() +
    ggplot2::theme(text = element_text(size = 15, color = 'black'),
                   axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, color = 'black', size = 15),
                   axis.text.y = element_text(color = 'black', size = 15)) +
    ggplot2::scale_y_continuous(labels = scales::label_comma(),
                                breaks = scales::pretty_breaks(n = 5)) +
    EnvStats::stat_n_text(size = 6)


  return(list(presUserInstitute = presUserInstitute,
              partUserInstitute = partUserInstitute,
              presInstCanadian = presInstCanadian,
              presInstType = presInstType,
              presAcademicPlot = presAcademicPlot,
              presARLRankTop = presARLRankTop,
              presAllData = presAllData,
              participantsAllData = participantsAllData))
}

# [END]



