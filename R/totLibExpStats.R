# visExpenditure
#' Plots to Compare Total Library Expenditures Over Years
#'
#' A function to visualize total library expenditures in United
#' States Dollars (USD) using multiple plot types as described below.
#' Institution types in the input data are assumed to be of the
#' categories: "Canadian", "Canadian Nonacademic",
#' "Private", "State", and "Nonacademic". For title statistics
#' visualization, the following variables (or columns) are required
#' in the dataset: "Year", "Institution Name", "Institution type",
#' "Region", "Rank in ARL investment index", "ARL investment index value",
#' and "Total library expenditures".
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
#'   \item tleUserInstitute - A lineplot comparing user selected
#'         institute over user selected number of years for total
#'         library expenditures. The median line is provided for
#'         comparison.
#'   \item tleInstCanadian - A barplot comparing Canadian institutes
#'         based on total library expenditure, along with user selected
#'         institute over user selected number of years. The median is
#'         provided for comparison.
#'   \item tleInstType - A barplot comparing maximum total library expenditure
#'         by institute type over user selected number of years. The user
#'         selected institute is provided for comparison. Institute
#'         types include: "Canadian", "Private", "State", and "Nonacademic".
#'          The median is provided for comparison.
#'   \item tleAcademicPlot - A barplot comparing maximum total library
#'         expenditure by academic institute type over user selected number
#'         of years. The user selected institute is provided for comparison.
#'         Institute types include: "Canadian" and "State". The median
#'         is provided for comparison.
#'   \item tleARLRankTop - A barplot comparing total library expenditures by
#'         top 5 ARL investment ranks over user selected number of years.
#'         The user selected institute is provided for comparison.
#'         The median is provided for comparison.
#' }
#'
#' @examples
#' visTotalLibraryExp(dataARL = ARLDataDownload,
#'               institute = "BOSTON",
#'               years = c(2015, 2016, 2017, 2022, 2018, 2019))
#'
#' @export
#' @importFrom ggplot2 ggplot
#' @import magrittr
visTotalLibraryExp <- function(dataARL, institute, years = NA) {

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
      "Canadian dollar exchange rate") %>%
    dplyr::mutate_at(
      c('Titles held',
        'Volumes held',
        'Electronic books',
        'Total library expenditures',
        'Total materials expenditures',
        'Total salaries & wages',
        'Other operating expenditures',
        'Canadian dollar exchange rate'), as.numeric) %>%
    dplyr::mutate('Total library expenditures (CAD)' = `Total library expenditures` * `Canadian dollar exchange rate`)  %>%
    dplyr::mutate('Total materials expenditures (CAD)' = `Total materials expenditures` * `Canadian dollar exchange rate`)  %>%
    dplyr::mutate('Total salaries & wages (CAD)' = `Total salaries & wages` * `Canadian dollar exchange rate`)  %>%
    dplyr::mutate('Other operating expenditures (CAD)' = `Other operating expenditures` * `Canadian dollar exchange rate`)

  yearsToDisplay <- setYearsToDispaly(years = years)
  # Phrases for testing purposes
  # cat("\n Years provided by user are:", years, "\n")
  # cat("\n Years to analyze are:", yearsToDisplay, "\n")

  # --- --- --- --- --- --- --- ---
  # Total Library Expenditures
  # Visualize institute selected with medium
  tleUserInstitute <- selectedData %>%
    dplyr::filter(`Institution Name` %in% c(institute, "MEDIAN")) %>%
    # ensure Median appear first in legend
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    dplyr::mutate(`Institution Name` = relevel(`Institution Name`, "MEDIAN")) %>%
    dplyr::filter(`Year` %in% c(yearsToDisplay)) %>% # Limit to five years
    # width = .75 ensures space between groups
    ggplot2::ggplot(aes(x = `Year`,
                        y = `Total library expenditures`,
                        width = .75)) +
    ggplot2::geom_line(linetype = "dashed",
                       linewidth = 0.5,
                       aes(color = `Institution Name`)) +
    ggplot2::geom_point(size = 0.5, aes(color = `Institution Name`)) +
    ggplot2::scale_color_manual(values = c(setColorPalette())) +
    ggplot2::labs(y = "Total library expenditures",
                  x = "Year",
                  title = "Total Library Expenditures By Selected Institute") +
    ggplot2::theme_bw() +
    ggplot2::theme(text = element_text(size = 10, color = 'black'),
                   axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, color = 'black', size = 10),
                   axis.text.y = element_text(color = 'black', size = 10)) +
    ggplot2::scale_y_continuous(labels = scales::dollar_format(),
                                breaks = scales::pretty_breaks(n = 10))


  # ---
  # Comparison of expenditures
  tleExpComp <- selectedData %>%
    dplyr::filter(`Institution Name` %in% c(institute)) %>%
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    dplyr::mutate(`Institution Name` = relevel(`Institution Name`, institute)) %>%
    dplyr::filter(`Year` %in% c(yearsToDisplay)) %>% # Limit to five years %>%
    dplyr::select(`Institution Name`, `Year`,
                  `Total materials expenditures`,
                  `Total salaries & wages`,
                  `Other operating expenditures`) %>%
    reshape2::melt(id = c("Year", "Institution Name")) %>%
    dplyr::rename("Expenditure Type" = "variable") %>%
    ggplot(aes(x = factor(`Year`),
               y = `value`,
               fill = factor(`Expenditure Type`))) +
    ggplot2::geom_bar(position = "stack", stat = "identity") +
    ggplot2::labs(y = "Total library expenditures",
                  x = "Year",
                  fill = "Type",
                  title = "Total Library Expenditures Proportion") +
    ggplot2::theme_bw() +
    ggplot2::theme(text = element_text(size = 10, color = 'black'),
                   axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, color = 'black', size = 10),
                   axis.text.y = element_text(color = 'black', size = 10)) +
    ggplot2::scale_fill_manual(values = rev(c(setColorPalette()))) +
    ggplot2::scale_y_continuous(labels = scales::label_comma(),
                                breaks = scales::pretty_breaks(n = 10))



  # ---
  # Plot of titles held Canadian institutes over 5 years
  InstSelectedData <- selectedData %>% # user selected institute
    dplyr::filter(`Institution Name` %in% institute)
  InstCadData <- selectedData %>% # Canadian institutes
    dplyr::filter(`Institution type` %in% c("Canadian",  "Canadian Nonacademic", "."))

 tleInstCanadian <- rbind(InstSelectedData, InstCadData) %>%
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
    ggplot2::geom_bar(position = "dodge", stat="identity") +
    ggplot2::labs(y = "Total library expenditures",
                  x = "Year",
                  fill = "Institute",
                  title = "Comparison With Total Library Expenditures Held By Canadian Institutes") +
    ggplot2::theme_bw() +
    ggplot2::theme(text = element_text(size = 10, color = 'black'),
                   axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, color = 'black', size = 10),
                   axis.text.y = element_text(color = 'black', size = 10)) +
    ggplot2::scale_fill_manual(values = setColorPalette()) +
    ggplot2::scale_y_continuous(labels = scales::dollar_format())



  # ---
  # Plot of institute types over 5 years, with median, highest in USA and Canada
  # selectedData$`Institution type` %>% unique()
  # [1] "."                    "State"                "Canadian"             "Private"
  # [5] "Nonacademic"          "Canadian Nonacademic"

  # Select Canadian institute with max titles
  CadAcademicMax <- selectedData %>%
    dplyr::filter(`Institution type` %in% c("Canadian", "Canadian Nonacademic")) %>%
    dplyr::group_by(`Year`) %>%
    dplyr::filter(`Total library expenditures` == max(`Total library expenditures`, na.rm = TRUE)) %>%
    dplyr::select(`Institution Name`)

  # Select non Canadian institute with max titles
  StateMax <- selectedData %>%
    #dplyr::filter(!(`Institution type` %in% c("Canadian", "."))) %>% # for "." median?
    dplyr::filter(`Institution type` %in% "State") %>%
    dplyr::group_by(`Year`) %>%
    dplyr::filter(`Total library expenditures` == max(`Total library expenditures`, na.rm = TRUE)) %>%
    dplyr::select(`Institution Name`)

  PrivateMax <- selectedData %>%
    dplyr::filter(`Institution type` %in% "Private") %>%
    dplyr::group_by(`Year`) %>%
    dplyr::filter(`Total library expenditures` == max(`Total library expenditures`, na.rm = TRUE)) %>%
    dplyr::select(`Institution Name`)

  NonacademicMax <- selectedData %>%
    dplyr::filter(`Institution type` %in% "Nonacademic") %>%
    dplyr::group_by(`Year`) %>%
    dplyr::filter(`Total library expenditures` == max(`Total library expenditures`, na.rm = TRUE)) %>%
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

  tleInstType <- topTitlesInst %>%
    # ensure Median appear first in legend
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    dplyr::mutate(`Institution Name` = relevel(`Institution Name`, ref = institute)) %>%
    dplyr::mutate(`Institution Name` = relevel(`Institution Name`, ref = "MEDIAN")) %>%
    dplyr::filter(`Year` %in% c(yearsToDisplay)) %>% # Limit to five years
    # ggplot2::ggplot(aes(x = reorder(factor(Year), +(`Titles held`)),
    ggplot2::ggplot(aes(x = factor(`Year`),
                        y = `Total library expenditures`,
                        fill = factor(`Institution Name`),
                        width = .75)) +
    ggplot2::geom_bar(position = "dodge", stat="identity") +
    ggplot2::labs(y = "Total library expenditures",
                  x = "Year",
                  fill = "Institute",
                  title = "Max Total Library Expenditures by Institute Type") +
    ggplot2::theme_bw() +
    ggplot2::theme(text = element_text(size = 10, color = 'black'),
                   axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, color = 'black', size = 10),
                   axis.text.y = element_text(color = 'black', size = 10)) +
    ggplot2::scale_fill_manual(values = setColorPalette()) +
    ggplot2::scale_y_continuous(labels = scales::dollar_format()) +
    # Add ranking labels on bars
    ggplot2::geom_text(aes(y = 0.5, label = `Institution type`),
                       position = position_dodge(width = 0.9),
                       angle = 90,
                       size = 4,
                       hjust = 'left')



  # ---
  # Join above selections together with other data for academic institutes
  topTLEAcademicInst <- dplyr::inner_join(rbind(medianTable,
                                                   userSelectTable,
                                                   CadAcademicMax,
                                                   StateMax,
                                                   PrivateMax),
                                             selectedData, by= c("Year", "Institution Name"))

  tleAcademicPlot <- topTLEAcademicInst %>%
    # ensure Median appear first in legend
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    dplyr::mutate(`Institution Name` = relevel(`Institution Name`, ref = institute)) %>%
    dplyr::mutate(`Institution Name` = relevel(`Institution Name`, ref = "MEDIAN")) %>%
    dplyr::filter(`Year` %in% c(yearsToDisplay)) %>% # Limit to five years
    ggplot2::ggplot(aes(x = factor(`Year`),
                        y = `Total library expenditures`,
                        fill = factor(`Institution Name`),
                        width = .75)) +
    ggplot2::geom_bar(position = "dodge", stat="identity") +
    ggplot2::labs(y = "Total library expenditures",
                  x = "Year",
                  fill = "Institute",
                  title = "Comparison of Max Total Library Expenditures by Academic Institute Type") +
    ggplot2::theme_bw() +
    ggplot2::theme(text = element_text(size = 10, color = 'black'),
                   axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, color = 'black', size = 10),
                   axis.text.y = element_text(color = 'black', size = 10)) +
    ggplot2::scale_fill_manual(values = setColorPalette()) +
    ggplot2::scale_y_continuous(labels = scales::dollar_format()) +
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

  tleARLRankTop <- combinedRankData %>%
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    dplyr::mutate(`Rank in ARL investment index` = factor(`Rank in ARL investment index`,
                                                          levels = c("1", "2", "3", "4", "5"))) %>%
    dplyr::mutate(`Institution Name` = relevel(`Institution Name`, ref = institute)) %>%
    dplyr::mutate(`Institution Name` = relevel(`Institution Name`, ref = "MEDIAN")) %>%
    dplyr::filter(`Year` %in% c(yearsToDisplay)) %>% # Limit to five years
    ggplot2::ggplot(aes(x = factor(`Year`),
                        y = `Total library expenditures`,
                        fill = factor(`Institution Name`),
                        width = .75)) +
    ggplot2::geom_bar(position = "dodge", stat="identity") +
    ggplot2::labs(y = "Total library expenditures",
                  x = "Year",
                  fill = "Institute",
                  title = "Total Library Expenditures by Institutes with Highest Investment ARL Rank") +
    ggplot2::theme_bw() +
    ggplot2::theme(text = element_text(size = 10, color = 'black'),
                   axis.text.x = element_text(angle = 90,
                                              hjust = 1,
                                              vjust = 0.5,
                                              color = 'black', size = 10),
                   axis.text.y = element_text(color = 'black', size = 10)) +
    ggplot2::scale_fill_manual(values = setColorPalette()) +
    ggplot2::scale_y_continuous(labels = scales::dollar_format()) +
    # Add ranking labels on bars
    ggplot2::geom_text(aes(label = `Rank in ARL investment index`),
                       position = position_dodge(width = 0.9), vjust = 0)


  return(list(tleUserInstitute = tleUserInstitute,
              tleExpComp = tleExpComp,
              tleInstCanadian = tleInstCanadian,
              tleInstType = tleInstType,
              tleAcademicPlot = tleAcademicPlot,
              tleARLRankTop = tleARLRankTop))
}

# [END]



