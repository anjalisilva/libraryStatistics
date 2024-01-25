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
#' visExpenditureData(dataARL = ARLDataDownload,
#'                    institute = "TORONTO",
#'                    years = c(2015, 2016, 2017, 2018, 2019))
#'
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

