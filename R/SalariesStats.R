# visExpenditure
#' Plots to Compare Salaries Over Years
#'
#' A function to visualize total library salaries in United
#' States Dollars (USD) using multiple plot types as described below.
#' Institution types in the input data are assumed to be of the
#' categories: "Canadian", "Canadian Nonacademic",
#' "Private", "State", and "Nonacademic". For title statistics
#' visualization, the following variables (or columns) are required
#' in the dataset: "Year", "Institution Name", "Institution type",
#' "Region", "Rank in ARL investment index", "ARL investment index value",
#' and "Total salaries & wages".
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
#' @return Returns three bar plots showing salary statistics
#' \itemize{
#'   \item salariesUserInstitute - A lineplot comparing user selected
#'         institute over user selected number of years for total
#'         salaries and wages (in USD). The median line is provided for
#'         comparison.
#'   \item salariesInstCanadian - A barplot comparing Canadian institutes
#'         based on total salaries and wages (in USD), along with user selected
#'         institute over user selected number of years. The median is
#'         provided for comparison.
#'   \item salariesInstType - A barplot comparing maximum total salaries and
#'         wages (in USD) by institute type over user selected number of years. The user
#'         selected institute is provided for comparison. Institute
#'         types include: "Canadian", "Private", "State", and "Nonacademic".
#'          The median is provided for comparison.
#'   \item salariesAcademicPlot - A barplot comparing maximum salaries and
#'         wages (in USD) by academic institute type over user selected number
#'         of years. The user selected institute is provided for comparison.
#'         Institute types include: "Canadian" and "State". The median
#'         is provided for comparison.
#'   \item salariesARLRankTop - A barplot comparing salaries and wages (in
#'         USD) by top 5 ARL investment ranks over user selected number of years.
#'         The user selected institute is provided for comparison.
#'         The median is provided for comparison.
#'   \item salariesExpComp - A barplot showing proportion of professional,
#'         support and student assistant staff making up the total salaries,
#'         over the period selected by user.
#' }
#'
#' @examples
#' visLibrarySalaries(dataARL = ARLDataDownload,
#'                    institute = "BOSTON",
#'                    years = c(2015, 2016, 2017, 2022, 2018, 2019))
#'
#' @export
#' @importFrom ggplot2 ggplot
#' @import magrittr
visLibrarySalaries <- function(dataARL, institute, years = NA) {

  selectedData <-
    dataAdjustment(dataARL = dataARL,
                   years = years)

  yearsToDisplay <- setYearsToDispaly(years = years)

  # --- --- --- --- --- --- --- ---
  # Total Library Salaries
  # Visualize institute selected with medium
  salariesUserInstitute <- selectedData %>%
    dplyr::filter(`Institution Name` %in% c(institute, "MEDIAN")) %>%
    # ensure Median appear first in legend
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    dplyr::mutate(`Institution Name` = relevel(`Institution Name`, "MEDIAN")) %>%
    dplyr::filter(`Year` %in% c(yearsToDisplay)) %>% # Limit to five years
    # width = .75 ensures space between groups
    ggplot2::ggplot(aes(x = factor(`Year`),
                        y = `Total salaries & wages`,
                        width = .75)) +
    ggplot2::geom_line(linetype = "dashed",
                       linewidth = 0.5,
                       aes(group = `Institution Name`,
                           color = `Institution Name`)) +
    ggplot2::geom_point(size = 0.5, aes(color = `Institution Name`)) +
    ggplot2::scale_color_manual(values = c(setColorPalette())) +
    ggplot2::labs(y = "Total Salaries & Wages",
                  x = "Year",
                  color = "Institute",
                  title = "Total Salaries & Wages by Selected Institute") +
    ggplot2::theme_bw() +
    ggplot2::theme(text = element_text(size = 15, color = 'black'),
                   axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, color = 'black', size = 15),
                   axis.text.y = element_text(color = 'black', size = 15)) +
    ggplot2::scale_y_continuous(labels = scales::dollar_format(),
                                breaks = scales::pretty_breaks(n = 5))


  # ---
  # Comparison of salaries
  salariesExpComp <- selectedData %>%
    dplyr::filter(`Institution Name` %in% c(institute)) %>%
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    dplyr::mutate(`Institution Name` = relevel(`Institution Name`, institute)) %>%
    dplyr::filter(`Year` %in% c(yearsToDisplay)) %>% # Limit to five years %>%
    dplyr::select(`Institution Name`, `Year`,
                  `Professional salaries & wages`,
                  `Support staff salaries & wages`,
                  `Student assistant wages`) %>%
    reshape2::melt(id = c("Year", "Institution Name")) %>%
    dplyr::rename("Salaries Type" = "variable") %>%
    ggplot(aes(x = factor(`Year`),
               y = `value`,
               fill = factor(`Salaries Type`))) +
    ggplot2::geom_bar(position = "stack", stat = "identity") +
    ggplot2::labs(y = "Total Salaries & Wages",
                  x = "Year",
                  fill = "Type",
                  title = "Total Salaries & Wages Proportion\nby Selected Institute") +
    ggplot2::theme_bw() +
    ggplot2::theme(text = element_text(size = 15, color = 'black'),
                   axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, color = 'black', size = 15),
                   axis.text.y = element_text(color = 'black', size = 15)) +
    ggplot2::scale_fill_manual(values = rev(c(setColorPalette()))[4:6]) +
    ggplot2::scale_y_continuous(labels = scales::dollar_format(),
                                breaks = scales::pretty_breaks(n = 5))



  # ---
  # Plot of salaries for Canadian institutes over 5 years
  InstSelectedData <- selectedData %>% # user selected institute
    dplyr::filter(`Institution Name` %in% institute)
  InstCadData <- selectedData %>% # Canadian institutes
    dplyr::filter(`Institution type` %in% c("Canadian",  "Canadian Nonacademic", "."))

  salariesInstCanadian <- rbind(InstSelectedData, InstCadData) %>%
    # ensure Median appear first in legend
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    dplyr::mutate(`Institution Name` = relevel(`Institution Name`, ref = institute)) %>%
    dplyr::mutate(`Institution Name` = relevel(`Institution Name`, ref = "MEDIAN")) %>%
    dplyr::filter(`Year` %in% c(yearsToDisplay)) %>% # Limit to five years
    # width = .75 ensures space between groups
    ggplot2::ggplot(aes(x = factor(`Year`),
                        y = `Total salaries & wages`,
                        fill = factor(`Institution Name`),
                        width = .75)) +
    ggplot2::geom_bar(position = "dodge", stat="identity") +
    ggplot2::labs(y = "Total Salaries & Wages",
                  x = "Year",
                  fill = "Institute",
                  title = "Total Salaries & Wages Held by Canadian Institutes") +
    ggplot2::theme_bw() +
    ggplot2::theme(text = element_text(size = 15, color = 'black'),
                   axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, color = 'black', size = 15),
                   axis.text.y = element_text(color = 'black', size = 15)) +
    ggplot2::scale_fill_manual(values = setColorPalette()) +
    ggplot2::scale_y_continuous(labels = scales::dollar_format(),
                                breaks = scales::pretty_breaks(n = 5))



  # ---
  # Plot of institute types over 5 years, with median, highest in USA and Canada
  # selectedData$`Institution type` %>% unique()
  # [1] "."                    "State"                "Canadian"             "Private"
  # [5] "Nonacademic"          "Canadian Nonacademic"

  # Select Canadian institute with max salaries
  CadAcademicMax <- selectedData %>%
    dplyr::filter(`Institution type` %in% c("Canadian", "Canadian Nonacademic")) %>%
    dplyr::group_by(`Year`) %>%
    dplyr::filter(`Total salaries & wages` == max(`Total salaries & wages`, na.rm = TRUE)) %>%
    dplyr::select(`Institution Name`)

  # Select non Canadian institute with max salaries
  StateMax <- selectedData %>%
    #dplyr::filter(!(`Institution type` %in% c("Canadian", "."))) %>% # for "." median?
    dplyr::filter(`Institution type` %in% "State") %>%
    dplyr::group_by(`Year`) %>%
    dplyr::filter(`Total salaries & wages` == max(`Total salaries & wages`, na.rm = TRUE)) %>%
    dplyr::select(`Institution Name`)

  PrivateMax <- selectedData %>%
    dplyr::filter(`Institution type` %in% "Private") %>%
    dplyr::group_by(`Year`) %>%
    dplyr::filter(`Total salaries & wages` == max(`Total salaries & wages`, na.rm = TRUE)) %>%
    dplyr::select(`Institution Name`)

  NonacademicMax <- selectedData %>%
    dplyr::filter(`Institution type` %in% "Nonacademic") %>%
    dplyr::group_by(`Year`) %>%
    dplyr::filter(`Total salaries & wages` == max(`Total salaries & wages`, na.rm = TRUE)) %>%
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
                                     selectedData,
                                     by = c("Year", "Institution Name"))

  salariesInstType <- topTitlesInst %>%
    # ensure Median appear first in legend
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    dplyr::mutate(`Institution Name` = relevel(`Institution Name`, ref = institute)) %>%
    dplyr::mutate(`Institution Name` = relevel(`Institution Name`, ref = "MEDIAN")) %>%
    dplyr::filter(`Year` %in% c(yearsToDisplay)) %>% # Limit to five years
    # ggplot2::ggplot(aes(x = reorder(factor(Year), +(`Titles held`)),
    ggplot2::ggplot(aes(x = factor(`Year`),
                        y = `Total salaries & wages`,
                        fill = factor(`Institution Name`),
                        width = .75)) +
    ggplot2::geom_bar(position = "dodge", stat="identity") +
    ggplot2::labs(y = "Total Salaries & Wages",
                  x = "Year",
                  fill = "Institute",
                  title = "Max Total Salaries & Wages by Institute Type") +
    ggplot2::theme_bw() +
    ggplot2::theme(text = element_text(size = 15, color = 'black'),
                   axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, color = 'black', size = 15),
                   axis.text.y = element_text(color = 'black', size = 15)) +
    ggplot2::scale_fill_manual(values = setColorPalette()) +
    ggplot2::scale_y_continuous(labels = scales::dollar_format(),
                                breaks = scales::pretty_breaks(n = 5)) +
    # Add ranking labels on bars
    ggplot2::geom_text(aes(y = 0.5, label = `Institution type`),
                       position = position_dodge(width = 0.9),
                       angle = 90,
                       size = 6,
                       hjust = 'left')



  # ---
  # Join above selections together with other data for academic institutes
  topSalariesAcademicInst <- dplyr::inner_join(rbind(medianTable,
                                                userSelectTable,
                                                CadAcademicMax,
                                                StateMax,
                                                PrivateMax),
                                          selectedData,
                                          by = c("Year", "Institution Name"))

  salariesAcademicPlot <- topSalariesAcademicInst %>%
    # ensure Median appear first in legend
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    dplyr::mutate(`Institution Name` = relevel(`Institution Name`, ref = institute)) %>%
    dplyr::mutate(`Institution Name` = relevel(`Institution Name`, ref = "MEDIAN")) %>%
    dplyr::filter(`Year` %in% c(yearsToDisplay)) %>% # Limit to five years
    ggplot2::ggplot(aes(x = factor(`Year`),
                        y = `Total salaries & wages`,
                        fill = factor(`Institution Name`),
                        width = .75)) +
    ggplot2::geom_bar(position = "dodge", stat = "identity") +
    ggplot2::labs(y = "Total Salaries & Wages",
                  x = "Year",
                  fill = "Institute",
                  title = "Max Total Salaries & Wages by Academic Institute Type") +
    ggplot2::theme_bw() +
    ggplot2::theme(text = element_text(size = 15, color = 'black'),
                   axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, color = 'black', size = 15),
                   axis.text.y = element_text(color = 'black', size = 15)) +
    ggplot2::scale_fill_manual(values = setColorPalette()) +
    ggplot2::scale_y_continuous(labels = scales::dollar_format(),
                                breaks = scales::pretty_breaks(n = 5)) +
    # Add ranking labels on bars
    ggplot2::geom_text(aes(y = 0.5, label = `Institution type`),
                       position = position_dodge(width = 0.9),
                       angle = 90,
                       size = 6,
                       hjust = 'left')




  # ---
  # Plot comparing top 5 ARL ranks and their salaries

  topARLRankData <- selectedData %>%
    dplyr::filter(`Rank in ARL investment index` %in% c("1", "2", "3", "4", "5"))

  selectARLRankData <- selectedData %>%
    dplyr::filter(`Institution Name` %in% c("MEDIAN", institute))

  combinedRankData <- rbind(topARLRankData, selectARLRankData)

  salariesARLRankTop <- combinedRankData %>%
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    dplyr::mutate(`Rank in ARL investment index` = factor(`Rank in ARL investment index`)) %>%
    dplyr::mutate(`Institution Name` = relevel(`Institution Name`, ref = institute)) %>%
    dplyr::mutate(`Institution Name` = relevel(`Institution Name`, ref = "MEDIAN")) %>%
    dplyr::filter(`Year` %in% c(yearsToDisplay)) %>% # Limit to five years
    ggplot2::ggplot(aes(x = factor(`Year`),
                        y = `Total salaries & wages`,
                        fill = factor(`Institution Name`),
                        width = .75)) +
    ggplot2::geom_bar(position = "dodge", stat="identity") +
    ggplot2::labs(y = "Total Salaries & Wages",
                  x = "Year",
                  fill = "Institute",
                  title = "Total Salaries & Wages by Institutes with Highest Investment ARL Rank") +
    ggplot2::theme_bw() +
    ggplot2::theme(text = element_text(size = 15, color = 'black'),
                   axis.text.x = element_text(angle = 90,
                                              hjust = 1,
                                              vjust = 0.5,
                                              color = 'black', size = 15),
                   axis.text.y = element_text(color = 'black', size = 15)) +
    ggplot2::scale_fill_manual(values = setColorPalette()) +
    ggplot2::scale_y_continuous(labels = scales::dollar_format(),
                                breaks = scales::pretty_breaks(n = 5)) +
    # Add ranking labels on bars
    ggplot2::geom_text(aes(label = `Rank in ARL investment index`),
                       position = position_dodge(width = 0.9),
                       vjust = 0,
                       size = 6)


  # ---
  # Prof staff salaries per prof staff count ratio
  salProfStaffperCount <- combinedRankData %>%
    dplyr::mutate(salProfStaffperCount = `Professional salaries & wages` / `Professional staff`) %>%
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    dplyr::mutate(`Rank in ARL investment index` = factor(`Rank in ARL investment index`)) %>%
    dplyr::mutate(`Institution Name` = relevel(`Institution Name`, ref = institute)) %>%
    dplyr::filter(! `Institution Name` %in% "MEDIAN") %>%
    dplyr::filter(`Year` %in% c(yearsToDisplay)) %>% # Limit to five years
    ggplot2::ggplot(aes(x = factor(`Year`),
                        y = `salProfStaffperCount`,
                        fill = factor(`Institution Name`),
                        width = .75)) +
    ggplot2::geom_bar(position = "dodge", stat="identity") +
    ggplot2::labs(y = "Professional Staff Salaries Per\nProfessional Staff (FTE)",
                  x = "Year",
                  fill = "Institute",
                  title = "Professional Staff Salaries Per Professional Staff\nby Institutes with Highest Investment ARL Rank") +
    ggplot2::theme_bw() +
    ggplot2::theme(text = element_text(size = 15, color = 'black'),
                   axis.text.x = element_text(angle = 90,
                                              hjust = 1,
                                              vjust = 0.5,
                                              color = 'black', size = 15),
                   axis.text.y = element_text(color = 'black', size = 15)) +
    ggplot2::scale_fill_manual(values = setColorPalette()[-1]) +
    ggplot2::scale_y_continuous(labels = scales::dollar_format(),
                                breaks = scales::pretty_breaks(n = 5)) +
    # Add ranking labels on bars
    ggplot2::geom_text(aes(label = `Rank in ARL investment index`),
                       position = position_dodge(width = 0.9),
                       vjust = 0,
                       size = 6)


  # ---
  # Support staff salaries per support staff count ratio
  salSupportStaffperCount <- combinedRankData %>%
    dplyr::mutate(salSupportStaffperCount = `Support staff salaries & wages` / `Support staff`) %>%
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    dplyr::mutate(`Rank in ARL investment index` = factor(`Rank in ARL investment index`)) %>%
    dplyr::mutate(`Institution Name` = relevel(`Institution Name`, ref = institute)) %>%
    dplyr::filter(! `Institution Name` %in% "MEDIAN") %>%
    dplyr::filter(`Year` %in% c(yearsToDisplay)) %>% # Limit to five years
    ggplot2::ggplot(aes(x = factor(`Year`),
                        y = `salSupportStaffperCount`,
                        fill = factor(`Institution Name`),
                        width = .75)) +
    ggplot2::geom_bar(position = "dodge", stat="identity") +
    ggplot2::labs(y = "Support Staff Salaries Per\nSupport Staff (FTE)",
                  x = "Year",
                  fill = "Institute",
                  title = "Support Staff Salaries Per Support Staff\nby Institutes with Highest Investment ARL Rank") +
    ggplot2::theme_bw() +
    ggplot2::theme(text = element_text(size = 15, color = 'black'),
                   axis.text.x = element_text(angle = 90,
                                              hjust = 1,
                                              vjust = 0.5,
                                              color = 'black', size = 15),
                   axis.text.y = element_text(color = 'black', size = 15)) +
    ggplot2::scale_fill_manual(values = setColorPalette()[-1]) +
    ggplot2::scale_y_continuous(labels = scales::dollar_format(),
                                breaks = scales::pretty_breaks(n = 5)) +
    # Add ranking labels on bars
    ggplot2::geom_text(aes(label = `Rank in ARL investment index`),
                       position = position_dodge(width = 0.9),
                       vjust = 0,
                       size = 6)



  return(list(salariesUserInstitute = salariesUserInstitute,
              salariesExpComp = salariesExpComp,
              salariesInstCanadian = salariesInstCanadian,
              salariesInstType = salariesInstType,
              salariesAcademicPlot = salariesAcademicPlot,
              salariesARLRankTop = salariesARLRankTop,
              salProfStaffperCount = salProfStaffperCount,
              salSupportStaffperCount = salSupportStaffperCount))
}

# [END]



