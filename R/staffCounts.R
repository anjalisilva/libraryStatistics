# staffCounts
#' Plots to Compare Library Staff Counts Over Years
#'
#' A function to visualize library staff counts, full time equivalent (FTE)
#' using multiple plot types as described below. Institution types in the
#' input data are assumed to be of the categories: "Canadian",
#' "Canadian Nonacademic", "Private", "State", and "Nonacademic". For
#' staff count visualization, the following variables (or columns) are
#' required in the dataset: "Year", "Institution Name", "Institution type",
#' "Region", "Rank in ARL investment index", "ARL investment index value",
#' "Professional staff", "Support staff", "Student assistants", and
#' "Total prof. + support + student staff".
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
#' @return Returns three bar plots showing staff count statistics
#' \itemize{
#'   \item staffFTEUserInstitute - A lineplot comparing user selected
#'         institute over user selected number of years for staff counts.
#'         The median line is provided for comparison.
#'   \item staffFTEInstCanadian - A barplot comparing Canadian institutes
#'         based on staff counts, along with user selected institute over
#'         user selected number of years. The median is provided for
#'         comparison.
#'   \item staffFTEInstType - A barplot comparing maximum staff counts by
#'         institute type over user selected number of years. The user
#'         selected institute is provided for comparison. Institute
#'         types include: "Canadian", "Private", "State", and "Nonacademic".
#'         The median is provided for comparison.
#'   \item staffFTEAcademicPlot - A barplot comparing maximum staff counts by
#'         academic institute type over user selected number of years.
#'         The user selected institute is provided for comparison.
#'         Institute types include: "Canadian" and "State".
#'         The median is provided for comparison.
#'   \item staffFTEARLRankTop - A barplot comparing staff counts by
#'         top 5 ARL investment ranks over user selected number of years.
#'         The user selected institute is provided for comparison.
#'         The median is provided for comparison.
#'    \ite staffFTEComp - A barplot showing proportion of professional,
#'         support and student assitant staff, making up the total
#'         staff (FTE) counts in the selected institute over the
#'         user provided period.
#' }
#'
#' @examples
#' visStaffCounts(dataARL = ARLDataDownload,
#'                institute = "TEXAS STATE",
#'                years = c(2015, 2016, 2017, 2022, 2018, 2019))
#'
#' @export
#' @importFrom ggplot2 ggplot
#' @import magrittr
visStaffCounts <- function(dataARL, institute, years = NA) {

  selectedData <-
    dataAdjustment(dataARL = dataARL,
                   years = years)

  yearsToDisplay <- setYearsToDispaly(years = years)

  # --- --- --- --- --- --- --- ---
  # staff counts
  # Visualize institute selected with medium
  staffFTEUserInstitute <- selectedData %>%
    dplyr::filter(`Institution Name` %in% c(institute, "MEDIAN")) %>%
    # ensure Median appear first in legend
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    dplyr::mutate(`Institution Name` = relevel(`Institution Name`, "MEDIAN")) %>%
    dplyr::filter(`Year` %in% c(yearsToDisplay)) %>% # Limit to five years
    # width = .75 ensures space between groups
    ggplot2::ggplot(aes(x = factor(`Year`),
                        y = `Professional staff`,
                        width = .75)) +
    ggplot2::geom_line(linetype = "dashed",
                       linewidth = 0.5,
                       aes(group = `Institution Name`,
                           color = `Institution Name`)) +
    ggplot2::geom_point(size = 0.5, aes(color = `Institution Name`)) +
    ggplot2::scale_color_manual(values = c(setColorPalette())) +
    ggplot2::labs(y = "Professional Staff (FTE)",
                  x = "Year",
                  title = "Professional Staff (FTE) By Selected Institute") +
    ggplot2::theme_bw() +
    ggplot2::theme(text = element_text(size = 15, color = 'black'),
                   axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, color = 'black', size = 10),
                   axis.text.y = element_text(color = 'black', size = 10)) +
    ggplot2::scale_y_continuous(labels = scales::label_comma(),
                                breaks = scales::pretty_breaks(n = 5))


  # ---
  # Plot of staffFTEs held Canadian institutes over 5 years
  InstSelectedData <- selectedData %>% # user selected institute
    dplyr::filter(`Institution Name` %in% institute)
  InstCadData <- selectedData %>% # Canadian institutes
    dplyr::filter(`Institution type` %in% c("Canadian",  "Canadian Nonacademic", "."))

  staffFTEInstCanadian <- rbind(InstSelectedData, InstCadData) %>%
    # ensure Median appear first in legend
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    dplyr::mutate(`Institution Name` = relevel(`Institution Name`, ref = institute)) %>%
    dplyr::mutate(`Institution Name` = relevel(`Institution Name`, ref = "MEDIAN")) %>%
    dplyr::filter(`Year` %in% c(yearsToDisplay)) %>% # Limit to five years
    # width = .75 ensures space between groups
    ggplot2::ggplot(aes(x = factor(`Year`),
                        y = `Professional staff`,
                        fill = factor(`Institution Name`),
                        width = .75)) +
    ggplot2::geom_bar(position = "dodge", stat="identity") +
    ggplot2::labs(y = "Professional Staff (FTE)",
                  x = "Year",
                  fill = "Institute",
                  title = "Professional Staff (FTE) By Canadian Institutes") +
    ggplot2::theme_bw() +
    ggplot2::theme(text = element_text(size = 15, color = 'black'),
                   axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, color = 'black', size = 10),
                   axis.text.y = element_text(color = 'black', size = 10)) +
    ggplot2::scale_fill_manual(values = setColorPalette()) +
    ggplot2::scale_y_continuous(labels = scales::label_comma(),
                                breaks = scales::pretty_breaks(n = 5))



  # ---
  # Plot of institute types over 5 years, with median, highest in USA and Canada
  # selectedData$`Institution type` %>% unique()
  # [1] "."                    "State"                "Canadian"             "Private"
  # [5] "Nonacademic"          "Canadian Nonacademic"

  # Select Canadian institute with max staffFTEs
  CadAcademicMax <- selectedData %>%
    dplyr::filter(`Professional staff` %in% c("Canadian", "Canadian Nonacademic")) %>%
    dplyr::group_by(`Year`) %>%
    dplyr::filter(`Professional staff` == max(`Professional staff`, na.rm = TRUE)) %>%
    dplyr::select(`Institution Name`)

  # Select non Canadian institute with max staffFTEs
  StateMax <- selectedData %>%
    #dplyr::filter(!(`Institution type` %in% c("Canadian", "."))) %>% # for "." median?
    dplyr::filter(`Institution type` %in% "State") %>%
    dplyr::group_by(`Year`) %>%
    dplyr::filter(`Professional staff` == max(`Professional staff`, na.rm = TRUE)) %>%
    dplyr::select(`Institution Name`)

  PrivateMax <- selectedData %>%
    dplyr::filter(`Institution type` %in% "Private") %>%
    dplyr::group_by(`Year`) %>%
    dplyr::filter(`Professional staff` == max(`Professional staff`, na.rm = TRUE)) %>%
    dplyr::select(`Institution Name`)

  NonacademicMax <- selectedData %>%
    dplyr::filter(`Institution type` %in% "Nonacademic") %>%
    dplyr::group_by(`Year`) %>%
    dplyr::filter(`Professional staff` == max(`Professional staff`, na.rm = TRUE)) %>%
    dplyr::select(`Institution Name`)

  # Join above selections together with other data for all institutes
  medianTable <- tibble::tibble(Year = yearsToDisplay,
                                `Institution Name` = rep("MEDIAN", length(yearsToDisplay)))
  userSelectTable <- tibble::tibble(Year = yearsToDisplay,
                                    `Institution Name` = rep(institute, length(yearsToDisplay)))
  topStaffFTEInst <- dplyr::inner_join(rbind(medianTable,
                                            userSelectTable,
                                            CadAcademicMax,
                                            StateMax,
                                            PrivateMax,
                                            NonacademicMax),
                                      selectedData, by= c("Year", "Institution Name"))

  staffFTEInstType <- topStaffFTEInst %>%
    # ensure Median appear first in legend
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    dplyr::mutate(`Institution Name` = relevel(`Institution Name`, ref = institute)) %>%
    dplyr::mutate(`Institution Name` = relevel(`Institution Name`, ref = "MEDIAN")) %>%
    dplyr::filter(`Year` %in% c(yearsToDisplay)) %>% # Limit to five years
    # ggplot2::ggplot(aes(x = reorder(factor(Year), +(`staffFTEs held`)),
    ggplot2::ggplot(aes(x = factor(`Year`),
                        y = `Professional staff`,
                        fill = factor(`Institution Name`),
                        width = .75)) +
    ggplot2::geom_bar(position = "dodge", stat="identity") +
    ggplot2::labs(y = "Professional Staff (FTE)",
                  x = "Year",
                  fill = "Institute",
                  title = "Max Professional Staff (FTE) Counts by Institute Type") +
    ggplot2::theme_bw() +
    ggplot2::theme(text = element_text(size = 15, color = 'black'),
                   axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, color = 'black', size = 10),
                   axis.text.y = element_text(color = 'black', size = 10)) +
    ggplot2::scale_fill_manual(values = setColorPalette()) +
    ggplot2::scale_y_continuous(labels = scales::label_comma(),
                                breaks = scales::pretty_breaks(n = 5)) +
    # Add ranking labels on bars
    ggplot2::geom_text(aes(y = 0.5, label = `Institution type`),
                       position = position_dodge(width = 0.9),
                       angle = 90,
                       size = 4,
                       hjust = 'left')



  # ---
  # Join above selections together with other data for academic institutes
  topStaffFTEsAcademicInst <- dplyr::inner_join(rbind(medianTable,
                                                    userSelectTable,
                                                    CadAcademicMax,
                                                    StateMax,
                                                    PrivateMax),
                                              selectedData, by= c("Year", "Institution Name"))

  staffFTEAcademicPlot <- topStaffFTEsAcademicInst %>%
    # ensure Median appear first in legend
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    dplyr::mutate(`Institution Name` = relevel(`Institution Name`, ref = institute)) %>%
    dplyr::mutate(`Institution Name` = relevel(`Institution Name`, ref = "MEDIAN")) %>%
    dplyr::filter(`Year` %in% c(yearsToDisplay)) %>% # Limit to five years
    ggplot2::ggplot(aes(x = factor(`Year`),
                        y = `Professional staff`,
                        fill = factor(`Institution Name`),
                        width = .75)) +
    ggplot2::geom_bar(position = "dodge", stat="identity") +
    ggplot2::labs(y = "Professional Staff (FTE)",
                  x = "Year",
                  fill = "Institute",
                  title = "Max Professional Staff (FTE) Counts by Academic Institute Type") +
    ggplot2::theme_bw() +
    ggplot2::theme(text = element_text(size = 15, color = 'black'),
                   axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, color = 'black', size = 10),
                   axis.text.y = element_text(color = 'black', size = 10)) +
    ggplot2::scale_fill_manual(values = setColorPalette()) +
    ggplot2::scale_y_continuous(labels = scales::label_comma(),
                                breaks = scales::pretty_breaks(n = 5)) +
    # Add ranking labels on bars
    ggplot2::geom_text(aes(y = 0.5, label = `Institution type`),
                       position = position_dodge(width = 0.9),
                       angle = 90,
                       size = 4,
                       hjust = 'left')




  # ---
  # Plot comparing top 5 ARL ranks and their staffFTEs

  topARLRankData <- selectedData %>%
    dplyr::filter(`Rank in ARL investment index` %in% c("1", "2", "3", "4", "5"))

  selectARLRankData <- selectedData %>%
    dplyr::filter(`Institution Name` %in% c("MEDIAN", institute))

  combinedRankData <- rbind(topARLRankData, selectARLRankData)

  staffFTEARLRankTop <- combinedRankData %>%
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    dplyr::mutate(`Rank in ARL investment index` = factor(`Rank in ARL investment index`)) %>%
    dplyr::mutate(`Institution Name` = relevel(`Institution Name`, ref = institute)) %>%
    dplyr::mutate(`Institution Name` = relevel(`Institution Name`, ref = "MEDIAN")) %>%
    dplyr::filter(`Year` %in% c(yearsToDisplay)) %>% # Limit to five years
    ggplot2::ggplot(aes(x = factor(`Year`),
                        y = `Professional staff`,
                        fill = factor(`Institution Name`),
                        width = .75)) +
    ggplot2::geom_bar(position = "dodge", stat="identity") +
    ggplot2::labs(y = "Professional Staff (FTE)",
                  x = "Year",
                  fill = "Institute",
                  title = "Professional Staff (FTE) by Institutes with Highest Investment ARL Rank") +
    ggplot2::theme_bw() +
    ggplot2::theme(text = element_text(size = 15, color = 'black'),
                   axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, color = 'black', size = 10),
                   axis.text.y = element_text(color = 'black', size = 10)) +
    ggplot2::scale_fill_manual(values = setColorPalette()) +
    ggplot2::scale_y_continuous(labels = scales::label_comma(),
                                breaks = scales::pretty_breaks(n = 5)) +
    # Add ranking labels on bars
    ggplot2::geom_text(aes(label = `Rank in ARL investment index`),
                       position = position_dodge(width = 0.9), vjust = 0)



  # ---
  # Comparison of Professional to All Staff
  staffFTEComp <- selectedData %>%
    dplyr::filter(`Institution Name` %in% c(institute)) %>%
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    dplyr::mutate(`Institution Name` = relevel(`Institution Name`, institute)) %>%
    dplyr::filter(`Year` %in% c(yearsToDisplay)) %>% # Limit to five years %>%
    dplyr::select(`Institution Name`, `Year`,
                  `Professional staff`,
                  `Support staff`,
                  `Student assistants`) %>%
    reshape2::melt(id = c("Year", "Institution Name")) %>%
    dplyr::rename(`Staff Type` = "variable") %>%
    dplyr::mutate(`Staff Type` = factor(`Staff Type`)) %>%
    dplyr::mutate(`Staff Type` = relevel(`Staff Type`, "Professional staff")) %>%
    ggplot(aes(x = factor(`Year`),
               y = `value`,
               fill = factor(`Staff Type`))) +
    ggplot2::geom_bar(position = "stack", stat = "identity") +
    ggplot2::labs(y = "Staff Counts (FTE)",
                  x = "Year",
                  fill = "Type",
                  title = " Staff Counts (FTE) Proportion By Selected Institute") +
    ggplot2::theme_bw() +
    ggplot2::theme(text = element_text(size = 15, color = 'black'),
                   axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, color = 'black', size = 10),
                   axis.text.y = element_text(color = 'black', size = 10)) +
    ggplot2::scale_fill_manual(values = rev(c(setColorPalette()))[4:6]) +
    ggplot2::scale_y_continuous(labels = scales::label_comma(),
                                breaks = scales::pretty_breaks(n = 5))



  return(list(staffFTEUserInstitute = staffFTEUserInstitute,
              staffFTEInstCanadian = staffFTEInstCanadian,
              staffFTEInstType = staffFTEInstType,
              staffFTEAcademicPlot = staffFTEAcademicPlot,
              staffFTEARLRankTop = staffFTEARLRankTop,
              staffFTEComp = staffFTEComp))
}
# [END]
