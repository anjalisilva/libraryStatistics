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
#'   \item tleAllData - Violin plots showing the distribution
#'         of total library expenditures for all institutes in dataset
#'         uploaded by user for the years chosen by the user.
#'   \item tleUserInstitute - A lineplot comparing user selected
#'         institute over user selected number of years for total
#'         library expenditures. The median line is provided for
#'         comparison.
#'   \item tleExpComp - A barplot showing the proportion making up total
#'         library expenditures for user selected institute over user selected
#'         number of years. The three categories include total materials
#'         expenditures, total salaries and wages, and other operating expenditures.
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
#'   \item tleARLRankTopPerFaculty - A barplot comparing user selected institute
#'         with institutes with high ARL investment ranks, for total library
#'         expenditures per teaching faculty, over user selected number of years.
#'   \item tleARLRankTopPerStudent - A barplot comparing user selected institute
#'         with institutes with high ARL investment ranks, for total library
#'         expenditures per total students reported that are either part-time (PT)
#'         or full-time (FT), over user selected number of years.
#'   \item tleARLRankTopPerGradStudent - A barplot comparing user selected institute
#'         with institutes with high ARL investment ranks, for total library
#'         expenditures per total graduate students reported that are either
#'         part-time (PT) or full-time (FT), over user selected number of years.
#'   \item tleARLRankTopPerDoctoral - A barplot comparing user selected institute
#'         with institutes with high ARL investment ranks, for total library
#'         expenditures per doctoral degrees awarded, over user selected number
#'         of years.
#'   \item tleTop - A barplot comparing user selected institute with institutes
#'         with highest value for total library expenditures, over user selected
#'         number of years.
#'   \item tleTopPerFaculty - A barplot comparing user selected institute
#'         with institutes with highest value for total library
#'         expenditures per teaching faculty, over user selected number of years.
#'   \item tleTopPerStudent - A barplot comparing user selected institute
#'         with institutes with highest value for total library
#'         expenditures per total students reported that are either part-time (PT)
#'         or full-time (FT), over user selected number of years.
#'   \item tleTopPerGradStudent - A barplot comparing user selected institute
#'         with institutes with highest value for total library
#'         expenditures per total graduate students reported that are either
#'         part-time (PT) or full-time (FT), over user selected number of years.
#'   \item tleTopPerDoctoral - A barplot comparing user selected institute
#'         with institutes with highest value for total library
#'         expenditures per doctoral degrees awarded, over user selected number
#'         of years.
#' }
#'
#' @examples
#' visTotalLibraryExp(dataARL = ARLDataDownload,
#'                    institute = "BOSTON",
#'                    years = c(2015, 2016, 2017, 2022, 2018, 2019))
#'
#' @export
#' @importFrom ggplot2 ggplot
#' @import magrittr
visTotalLibraryExp <- function(dataARL, institute, years = NA) {

  selectedData <-
    dataAdjustment(dataARL = dataARL,
                   years = years)

  yearsToDisplay <- setYearsToDispaly(years = years)


  # --- --- --- --- --- --- --- ---
  # Total Library Expenditures
  # Visualize institute selected with medium
  tleUserInstitute <- selectedData %>%
    dplyr::filter(`Institution Name` %in% c(institute, "MEDIAN")) %>%
    # ensure Median appear first in legend
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    dplyr::mutate(`Institution Name` = relevel(`Institution Name`, "MEDIAN")) %>%
    # width = .75 ensures space between groups
    ggplot2::ggplot(aes(x = factor(`Year`),
                        y = `Total library expenditures`,
                        width = .75)) +
    ggplot2::geom_line(linetype = "dashed",
                       linewidth = 0.5,
                       aes(group = `Institution Name`,
                           color = `Institution Name`)) +
    ggplot2::geom_point(size = 0.5, aes(color = `Institution Name`)) +
    ggplot2::scale_color_manual(values = c(setColorPalette())) +
    ggplot2::labs(y = "Total Library Expenditures",
                  x = "Year",
                  color = "Institute",
                  title = "Total Library Expenditures by Selected Institute") +
    ggplot2::theme_bw() +
    ggplot2::theme(text = element_text(size = 15, color = 'black'),
                   axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, color = 'black', size = 15),
                   axis.text.y = element_text(color = 'black', size = 15)) +
    ggplot2::scale_y_continuous(labels = scales::dollar_format(),
                                breaks = scales::pretty_breaks(n = 5))


  # ---
  # Comparison of expenditures
  tleExpComp <- selectedData %>%
    dplyr::filter(`Institution Name` %in% c(institute)) %>%
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    dplyr::mutate(`Institution Name` = relevel(`Institution Name`, institute)) %>%
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
    ggplot2::labs(y = "Total Library Expenditures",
                  x = "Year",
                  fill = "Type",
                  title = "Total Library Expenditures Proportion\nby Selected Institute") +
    ggplot2::theme_bw() +
    ggplot2::theme(text = element_text(size = 15, color = 'black'),
                   axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, color = 'black', size = 15),
                   axis.text.y = element_text(color = 'black', size = 15)) +
    ggplot2::scale_fill_manual(values = rev(c(setColorPalette()))) +
    ggplot2::scale_y_continuous(labels = scales::label_comma(),
                                breaks = scales::pretty_breaks(n = 5))
    # Show percentage
    # +
    # ggplot2::geom_text(aes(label = scales::percent(`value`)),
    #                   position = "stack", vjust = +2.1, size = 3)
    #


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
    # width = .75 ensures space between groups
    ggplot2::ggplot(aes(x = factor(`Year`),
                        y = `Total library expenditures`,
                        fill = factor(`Institution Name`),
                        width = .75)) +
    ggplot2::geom_bar(position = "dodge", stat="identity") +
    ggplot2::labs(y = "Total Library Expenditures",
                  x = "Year",
                  fill = "Institute",
                  title = "Total Library Expenditures by Canadian Institutes") +
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
                                     selectedData,
                                     by = c("Year", "Institution Name"))

  tleInstType <- topTitlesInst %>%
    # ensure Median appear first in legend
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    dplyr::mutate(`Institution Name` = relevel(`Institution Name`, ref = institute)) %>%
    dplyr::mutate(`Institution Name` = relevel(`Institution Name`, ref = "MEDIAN")) %>%
    # ggplot2::ggplot(aes(x = reorder(factor(Year), +(`Titles held`)),
    ggplot2::ggplot(aes(x = factor(`Year`),
                        y = `Total library expenditures`,
                        fill = factor(`Institution Name`),
                        width = .75)) +
    ggplot2::geom_bar(position = "dodge", stat="identity") +
    ggplot2::labs(y = "Total Library Expenditures",
                  x = "Year",
                  fill = "Institute",
                  title = "Maximum Total Library Expenditures by Institute Type") +
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
  topTLEAcademicInst <- dplyr::inner_join(rbind(medianTable,
                                                   userSelectTable,
                                                   CadAcademicMax,
                                                   StateMax,
                                                   PrivateMax),
                                             selectedData, by = c("Year", "Institution Name"))

  tleAcademicPlot <- topTLEAcademicInst %>%
    # ensure Median appear first in legend
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    dplyr::mutate(`Institution Name` = relevel(`Institution Name`, ref = institute)) %>%
    dplyr::mutate(`Institution Name` = relevel(`Institution Name`, ref = "MEDIAN")) %>%
   ggplot2::ggplot(aes(x = factor(`Year`),
                        y = `Total library expenditures`,
                        fill = factor(`Institution Name`),
                        width = .75)) +
    ggplot2::geom_bar(position = "dodge", stat="identity") +
    ggplot2::labs(y = "Total Library Expenditures",
                  x = "Year",
                  fill = "Institute",
                  title = "Maximum Total Library Expenditures by Academic Institute Type") +
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
  # Plot comparing top 5 ARL ranks and their exp
  topARLRankData <- selectedData %>%
    dplyr::filter(`Rank in ARL investment index` %in% c("1", "2", "3", "4", "5"))

  selectARLRankData <- selectedData %>%
    dplyr::filter(`Institution Name` %in% c("MEDIAN", institute))

  combinedARLRankData <- rbind(topARLRankData, selectARLRankData)

  tleARLRankTop <- combinedARLRankData %>%
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    dplyr::mutate(`Rank in ARL investment index` = factor(`Rank in ARL investment index`)) %>%
    dplyr::mutate(`Institution Name` = relevel(`Institution Name`, ref = institute)) %>%
    dplyr::mutate(`Institution Name` = relevel(`Institution Name`, ref = "MEDIAN")) %>%
    ggplot2::ggplot(aes(x = factor(`Year`),
                        y = `Total library expenditures`,
                        fill = factor(`Institution Name`),
                        width = .75)) +
    ggplot2::geom_bar(position = "dodge", stat="identity") +
    ggplot2::labs(y = "Total Library Expenditures",
                  x = "Year",
                  fill = "Institute") +
    ggplot2::theme_bw() +
    ggplot2::ggtitle(label = "Total Library Expenditures by Institutes with Highest Investment ARL Rank",
                     subtitle = "ARL rank is shown on top of each bar; median value in green and selected institute in red color.") +
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
  # Total Library Expenditures by Institutes with Highest Total Library Expenditures
  topTLEAllData <- selectedData %>%
    dplyr::group_by(`Year`) %>%
    dplyr::top_n(5, `Total library expenditures`)

  selectInstituteData <- selectedData %>%
    dplyr::filter(`Institution Name` %in% c("MEDIAN", institute))

  combinedRankData <- rbind(topTLEAllData, selectInstituteData)

  tleTop <- combinedRankData %>%
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    dplyr::mutate(`Institution Name` = relevel(`Institution Name`, institute)) %>%
    dplyr::mutate(`Institution Name` = relevel(`Institution Name`, ref = "MEDIAN")) %>%
    ggplot2::ggplot(aes(x = factor(`Year`),
                        y = `Total library expenditures`,
                        fill = factor(`Institution Name`),
                        width = .75)) +
    ggplot2::geom_bar(position = "dodge", stat="identity") +
    ggplot2::labs(y = "Total Library Expenditures",
                  x = "Year",
                  fill = "Institute") +
    ggplot2::theme_bw()  +
    ggplot2::ggtitle(label = "Total Library Expenditures by Institutes with Highest Total Library Expenditures",
                     subtitle = "ARL rank is shown on top of each bar; median value in green and selected institute in red color.") +
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
  # Using total lib stats per faculty by ARL rank

  # combinedARLRankData was defined in line 333
  tleARLRankTopPerFaculty <- combinedARLRankData %>%
    dplyr::mutate(expPerFaculty = `Total library expenditures`/`Total teaching faculty`) %>%
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    dplyr::mutate(`Rank in ARL investment index` = factor(`Rank in ARL investment index`)) %>%
    dplyr::mutate(`Institution Name` = relevel(`Institution Name`, ref = institute)) %>%
    dplyr::filter(! `Institution Name` %in% "MEDIAN") %>%
    ggplot2::ggplot(aes(x = factor(`Year`),
                        y = `expPerFaculty`,
                        fill = factor(`Institution Name`),
                        width = .75)) +
    ggplot2::geom_bar(position = "dodge", stat = "identity") +
    ggplot2::labs(y = "Total Library Expenditures\nPer Teaching Faculty",
                  x = "Year",
                  fill = "Institute") +
    ggplot2::theme_bw() +
    ggplot2::ggtitle(label = "Total Library Expenditures Per Teaching Faculty\nby Institutes with Highest Investment ARL Rank",
                     subtitle = "ARL rank is shown on top of each bar; selected institute in red color.") +
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
  # Using total lib stats per student by ARL rank

  # combinedARLRankData was defined in line 333
  tleARLRankTopPerStudent <- combinedARLRankData %>%
    dplyr::mutate(expPerStudent = `Total library expenditures`/
                    (`Total fulltime students` + `Part-time students, undergraduate and graduate`)) %>%
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    dplyr::mutate(`Rank in ARL investment index` = factor(`Rank in ARL investment index`)) %>%
    dplyr::mutate(`Institution Name` = relevel(`Institution Name`, ref = institute)) %>%
    dplyr::filter(! `Institution Name` %in% "MEDIAN") %>%
    ggplot2::ggplot(aes(x = factor(`Year`),
                        y = `expPerStudent`,
                        fill = factor(`Institution Name`),
                        width = .75)) +
    ggplot2::geom_bar(position = "dodge", stat = "identity") +
    ggplot2::labs(y = "Total Library Expenditures\nPer Student",
                  x = "Year",
                  fill = "Institute") +
    ggplot2::theme_bw() +
    ggplot2::ggtitle(label = "Total Library Expenditures Per Student (FT + PT)\nby Institutes with Highest Investment ARL Rank",
                     subtitle = "ARL rank is shown on top of each bar; selected institute in red color.") +
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
  # Using total lib stats per graduate student by ARL rank

  # combinedARLRankData was defined in line 333
  tleARLRankTopPerGradStudent <- combinedARLRankData %>%
    dplyr::mutate(expPerStudent = `Total library expenditures`/
                    (`Part-time graduate students` + `Total fulltime graduate students`)) %>%
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    dplyr::mutate(`Rank in ARL investment index` = factor(`Rank in ARL investment index`)) %>%
    dplyr::mutate(`Institution Name` = relevel(`Institution Name`, ref = institute)) %>%
    dplyr::filter(! `Institution Name` %in% "MEDIAN") %>%
    ggplot2::ggplot(aes(x = factor(`Year`),
                        y = `expPerStudent`,
                        fill = factor(`Institution Name`),
                        width = .75)) +
    ggplot2::geom_bar(position = "dodge", stat = "identity") +
    ggplot2::labs(y = "Total Library Expenditures\nPer Grad Student",
                  x = "Year",
                  fill = "Institute") +
    ggplot2::theme_bw() +
    ggplot2::ggtitle(label = "Total Library Expenditures Per Grad Student (FT + PT)\nby Institutes with Highest Investment ARL Rank",
                     subtitle = "ARL rank is shown on top of each bar; selected institute in red color.") +
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
  # Using total lib stats per doctoral degree by ARL rank

  # combinedARLRankData was defined in line 333
  tleARLRankTopPerDoctoral <- combinedARLRankData %>%
    dplyr::mutate(expPerStudent = `Total library expenditures`/ `Doctor's degrees awarded`) %>%
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    dplyr::mutate(`Rank in ARL investment index` = factor(`Rank in ARL investment index`)) %>%
    dplyr::mutate(`Institution Name` = relevel(`Institution Name`, ref = institute)) %>%
    dplyr::filter(! `Institution Name` %in% "MEDIAN") %>%
    ggplot2::ggplot(aes(x = factor(`Year`),
                        y = `expPerStudent`,
                        fill = factor(`Institution Name`),
                        width = .75)) +
    ggplot2::geom_bar(position = "dodge", stat = "identity") +
    ggplot2::labs(y = "Total Library Expenditures\nPer Doctoral Degree",
                  x = "Year",
                  fill = "Institute") +
    ggplot2::theme_bw()  +
    ggplot2::ggtitle(label = "Total Library Expenditures Per Doctoral Degree\nby Institutes with Highest Investment ARL Rank",
                     subtitle = "ARL rank is shown on top of each bar; selected institute in red color.") +
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
  # Using total lib stats in the entire dataset
  tleAllData <- selectedData %>%
    # Remove median value as it is not a true entry
    dplyr::filter(! `Institution Name` %in% c(institute, "MEDIAN")) %>%
    ggplot2::ggplot(aes(x = factor(`Year`),
                        y = `Total library expenditures`,
                        width = .75)) +
    ggplot2::geom_violin() +
    ggplot2::stat_summary(fun = median, geom = "point", size = 2, color = setColorPalette()[1]) +
    ggplot2::scale_color_manual(values = c(setColorPalette())) +
    ggplot2::labs(y = "Total Library Expenditures",
                  x = "Year") +
    ggplot2::theme_bw() +
    ggplot2::ggtitle(label = "Distribution of Total Library Expenditures in Dataset",
            subtitle = "The sample size (n) equals number of institutes submitting data.") +
    ggplot2::theme(text = element_text(size = 15, color = 'black'),
                   # plot.title = element_text(hjust = 0.5),
                   # plot.subtitle = element_text(hjust = 0.5),
                   axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, color = 'black', size = 15),
                   axis.text.y = element_text(color = 'black', size = 15)) +
    ggplot2::scale_y_continuous(labels = scales::label_comma(),
                                breaks = scales::pretty_breaks(n = 5)) +
    EnvStats::stat_n_text(size = 6)


  # ---
  # Using total lib stats per faculty by top contributors (not ARL)
  tlePerFaculty <- selectedData %>%
    # Remove median value as it is not a true entry
    dplyr::filter(! `Institution Name` %in% c(institute, "MEDIAN")) %>%
    dplyr::mutate(expPerFaculty = `Total library expenditures`/`Total teaching faculty`) %>%
    # Replace INF values with NA
    dplyr::mutate(expPerFaculty = na_if(expPerFaculty, Inf)) %>%
    dplyr::group_by(`Year`) %>%
    dplyr::top_n(5, expPerFaculty) %>%
    dplyr::arrange(`Year`, desc(expPerFaculty))

  selectInstPerFaculty <- selectedData %>%
    dplyr::filter(`Institution Name` %in% institute) %>%
    dplyr::mutate(expPerFaculty = `Total library expenditures`/`Total teaching faculty`) %>%
    # Replace INF values with NA
    dplyr::mutate(expPerFaculty = na_if(expPerFaculty, Inf))

  combinedPerFaculty <- rbind(tlePerFaculty, selectInstPerFaculty)

  tleTopPerFaculty <- combinedPerFaculty %>%
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    dplyr::mutate(`Institution Name` = relevel(`Institution Name`, ref = institute)) %>%
    ggplot2::ggplot(aes(x = factor(`Year`),
                        y = `expPerFaculty`,
                        fill = factor(`Institution Name`),
                        width = .75)) +
    ggplot2::geom_bar(position = "dodge", stat = "identity") +
    ggplot2::labs(y = "Total Library Expenditures\nPer Teaching Faculty",
                  x = "Year",
                  fill = "Institute") +
    ggplot2::theme_bw() +
    ggplot2::ggtitle(label = "Institutes with Highest Total Library Expenditures\nPer Teaching Faculty",
                     subtitle = "ARL rank is shown on top of each bar; selected institute in red color.") +
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
  # Using total lib stats per student by top contributors (not ARL)
  tlePerStudent <- selectedData %>%
    # Remove median value as it is not a true entry
    dplyr::filter(! `Institution Name` %in% c(institute, "MEDIAN")) %>%
    dplyr::mutate(expPerStudent = `Total library expenditures`/
                    (`Total fulltime students` + `Part-time students, undergraduate and graduate`)) %>%
    # Replace INF values with NA
    dplyr::mutate(expPerStudent = na_if(expPerStudent, Inf)) %>%
    dplyr::group_by(`Year`) %>%
    dplyr::top_n(5, expPerStudent) %>%
    dplyr::arrange(`Year`, desc(expPerStudent))

  selectInstPerStudent <- selectedData %>%
    dplyr::filter(`Institution Name` %in% institute) %>%
    dplyr::mutate(expPerStudent = `Total library expenditures`/
                    (`Total fulltime students` + `Part-time students, undergraduate and graduate`)) %>%
    # Replace INF values with NA
    dplyr::mutate(expPerStudent = na_if(expPerStudent, Inf))

  combinedPerStudent <- rbind(tlePerStudent, selectInstPerStudent)

  tleTopPerStudent <- combinedPerStudent %>%
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    dplyr::mutate(`Institution Name` = relevel(`Institution Name`, ref = institute)) %>%
    ggplot2::ggplot(aes(x = factor(`Year`),
                        y = `expPerStudent`,
                        fill = factor(`Institution Name`),
                        width = .75)) +
    ggplot2::geom_bar(position = "dodge", stat = "identity") +
    ggplot2::labs(y = "Total Library Expenditures\nPer Student",
                  x = "Year",
                  fill = "Institute") +
    ggplot2::theme_bw() +
    ggplot2::ggtitle(label = "Institutes with Highest Total Library Expenditures\nPer Student (FT + PT)",
                     subtitle = "ARL rank is shown on top of each bar; selected institute in red color.") +
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
  # Using total lib stats per graduate student by top contributors (not ARL)
  tlePerGradStudent <- selectedData %>%
    # Remove median value as it is not a true entry
    dplyr::filter(! `Institution Name` %in% c(institute, "MEDIAN")) %>%
    dplyr::mutate(expPerStudent = `Total library expenditures`/
                    (`Part-time graduate students` + `Total fulltime graduate students`)) %>%
    # Replace INF values with NA
    dplyr::mutate(expPerStudent = na_if(expPerStudent, Inf)) %>%
    dplyr::group_by(`Year`) %>%
    dplyr::top_n(5, expPerStudent) %>%
    dplyr::arrange(`Year`, desc(expPerStudent))

  selectInstGrad <- selectedData %>%
    dplyr::filter(`Institution Name` %in% institute) %>%
    dplyr::mutate(expPerStudent = `Total library expenditures`/
                    (`Part-time graduate students` + `Total fulltime graduate students`)) %>%
    # Replace INF values with NA
    dplyr::mutate(expPerStudent = na_if(expPerStudent, Inf))

  combinedTopData <- rbind(tlePerGradStudent, selectInstGrad)

  tleTopPerGradStudent <- combinedTopData %>%
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    dplyr::mutate(`Institution Name` = relevel(`Institution Name`, ref = institute)) %>%
    ggplot2::ggplot(aes(x = factor(`Year`),
                        y = `expPerStudent`,
                        fill = factor(`Institution Name`),
                        width = .75)) +
    ggplot2::geom_bar(position = "dodge", stat = "identity") +
    ggplot2::labs(y = "Total Library Expenditures\nPer Grad Student",
                  x = "Year",
                  fill = "Institute") +
    ggplot2::theme_bw() +
    ggplot2::ggtitle(label = "Institutes with Highest Total Library Expenditures\nPer Grad Student (FT + PT)",
                     subtitle = "ARL rank is shown on top of each bar; selected institute in red color.") +
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
  # Using total lib stats per doctoral degree by top contributors (not ARL)
  tleTopPerDoctoral <- selectedData %>%
    # Remove median value as it is not a true entry
    dplyr::filter(! `Institution Name` %in% c(institute, "MEDIAN")) %>%
    dplyr::mutate(expPerStudent = `Total library expenditures`/ `Doctor's degrees awarded`) %>%
    # Replace INF values with NA
    dplyr::mutate(expPerStudent = na_if(expPerStudent, Inf)) %>%
    dplyr::group_by(`Year`) %>%
    dplyr::top_n(5, expPerStudent) %>%
    dplyr::arrange(`Year`, desc(expPerStudent))

  selectInstDoc <- selectedData %>%
    dplyr::filter(`Institution Name` %in% institute) %>%
    dplyr::mutate(expPerStudent = `Total library expenditures`/ `Doctor's degrees awarded`) %>%
    # Replace INF values with NA
    dplyr::mutate(expPerStudent = na_if(expPerStudent, Inf))

  combinedTopDoc <- rbind(tleTopPerDoctoral, selectInstDoc)

  tleTopPerDoctoral <- combinedTopDoc %>%
    dplyr::mutate(expPerStudent = `Total library expenditures`/ `Doctor's degrees awarded`) %>%
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    dplyr::mutate(`Rank in ARL investment index` = factor(`Rank in ARL investment index`)) %>%
    dplyr::mutate(`Institution Name` = relevel(`Institution Name`, ref = institute)) %>%
    dplyr::filter(! `Institution Name` %in% "MEDIAN") %>%
    ggplot2::ggplot(aes(x = factor(`Year`),
                        y = `expPerStudent`,
                        fill = factor(`Institution Name`),
                        width = .75)) +
    ggplot2::geom_bar(position = "dodge", stat = "identity") +
    ggplot2::labs(y = "Total Library Expenditures\nPer Doctoral Degree",
                  x = "Year",
                  fill = "Institute") +
    ggplot2::theme_bw() +
    ggplot2::ggtitle(label = "Institutes with Highest Total Library Expenditures\nPer Doctoral Degree",
                     subtitle = "ARL rank is shown on top of each bar; selected institute in red color.") +
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




  return(list(tleAllData = tleAllData,
              tleUserInstitute = tleUserInstitute,
              tleExpComp = tleExpComp,
              tleInstCanadian = tleInstCanadian,
              tleInstType = tleInstType,
              tleAcademicPlot = tleAcademicPlot,
              tleARLRankTop = tleARLRankTop,
              tleARLRankTopPerFaculty = tleARLRankTopPerFaculty,
              tleARLRankTopPerStudent = tleARLRankTopPerStudent,
              tleARLRankTopPerGradStudent = tleARLRankTopPerGradStudent,
              tleARLRankTopPerDoctoral = tleARLRankTopPerDoctoral,
              tleTop = tleTop,
              tleTopPerFaculty = tleTopPerFaculty,
              tleTopPerStudent = tleTopPerStudent,
              tleTopPerGradStudent = tleTopPerGradStudent,
              tleTopPerDoctoral = tleTopPerDoctoral))
}

# [END]



