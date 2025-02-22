# visExpenditure
#' Plots to Compare Total Library Expenditures Over Years
#'
#' A function to visualize total library expenditures in United
#' States Dollars (USD) as ratios in comparison to various statistics
#' reported in the annual survey of Association of Research Libraries (ARL)
#' as bar plots. Note, this function provides question 6 of ARL survey
#' as the numerator of the ratio.
#'
#'@param dataARL A dataframe containing ARL survey data directly
#'   downloaded from ARL platform. The years should be placed along
#'   rows. The first column must be 'Year', followed by other variables
#'   in no particular order, e.g., 'Institution Name', 'Institution type',
#'   etc. To download data from ARL Data Portal, it is recommended that
#'   all variables are selected, with columns being 'Variables' and data
#'   sorted by 'Institution Name' (default options).
#'@param members A character vector specifying up to five ARL members
#'   of interest, as identified in the dataset. E.g.,
#'   c("BOSTON", "TORONTO", "OTTAWA", "LAVAL", "HARVARD").
#'@param years A numeric vector specifying up to 5 calendar years
#'   for which data should be plotted, e.g., c(2015, 2016, 2017,
#'   2018, 2019). If no value is provided (i.e., NA), then most
#'   recent five years available in the uploaded data will be used.
#'   If more than 5 values provided, last 5 values will be selected.
#'   Default is NA.
#'
#' @return Returns bar plots and tables showing varying ratios specified below:
#' \itemize{
#'   \item tleTopPerFaculty - A barplot showing ARL members with highest ratio of total
#'         library expenditures per teaching faculty, over user selected number of years.
#'   \item tleTopPerStudent - A barplot showing ARL members with highest ratio of total
#'         library expenditures per student (full-time, FT, and part-time, PT), over
#'          user selected number of years.
#'   \item tleTopPerGradStudent - A barplot showing ARL members with highest ratio of total
#'         library expenditures per graduate student (full-time, FT, and part-time, PT),
#'         over user selected number of years.
#'   \item tleTopPerUndergradStudent - A barplot showing ARL members with highest ratio of total
#'         library expenditures per undergraduate student (full-time, FT, and part-time, PT),
#'         over user selected number of years.
#'   \item tleTopPerDoctoral - A barplot showing ARL members with highest ratio of total
#'         library expenditures per doctoral degree awarded, over user selected number
#'         of years.
#'   \item tlePerFacultyUserSelected - A barplot showing ratio of total library expenditures per
#'         teaching faculty for user selected ARL members, over user selected number of years.
#'   \item tlePerStudentUserSelected - A barplot showing ratio of total library expenditures per
#'         student (full-time, FT, and part-time, PT) for user selected ARL members, over user
#'         selected number of years.
#'   \item tlePerGradStudentUserSelected - A barplot showing ratio of total library expenditures per
#'         graduate student (full-time, FT, and part-time, PT) for user selected ARL members, over user
#'         selected number of years.
#'   \item tlePerUndergradStudentUserSelected - A barplot showing ratio of total library expenditures per
#'         undergraduate student (full-time, FT, and part-time, PT) for user selected ARL members, over user
#'         selected number of years.
#'   \item tleTopPerDoctoralUserSelected - A barplot showing ratio of total library expenditures per
#'         per doctoral degree awarded for user selected ARL members, over user
#'         selected number of years.
#'   \item tleTopPerFacultyTable - A table showing the original values used for calculating
#          the ratios for ARL members with highest ratio of total
#'         library expenditures per teaching faculty, over user selected number of years.
#'   \item tleTopPerStudentTable - A table showing the original values used for calculating
#          the ratios for ARL members with highest ratio of total
#'         library expenditures per student (full-time, FT, and part-time, PT), over
#'          user selected number of years.
#'   \item tleTopPerGradStudentTable - A table showing the original values used for calculating
#          the ratios for ARL members with highest ratio of total
#'         library expenditures per graduate student (full-time, FT, and part-time, PT),
#'         over user selected number of years.
#'   \item tleTopPerUndergradStudentTable - A table showing the original values used for calculating
#          the ratios for ARL members with highest ratio of total
#'         library expenditures per undergraduate student (full-time, FT, and part-time, PT),
#'         over user selected number of years.
#'   \item tleTopPerDoctoralTable - A table showing the original values used for calculating
#          the ratios for ARL members with highest ratio of total
#'         library expenditures per doctoral degree awarded, over user selected number
#'         of years.
#'   \item tlePerFacultyUserSelectedTable - A table showing the original values used for calculating
#          the ratios for total library expenditures per
#'         teaching faculty for user selected ARL members, over user selected number of years.
#'   \item tlePerStudentUserSelectedTable - A table showing the original values used for calculating
#          the ratios for total library expenditures per
#'         graduate student (full-time, FT, and part-time, PT) for user selected ARL members, over user
#'         selected number of years.
#'   \item tlePerGradStudentUserSelectedTable - A table showing the original values used for calculating
#          the ratios for total library expenditures per
#'         graduate student (full-time, FT, and part-time, PT) for user selected ARL members, over user
#'         selected number of years.
#'   \item tlePerUndergradStudentUserSelectedTable - A table showing the original values used for calculating
#          the ratios for total library expenditures per
#'         undergraduate student (full-time, FT, and part-time, PT) for user selected ARL members, over user
#'         selected number of years.
#'   \item tlePerDoctoralUserSelectedTable - A table showing the original values used for calculating
#          the ratios for total library expenditures per
#'         per doctoral degree awarded for user selected ARL members, over user
#'         selected number of years.
#' }
#'
#' @examples
#' # Reading R package example data
#' # ?ARLDataDownload
#' visTotalLibraryExp(dataARL = ARLDataDownload,
#'                    members = c("Library A", "Library B", "Library C", "Library D", "Library E"),
#'                    years = c(2020, 2021, 2022))
#'
#' # Reading actual data downloaded from ARL (not run)
#' # Set file path
#' # ARLData <- readr::read_csv("~/ARLData.csv")
#' # visTotalLibraryExp(dataARL = ARLData,
#' #                   members = c("BOSTON", "TORONTO", "OTTAWA", "LAVAL", "HARVARD"),
#' #                   years = c(2015, 2016, 2017, 2022, 2018, 2019))
#'
#' @author {Anjali Silva, \email{a.silva@utoronto.ca}, Klara Maidenberg, \email{klara.maidenberg@utoronto.ca}}
#'
#' @references
#' Mian, A., & Gross, H. (2023). ARL Statistics 2022. Washington, DC:
#' Association of Research Libraries.
#' \href{https://publications.arl.org/ARL-Statistics-2022/}{Link}
#'
#' Association of Research Libraries. (2023). ARL Statistics 2023
#' Instructions. \href{https://www.arlstatistics.org/resources/stats_instructions}{Link}
#'
#' @export
#' @import ggplot2
#' @import magrittr
#' @import dplyr
#' @importFrom MASS fractions
visTotalLibraryExp <- function(dataARL, members = NA, years = NA) {

  selectedData <- dataAdjustment(dataARL = dataARL)

  yearsToDisplay <- setYearsToDispaly(years = years,
                                      dataARL = dataARL)

  membersToDisplay <- setMemebersToDispaly(members = members,
                                           dataARL = dataARL)

  # ---
  # Using total lib stats per faculty by top contributors over 5 years ####
  tleTopPerFaculty <- selectedData %>%
   dplyr::filter(`Year` %in% yearsToDisplay) %>%
    # Remove median value as it is not a true entry
    dplyr::filter(! `Institution Name` %in% "MEDIAN") %>%
    # filter denominator with zero value to avoid Inf results
    dplyr::filter(`Total teaching faculty` != 0) %>%
    # Add an error message if no data is selected based on criteria
    { if (nrow(.) == 0) stop("No data available for selected years.") else . } %>%
    dplyr::mutate(expPerFaculty = `Total library expenditures`/`Total teaching faculty`) %>%
    # Replace INF values with NA
    dplyr::mutate(expPerFaculty = na_if(expPerFaculty, Inf)) %>%
    dplyr::group_by(`Year`) %>%
    dplyr::top_n(5, expPerFaculty) %>%
    dplyr::arrange(`Year`, desc(expPerFaculty)) %>%
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    ggplot2::ggplot(aes(x = factor(`Year`),
                        y = `expPerFaculty`,
                        fill = factor(`Institution Name`),
                        width = .75)) +
    ggplot2::geom_bar(position = "dodge", stat = "identity") +
    ggplot2::labs(y = "Total Library Expenditures\nPer Teaching Faculty",
                  x = "Year",
                  fill = "ARL Member") +
    ggplot2::theme_bw() +
    ggplot2::ggtitle(label = "Ratio of Total Library Expenditures Per Teaching\nFaculty For Top 5 ARL Members Overall") +
                     # subtitle = "ARL rank is shown on top of each bar.") +
    ggplot2::theme(text = element_text(size = 15, color = 'black'),
                   axis.text.x = element_text(angle = 90,
                                              hjust = 1,
                                              vjust = 0.5,
                                              color = 'black', size = 15),
                   axis.text.y = element_text(color = 'black', size = 15)) +
    ggplot2::scale_fill_manual(values = setColorPalette()[-c(1:5)]) +
    ggplot2::scale_y_continuous(labels = scales::dollar_format(),
                                breaks = scales::pretty_breaks(n = 5)) # +
    # Add ranking labels on bars
    # ggplot2::geom_text(aes(label = `Rank in ARL investment index`),
    #                    position = position_dodge(width = 0.9),
    #                    vjust = 0,
    #                    size = 6)


  # Final table - total lib stats per faculty by top contributors over 5 years
  tleTopPerFacultyTable <- selectedData %>%
    dplyr::filter(`Year` %in% yearsToDisplay) %>%
    # Remove median value as it is not a true entry
    dplyr::filter(! `Institution Name` %in% "MEDIAN") %>%
    # filter denominator with zero value to avoid Inf results
    dplyr::filter(`Total teaching faculty` != 0) %>%
    # if the resulting number of rows is zero, end and tell user
    { if (nrow(.) == 0) stop("No data available for selected years.") else . } %>%
    dplyr::mutate(expPerFaculty = MASS::fractions(`Total library expenditures`/`Total teaching faculty`)) %>%
    dplyr::select('Year', 'expPerFaculty', `Institution Name`) %>%
    dplyr::group_by(`Year`) %>%
    dplyr::top_n(5, expPerFaculty) %>%
    # dplyr::arrange(`Year`, desc(expPerFaculty)) %>%
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    dplyr::mutate(expPerFaculty = as.character(expPerFaculty)) %>%  # Convert to character
    tidyr::pivot_wider(names_from = `Year`, values_from = 'expPerFaculty') %>%
    kableExtra::kbl() %>%
    kableExtra::kable_paper(lightable_options = "striped")

  # ---
  # Using total lib stats per student by top contributors
  tleTopPerStudent <- selectedData %>%
    dplyr::filter(`Year` %in% yearsToDisplay) %>%
    # Remove median value as it is not a true entry
    dplyr::filter(! `Institution Name` %in% "MEDIAN") %>%
    dplyr::mutate(allStudents = `Total fulltime students` + `Part-time students, undergraduate and graduate`) %>%
    # filter denominator with zero value to avoid Inf results
    dplyr::filter(allStudents != 0) %>%
    # Add an error message if no data is selected based on criteria
    { if (nrow(.) == 0) stop("No data available for selected years.") else . } %>%
    dplyr::mutate(expPerStudent = `Total library expenditures`/ allStudents) %>%
    # Replace INF values with NA
    dplyr::mutate(expPerStudent = na_if(expPerStudent, Inf)) %>%
    dplyr::group_by(`Year`) %>%
    dplyr::top_n(5, expPerStudent) %>%
    dplyr::arrange(`Year`, desc(expPerStudent)) %>%
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    ggplot2::ggplot(aes(x = factor(`Year`),
                        y = `expPerStudent`,
                        fill = factor(`Institution Name`),
                        width = .75)) +
    ggplot2::geom_bar(position = "dodge", stat = "identity") +
    ggplot2::labs(y = "Total Library Expenditures\nPer Student",
                  x = "Year",
                  fill = "ARL Member") +
    ggplot2::theme_bw() +
    ggplot2::ggtitle(label = "Ratio of Total Library Expenditures Per Student\n(FT + PT) For Top 5 ARL Members Overall") +
                    # subtitle = "ARL rank is shown on top of each bar.") +
    ggplot2::theme(text = element_text(size = 15, color = 'black'),
                   axis.text.x = element_text(angle = 90,
                                              hjust = 1,
                                              vjust = 0.5,
                                              color = 'black', size = 15),
                   axis.text.y = element_text(color = 'black', size = 15)) +
    ggplot2::scale_fill_manual(values = setColorPalette()[-c(1:5)]) +
    ggplot2::scale_y_continuous(labels = scales::dollar_format(),
                                breaks = scales::pretty_breaks(n = 5)) # +
    # Add ranking labels on bars
    # ggplot2::geom_text(aes(label = `Rank in ARL investment index`),
    #                    position = position_dodge(width = 0.9),
    #                    vjust = 0,
    #                    size = 6)



  # Final table - total lib stats per student by top contributors
  tleTopPerStudentTable <- selectedData %>%
    dplyr::filter(`Year` %in% yearsToDisplay) %>%
    # Remove median value as it is not a true entry
    dplyr::filter(! `Institution Name` %in% "MEDIAN") %>%
    dplyr::mutate(allStudents = `Total fulltime students` + `Part-time students, undergraduate and graduate`) %>%
    # filter denominator with zero value to avoid Inf results
    dplyr::filter(allStudents != 0) %>%
    # if the resulting number of rows is zero, end and tell user
    { if (nrow(.) == 0) stop("No data available for selected years.") else . } %>%
    dplyr::mutate(expPerStudent = MASS::fractions(`Total library expenditures`/ allStudents)) %>%
    dplyr::select('Year', 'expPerStudent', `Institution Name`) %>%
    dplyr::group_by(`Year`) %>%
    dplyr::top_n(5, expPerStudent) %>%
    # dplyr::arrange(`Year`, desc(expPerStudent)) %>%
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    dplyr::mutate(expPerStudent = as.character(expPerStudent)) %>%  # Convert to character
    tidyr::pivot_wider(names_from = `Year`, values_from = 'expPerStudent') %>%
    kableExtra::kbl() %>%
    kableExtra::kable_paper(lightable_options = "striped")


  # ---
  # Using total lib stats per graduate student by top contributors
  tleTopPerGradStudent <- selectedData %>%
    dplyr::filter(`Year` %in% yearsToDisplay) %>%
    # Remove median value as it is not a true entry
    dplyr::filter(! `Institution Name` %in% "MEDIAN") %>%
    dplyr::mutate(allGradStudents = `Part-time graduate students` + `Total fulltime graduate students`) %>%
    # filter denominator with zero value to avoid Inf results
    dplyr::filter(allGradStudents != 0) %>%
    # if the resulting number of rows is zero, end and tell user
    { if (nrow(.) == 0) stop("No data available for selected years.") else . } %>%
    dplyr::mutate(expPerGradStudent = `Total library expenditures`/ allGradStudents) %>%
    # Replace INF values with NA
    dplyr::mutate(expPerGradStudent = na_if(expPerGradStudent, Inf)) %>%
    dplyr::group_by(`Year`) %>%
    dplyr::top_n(5, expPerGradStudent) %>%
    dplyr::arrange(`Year`, desc(expPerGradStudent)) %>%
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    ggplot2::ggplot(aes(x = factor(`Year`),
                        y = `expPerGradStudent`,
                        fill = factor(`Institution Name`),
                        width = .75)) +
    ggplot2::geom_bar(position = "dodge", stat = "identity") +
    ggplot2::labs(y = "Total Library Expenditures\nPer Grad Student",
                  x = "Year",
                  fill = "ARL Member") +
    ggplot2::theme_bw() +
    ggplot2::ggtitle(label = "Ratio of Total Library Expenditures Per Grad Student\n(FT + PT) For Top 5 ARL Members Overall") +
                    # subtitle = "ARL rank is shown on top of each bar.") +
    ggplot2::theme(text = element_text(size = 15, color = 'black'),
                   axis.text.x = element_text(angle = 90,
                                              hjust = 1,
                                              vjust = 0.5,
                                              color = 'black', size = 15),
                   axis.text.y = element_text(color = 'black', size = 15)) +
    ggplot2::scale_fill_manual(values = setColorPalette()[-c(1:5)]) +
    ggplot2::scale_y_continuous(labels = scales::dollar_format(),
                                breaks = scales::pretty_breaks(n = 5)) # +
    # Add ranking labels on bars
    # ggplot2::geom_text(aes(label = `Rank in ARL investment index`),
    #                    position = position_dodge(width = 0.9),
    #                    vjust = 0,
    #                    size = 6)


  # Final table - total lib stats per graduate student by top contributors
  tleTopPerGradStudentTable <- selectedData %>%
    dplyr::filter(`Year` %in% yearsToDisplay) %>%
    # Remove median value as it is not a true entry
    dplyr::filter(! `Institution Name` %in% "MEDIAN") %>%
    dplyr::mutate(allGradStudents = `Part-time graduate students` + `Total fulltime graduate students`) %>%
    # filter denominator with zero value to avoid Inf results
    dplyr::filter(allGradStudents != 0) %>%
    # if the resulting number of rows is zero, end and tell user
    { if (nrow(.) == 0) stop("No data available for selected years.") else . } %>%
    dplyr::mutate(expPerGradStudent = MASS::fractions(`Total library expenditures`/ allGradStudents)) %>%
    dplyr::select('Year', 'expPerGradStudent', `Institution Name`) %>%
    dplyr::group_by(`Year`) %>%
    dplyr::top_n(5, expPerGradStudent) %>%
    # dplyr::arrange(`Year`, desc(expPerGradStudent)) %>%
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    dplyr::mutate(expPerGradStudent = as.character(expPerGradStudent)) %>%  # Convert to character
    tidyr::pivot_wider(names_from = `Year`, values_from = 'expPerGradStudent') %>%
    kableExtra::kbl() %>%
    kableExtra::kable_paper(lightable_options = "striped")


  # ---
  # Using total lib stats per undergraduate student by top contributors
  tleTopPerUndergradStudent <- selectedData %>%
    dplyr::filter(`Year` %in% yearsToDisplay) %>%
    # Remove median value as it is not a true entry
    dplyr::filter(! `Institution Name` %in% "MEDIAN") %>%
    dplyr::mutate(totalUndergradStudents = ((`Total fulltime students` + `Part-time students, undergraduate and graduate`) -
                                              (`Part-time graduate students` + `Total fulltime graduate students`))) %>%
    # filter denominator with zero value to avoid Inf results
    dplyr::filter(totalUndergradStudents != 0) %>%
    # Add an error message if no data is selected based on criteria
    { if (nrow(.) == 0) stop("No data available for selected years.") else . } %>%
    dplyr::mutate(expPerUndergradStudent = `Total library expenditures`/ totalUndergradStudents) %>%
    # Replace INF values with NA
    dplyr::mutate(expPerUndergradStudent = na_if(expPerUndergradStudent, Inf)) %>%
    dplyr::group_by(`Year`) %>%
    dplyr::top_n(5, expPerUndergradStudent) %>%
    dplyr::arrange(`Year`, desc(expPerUndergradStudent)) %>%
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    ggplot2::ggplot(aes(x = factor(`Year`),
                        y = `expPerUndergradStudent`,
                        fill = factor(`Institution Name`),
                        width = .75)) +
    ggplot2::geom_bar(position = "dodge", stat = "identity") +
    ggplot2::labs(y = "Total Library Expenditures\nPer Undergrad Student",
                  x = "Year",
                  fill = "ARL Member") +
    ggplot2::theme_bw() +
    ggplot2::ggtitle(label = "Ratio of Total Library Expenditures Per Undergrad Student\n(FT + PT) For Top 5 ARL Members Overall") +
    # subtitle = "ARL rank is shown on top of each bar.") +
    ggplot2::theme(text = element_text(size = 15, color = 'black'),
                   axis.text.x = element_text(angle = 90,
                                              hjust = 1,
                                              vjust = 0.5,
                                              color = 'black', size = 15),
                   axis.text.y = element_text(color = 'black', size = 15)) +
    ggplot2::scale_fill_manual(values = setColorPalette()[-c(1:5)]) +
    ggplot2::scale_y_continuous(labels = scales::dollar_format(),
                                breaks = scales::pretty_breaks(n = 5)) # +
  # Add ranking labels on bars
  # ggplot2::geom_text(aes(label = `Rank in ARL investment index`),
  #                    position = position_dodge(width = 0.9),
  #                    vjust = 0,
  #                    size = 6)


  # Final table - total lib stats per undergraduate student by top contributors
  tleTopPerUndergradStudentTable <- selectedData %>%
    dplyr::filter(`Year` %in% yearsToDisplay) %>%
    # Remove median value as it is not a true entry
    dplyr::filter(! `Institution Name` %in% "MEDIAN") %>%
    dplyr::mutate(totalUndergradStudents = ((`Total fulltime students` + `Part-time students, undergraduate and graduate`) -
                                              (`Part-time graduate students` + `Total fulltime graduate students`))) %>%
    # filter denominator with zero value to avoid Inf results
    dplyr::filter(totalUndergradStudents != 0) %>%
    # if the resulting number of rows is zero, end and tell user
    { if (nrow(.) == 0) stop("No data available for selected years.") else . } %>%
    dplyr::mutate(expPerUndergradStudent = MASS::fractions(`Total library expenditures`/ totalUndergradStudents)) %>%
    dplyr::select('Year', 'expPerUndergradStudent', `Institution Name`) %>%
    dplyr::group_by(`Year`) %>%
    dplyr::top_n(5, expPerUndergradStudent) %>%
    # dplyr::arrange(`Year`, desc(expPerUndergradStudent)) %>%
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    dplyr::mutate(expPerUndergradStudent = as.character(expPerUndergradStudent)) %>%  # Convert to character
    tidyr::pivot_wider(names_from = `Year`, values_from = 'expPerUndergradStudent') %>%
    kableExtra::kbl() %>%
    kableExtra::kable_paper(lightable_options = "striped")



  # ---
  # Using total lib stats per doctoral degree by top contributors
  tleTopPerDoctoral <- selectedData %>%
    dplyr::filter(`Year` %in% yearsToDisplay) %>%
    # Remove median value as it is not a true entry
    dplyr::filter(! `Institution Name` %in% "MEDIAN") %>%
    # filter denominator with zero value to avoid Inf results
    dplyr::filter(`Doctor's degrees awarded` != 0) %>%
    # Add an error message if no data is selected based on criteria
    { if (nrow(.) == 0) stop("No data available for selected years.") else . } %>%
    dplyr::mutate(expPerDoctoral = `Total library expenditures`/ `Doctor's degrees awarded`) %>%
    # Replace INF values with NA
    dplyr::mutate(expPerDoctoral = na_if(expPerDoctoral, Inf)) %>%
    dplyr::group_by(`Year`) %>%
    dplyr::top_n(5, expPerDoctoral) %>%
    dplyr::arrange(`Year`, desc(expPerDoctoral)) %>%
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    dplyr::filter(! `Institution Name` %in% "MEDIAN") %>%
    ggplot2::ggplot(aes(x = factor(`Year`),
                        y = `expPerDoctoral`,
                        fill = factor(`Institution Name`),
                        width = .75)) +
    ggplot2::geom_bar(position = "dodge", stat = "identity") +
    ggplot2::labs(y = "Total Library Expenditures\nPer Doctoral Degree",
                  x = "Year",
                  fill = "ARL Member") +
    ggplot2::theme_bw() +
    ggplot2::ggtitle(label = "Ratio of Total Library Expenditures Per Doctoral\nDegree For Top 5 ARL Members Overall") +
                    # subtitle = "ARL rank is shown on top of each bar.") +
    ggplot2::theme(text = element_text(size = 15, color = 'black'),
                   axis.text.x = element_text(angle = 90,
                                              hjust = 1,
                                              vjust = 0.5,
                                              color = 'black', size = 15),
                   axis.text.y = element_text(color = 'black', size = 15)) +
    ggplot2::scale_fill_manual(values = setColorPalette()[-c(1:5)]) +
    ggplot2::scale_y_continuous(labels = scales::dollar_format(),
                                breaks = scales::pretty_breaks(n = 5)) # +
    # Add ranking labels on bars
    # ggplot2::geom_text(aes(label = `Rank in ARL investment index`),
    #                    position = position_dodge(width = 0.9),
    #                    vjust = 0,
    #                    size = 6)


  # Final table - total lib stats per doctoral degree by top contributors
  tleTopPerDoctoralTable <- selectedData %>%
    dplyr::filter(`Year` %in% yearsToDisplay) %>%
    # Remove median value as it is not a true entry
    dplyr::filter(! `Institution Name` %in% "MEDIAN") %>%
    # filter denominator with zero value to avoid Inf results
    dplyr::filter(`Doctor's degrees awarded` != 0) %>%
    # if the resulting number of rows is zero, end and tell user
    { if (nrow(.) == 0) stop("No data available for selected years.") else . } %>%
    dplyr::mutate(expPerDoctoral = MASS::fractions(`Total library expenditures`/ `Doctor's degrees awarded`)) %>%
    dplyr::select('Year', 'expPerDoctoral', `Institution Name`) %>%
    dplyr::group_by(`Year`) %>%
    dplyr::top_n(5, expPerDoctoral) %>%
    # dplyr::arrange(`Year`, desc(expPerDoctoral)) %>%
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    dplyr::mutate(expPerDoctoral = as.character(expPerDoctoral)) %>%  # Convert to character
    tidyr::pivot_wider(names_from = `Year`, values_from = 'expPerDoctoral') %>%
    kableExtra::kbl() %>%
    kableExtra::kable_paper(lightable_options = "striped")



  # ---
  # Using total lib stats per faculty by user selection
  tlePerFacultyUserSelected <- selectedData %>%
    dplyr::filter(`Year` %in% yearsToDisplay) %>%
    dplyr::filter(`Institution Name` %in% membersToDisplay) %>%
    # Remove median value as it is not a true entry
    dplyr::filter(! `Institution Name` %in% "MEDIAN") %>%
    # Add an error message if no data is selected based on criteria
    { if (nrow(.) == 0) stop("No data available for selected years.") else . } %>%
    dplyr::mutate(expPerFaculty = `Total library expenditures`/`Total teaching faculty`) %>%
    # Replace INF values with NA
    dplyr::mutate(expPerFaculty = na_if(expPerFaculty, Inf)) %>%
    dplyr::group_by(`Year`) %>%
    dplyr::top_n(5, expPerFaculty) %>%
    dplyr::arrange(`Year`, desc(expPerFaculty)) %>%
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    ggplot2::ggplot(aes(x = factor(`Year`),
                        y = `expPerFaculty`,
                        fill = factor(`Institution Name`),
                        width = .75)) +
    ggplot2::geom_bar(position = "dodge", stat = "identity") +
    ggplot2::labs(y = "Total Library Expenditures\nPer Teaching Faculty",
                  x = "Year",
                  fill = "ARL Member") +
    ggplot2::theme_bw() +
    ggplot2::ggtitle(label = "Ratio of Total Library Expenditures Per Teaching\nFaculty For User Selected ARL Members") +
                    # subtitle = "ARL rank is shown on top of each bar.") +
    ggplot2::theme(text = element_text(size = 15, color = 'black'),
                   axis.text.x = element_text(angle = 90,
                                              hjust = 1,
                                              vjust = 0.5,
                                              color = 'black', size = 15),
                   axis.text.y = element_text(color = 'black', size = 15)) +
    ggplot2::scale_fill_manual(values = setColorPalette()) +
    ggplot2::scale_y_continuous(labels = scales::dollar_format(),
                                breaks = scales::pretty_breaks(n = 5)) # +
    # Add ranking labels on bars
    # ggplot2::geom_text(aes(label = `Rank in ARL investment index`),
    #                    position = position_dodge(width = 0.9),
    #                    vjust = 0,
    #                    size = 6)


  # Final table - total lib stats per faculty by user selection
  tlePerFacultyUserSelectedTable <- selectedData %>%
    dplyr::filter(`Year` %in% yearsToDisplay) %>%
    dplyr::filter(`Institution Name` %in% membersToDisplay) %>%
    # Remove median value as it is not a true entry
    dplyr::filter(! `Institution Name` %in% "MEDIAN") %>%
    # filter denominator with zero value to avoid Inf results
    dplyr::filter(`Total teaching faculty` != 0) %>%
    # if the resulting number of rows is zero, end and tell user
    { if (nrow(.) == 0) stop("No data available for selected years.") else . } %>%
    dplyr::mutate(expPerFaculty = MASS::fractions(`Total library expenditures`/`Total teaching faculty`)) %>%
    dplyr::select('Year', 'expPerFaculty', `Institution Name`) %>%
    dplyr::group_by(`Year`) %>%
    dplyr::top_n(5, expPerFaculty) %>%
    # dplyr::arrange(`Year`, desc(expPerFaculty)) %>%
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    dplyr::mutate(expPerFaculty = as.character(expPerFaculty)) %>%  # Convert to character
    tidyr::pivot_wider(names_from = `Year`, values_from = 'expPerFaculty') %>%
    kableExtra::kbl() %>%
    kableExtra::kable_paper(lightable_options = "striped")



  # ---
  # Using total lib stats per student by user selection
  tlePerStudentUserSelected <- selectedData %>%
    dplyr::filter(`Year` %in% yearsToDisplay) %>%
    dplyr::filter(`Institution Name` %in% membersToDisplay) %>%
    # Remove median value as it is not a true entry
    dplyr::filter(! `Institution Name` %in% "MEDIAN") %>%
    # Add an error message if no data is selected based on criteria
    { if (nrow(.) == 0) stop("No data available for selected years.") else . } %>%
    dplyr::mutate(allStudents = `Total fulltime students` + `Part-time students, undergraduate and graduate`) %>%
    dplyr::mutate(expPerStudent = `Total library expenditures`/ allStudents) %>%
    # Replace INF values with NA
    dplyr::mutate(expPerStudent = na_if(expPerStudent, Inf)) %>%
    dplyr::group_by(`Year`) %>%
    dplyr::top_n(5, expPerStudent) %>%
    dplyr::arrange(`Year`, desc(expPerStudent)) %>%
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    ggplot2::ggplot(aes(x = factor(`Year`),
                        y = `expPerStudent`,
                        fill = factor(`Institution Name`),
                        width = .75)) +
    ggplot2::geom_bar(position = "dodge", stat = "identity") +
    ggplot2::labs(y = "Total Library Expenditures\nPer Student",
                  x = "Year",
                  fill = "ARL Member") +
    ggplot2::theme_bw() +
    ggplot2::ggtitle(label = "Ratio of Total Library Expenditures Per Student\n(FT + PT) For User Selected ARL Members") +
                    # subtitle = "ARL rank is shown on top of each bar.") +
    ggplot2::theme(text = element_text(size = 15, color = 'black'),
                   axis.text.x = element_text(angle = 90,
                                              hjust = 1,
                                              vjust = 0.5,
                                              color = 'black', size = 15),
                   axis.text.y = element_text(color = 'black', size = 15)) +
    ggplot2::scale_fill_manual(values = setColorPalette()) +
    ggplot2::scale_y_continuous(labels = scales::dollar_format(),
                                breaks = scales::pretty_breaks(n = 5)) # +
    # Add ranking labels on bars
    # ggplot2::geom_text(aes(label = `Rank in ARL investment index`),
    #                    position = position_dodge(width = 0.9),
    #                    vjust = 0,
    #                    size = 6)


  # Final table - total lib stats per student by user selection
  tlePerStudentUserSelectedTable <- selectedData %>%
    dplyr::filter(`Year` %in% yearsToDisplay) %>%
    dplyr::filter(`Institution Name` %in% membersToDisplay) %>%
    # Remove median value as it is not a true entry
    dplyr::filter(! `Institution Name` %in% "MEDIAN") %>%
    dplyr::mutate(allStudents = `Total fulltime students` + `Part-time students, undergraduate and graduate`) %>%
    # filter denominator with zero value to avoid Inf results
    dplyr::filter(`allStudents` != 0) %>%
     # if the resulting number of rows is zero, end and tell user
    { if (nrow(.) == 0) stop("No data available for selected years.") else . } %>%
    dplyr::mutate(expPerStudent = MASS::fractions(`Total library expenditures`/ allStudents)) %>%
    dplyr::select('Year', 'expPerStudent', `Institution Name`) %>%
    dplyr::group_by(`Year`) %>%
    dplyr::top_n(5, expPerStudent) %>%
    # dplyr::arrange(`Year`, desc(expPerStudent)) %>%
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    dplyr::mutate(expPerStudent = as.character(expPerStudent)) %>%  # Convert to character
    tidyr::pivot_wider(names_from = `Year`, values_from = 'expPerStudent') %>%
    kableExtra::kbl() %>%
    kableExtra::kable_paper(lightable_options = "striped")


  # ---
  # Using total lib stats per graduate student by user selection
  tlePerGradStudentUserSelected <- selectedData %>%
    dplyr::filter(`Year` %in% yearsToDisplay) %>%
    dplyr::filter(`Institution Name` %in% membersToDisplay) %>%
    # Remove median value as it is not a true entry
    dplyr::filter(! `Institution Name` %in% "MEDIAN") %>%
    # Add an error message if no data is selected based on criteria
    { if (nrow(.) == 0) stop("No data available for selected years.") else . } %>%
    dplyr::mutate(allgradStudents = `Part-time graduate students` + `Total fulltime graduate students`) %>%
    dplyr::mutate(expPerGradStudent = `Total library expenditures`/ allgradStudents) %>%
    # Replace INF values with NA
    dplyr::mutate(expPerGradStudent = na_if(expPerGradStudent, Inf)) %>%
    dplyr::group_by(`Year`) %>%
    dplyr::top_n(5, expPerGradStudent) %>%
    dplyr::arrange(`Year`, desc(expPerGradStudent)) %>%
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    ggplot2::ggplot(aes(x = factor(`Year`),
                        y = `expPerGradStudent`,
                        fill = factor(`Institution Name`),
                        width = .75)) +
    ggplot2::geom_bar(position = "dodge", stat = "identity") +
    ggplot2::labs(y = "Total Library Expenditures\nPer Grad Student",
                  x = "Year",
                  fill = "ARL Member") +
    ggplot2::theme_bw() +
    ggplot2::ggtitle(label = "Ratio of Total Library Expenditures Per Grad Student\n(FT + PT) For User Selected ARL Members") +
                    # subtitle = "ARL rank is shown on top of each bar.") +
    ggplot2::theme(text = element_text(size = 15, color = 'black'),
                   axis.text.x = element_text(angle = 90,
                                              hjust = 1,
                                              vjust = 0.5,
                                              color = 'black', size = 15),
                   axis.text.y = element_text(color = 'black', size = 15)) +
    ggplot2::scale_fill_manual(values = setColorPalette()) +
    ggplot2::scale_y_continuous(labels = scales::dollar_format(),
                                breaks = scales::pretty_breaks(n = 5)) # +
    # Add ranking labels on bars
    # ggplot2::geom_text(aes(label = `Rank in ARL investment index`),
    #                    position = position_dodge(width = 0.9),
    #                    vjust = 0,
    #                    size = 6)


  # Final table - total lib stats per graduate student by user selection
  tlePerGradStudentUserSelectedTable <- selectedData %>%
    dplyr::filter(`Year` %in% yearsToDisplay) %>%
    dplyr::filter(`Institution Name` %in% membersToDisplay) %>%
    # Remove median value as it is not a true entry
    dplyr::filter(! `Institution Name` %in% "MEDIAN") %>%
    dplyr::mutate(allgradStudents = `Part-time graduate students` + `Total fulltime graduate students`) %>%
    # filter denominator with zero value to avoid Inf results
    dplyr::filter(`allgradStudents` != 0) %>%
    # if the resulting number of rows is zero, end and tell user
    { if (nrow(.) == 0) stop("No data available for selected years.") else . } %>%
    dplyr::mutate(expPerGradStudent = MASS::fractions(`Total library expenditures`/ allgradStudents)) %>%
    dplyr::select('Year', 'expPerGradStudent', `Institution Name`) %>%
    dplyr::group_by(`Year`) %>%
    dplyr::top_n(5, expPerGradStudent) %>%
    # dplyr::arrange(`Year`, desc(expPerGradStudent)) %>%
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    dplyr::mutate(expPerGradStudent = as.character(expPerGradStudent)) %>%  # Convert to character
    tidyr::pivot_wider(names_from = `Year`, values_from = 'expPerGradStudent') %>%
    kableExtra::kbl() %>%
    kableExtra::kable_paper(lightable_options = "striped")



  # ---
  # Using total lib stats per undergraduate student by user selection
  tlePerUndergradStudentUserSelected <- selectedData %>%
    dplyr::filter(`Year` %in% yearsToDisplay) %>%
    dplyr::filter(`Institution Name` %in% membersToDisplay) %>%
    # Remove median value as it is not a true entry
    dplyr::filter(! `Institution Name` %in% "MEDIAN") %>%
    # Add an error message if no data is selected based on criteria
    { if (nrow(.) == 0) stop("No data available for selected years.") else . } %>%
    dplyr::mutate(totalUndergradStudents = ((`Total fulltime students` + `Part-time students, undergraduate and graduate`) -
                    (`Part-time graduate students` + `Total fulltime graduate students`))) %>%
    dplyr::mutate(expPerUndergradStudent = `Total library expenditures`/ totalUndergradStudents) %>%
    # Replace INF values with NA
    dplyr::mutate(expPerUndergradStudent = na_if(expPerUndergradStudent, Inf)) %>%
    dplyr::group_by(`Year`) %>%
    dplyr::top_n(5, expPerUndergradStudent) %>%
    dplyr::arrange(`Year`, desc(expPerUndergradStudent)) %>%
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    ggplot2::ggplot(aes(x = factor(`Year`),
                        y = `expPerUndergradStudent`,
                        fill = factor(`Institution Name`),
                        width = .75)) +
    ggplot2::geom_bar(position = "dodge", stat = "identity") +
    ggplot2::labs(y = "Total Library Expenditures\nPer Undergrad Student",
                  x = "Year",
                  fill = "ARL Member") +
    ggplot2::theme_bw() +
    ggplot2::ggtitle(label = "Ratio of Total Library Expenditures Per Undergrad Student\n(FT + PT) For User Selected ARL Members") +
    # subtitle = "ARL rank is shown on top of each bar.") +
    ggplot2::theme(text = element_text(size = 15, color = 'black'),
                   axis.text.x = element_text(angle = 90,
                                              hjust = 1,
                                              vjust = 0.5,
                                              color = 'black', size = 15),
                   axis.text.y = element_text(color = 'black', size = 15)) +
    ggplot2::scale_fill_manual(values = setColorPalette()) +
    ggplot2::scale_y_continuous(labels = scales::dollar_format(),
                                breaks = scales::pretty_breaks(n = 5)) # +
  # Add ranking labels on bars
  # ggplot2::geom_text(aes(label = `Rank in ARL investment index`),
  #                    position = position_dodge(width = 0.9),
  #                    vjust = 0,
  #                    size = 6)


  # Final table - total lib stats per undergraduate student by user selection
  tlePerUndergradStudentUserSelectedTable <- selectedData %>%
    dplyr::filter(`Year` %in% yearsToDisplay) %>%
    dplyr::filter(`Institution Name` %in% membersToDisplay) %>%
    # Remove median value as it is not a true entry
    dplyr::filter(! `Institution Name` %in% "MEDIAN") %>%
    dplyr::mutate(totalUndergradStudents = ((`Total fulltime students` + `Part-time students, undergraduate and graduate`) -
                                              (`Part-time graduate students` + `Total fulltime graduate students`))) %>%
    # filter denominator with zero value to avoid Inf results
    dplyr::filter(`totalUndergradStudents` != 0) %>%
    # if the resulting number of rows is zero, end and tell user
    { if (nrow(.) == 0) stop("No data available for selected years.") else . } %>%
    dplyr::mutate(expPerUndergradStudent = MASS::fractions(`Total library expenditures`/ totalUndergradStudents)) %>%
    dplyr::select('Year', 'expPerUndergradStudent', `Institution Name`) %>%
    dplyr::group_by(`Year`) %>%
    dplyr::top_n(5, expPerUndergradStudent) %>%
    # dplyr::arrange(`Year`, desc(expPerUndergradStudent)) %>%
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    dplyr::mutate(expPerUndergradStudent = as.character(expPerUndergradStudent)) %>%  # Convert to character
    tidyr::pivot_wider(names_from = `Year`, values_from = 'expPerUndergradStudent') %>%
    kableExtra::kbl() %>%
    kableExtra::kable_paper(lightable_options = "striped")



  # ---
  # Using total lib stats per doctoral degree by user selection
  tlePerDoctoralUserSelected <- selectedData %>%
    dplyr::filter(`Year` %in% yearsToDisplay) %>%
    dplyr::filter(`Institution Name` %in% membersToDisplay) %>%
    # Remove median value as it is not a true entry
    dplyr::filter(! `Institution Name` %in% "MEDIAN") %>%
    # Add an error message if no data is selected based on criteria
    { if (nrow(.) == 0) stop("No data available for selected years.") else . } %>%
    dplyr::mutate(expPerDoctoral = `Total library expenditures`/ `Doctor's degrees awarded`) %>%
    # Replace INF values with NA
    dplyr::mutate(expPerDoctoral = na_if(expPerDoctoral, Inf)) %>%
    dplyr::group_by(`Year`) %>%
    dplyr::top_n(5, expPerDoctoral) %>%
    dplyr::arrange(`Year`, desc(expPerDoctoral)) %>%
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    ggplot2::ggplot(aes(x = factor(`Year`),
                        y = `expPerDoctoral`,
                        fill = factor(`Institution Name`),
                        width = .75)) +
    ggplot2::geom_bar(position = "dodge", stat = "identity") +
    ggplot2::labs(y = "Total Library Expenditures\nPer Doctoral Degree",
                  x = "Year",
                  fill = "ARL Member") +
    ggplot2::theme_bw() +
    ggplot2::ggtitle(label = "Ratio of Total Library Expenditures Per Doctoral\nDegree For User Selected ARL Members") +
                    # subtitle = "ARL rank is shown on top of each bar.") +
    ggplot2::theme(text = element_text(size = 15, color = 'black'),
                   axis.text.x = element_text(angle = 90,
                                              hjust = 1,
                                              vjust = 0.5,
                                              color = 'black', size = 15),
                   axis.text.y = element_text(color = 'black', size = 15)) +
    ggplot2::scale_fill_manual(values = setColorPalette()) +
    ggplot2::scale_y_continuous(labels = scales::dollar_format(),
                                breaks = scales::pretty_breaks(n = 5)) # +
    # Add ranking labels on bars
    # ggplot2::geom_text(aes(label = `Rank in ARL investment index`),
    #                    position = position_dodge(width = 0.9),
    #                    vjust = 0,
    #                    size = 6)



  # Final table - total lib stats per doctoral degree by user selection
  tlePerDoctoralUserSelectedTable <- selectedData %>%
    dplyr::filter(`Year` %in% yearsToDisplay) %>%
    dplyr::filter(`Institution Name` %in% membersToDisplay) %>%
    # Remove median value as it is not a true entry
    dplyr::filter(! `Institution Name` %in% "MEDIAN") %>%
    # filter denominator with zero value to avoid Inf results
    dplyr::filter(`Doctor's degrees awarded` != 0) %>%
    # if the resulting number of rows is zero, end and tell user
    { if (nrow(.) == 0) stop("No data available for selected years.") else . } %>%
    dplyr::mutate(expPerDoctoral = MASS::fractions(`Total library expenditures`/ `Doctor's degrees awarded`)) %>%
    dplyr::select('Year', 'expPerDoctoral', `Institution Name`) %>%
    dplyr::group_by(`Year`) %>%
    dplyr::top_n(5, expPerDoctoral) %>%
    # dplyr::arrange(`Year`, desc(expPerDoctoral)) %>%
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    dplyr::mutate(expPerDoctoral = as.character(expPerDoctoral)) %>%  # Convert to character
    tidyr::pivot_wider(names_from = `Year`, values_from = 'expPerDoctoral') %>%
    kableExtra::kbl() %>%
    kableExtra::kable_paper(lightable_options = "striped")


  return(list(tleTopPerFaculty = tleTopPerFaculty, # 1
              tleTopPerStudent = tleTopPerStudent, # 2
              tleTopPerGradStudent = tleTopPerGradStudent, # 3
              tleTopPerUndergradStudent = tleTopPerUndergradStudent, # 4
              tleTopPerDoctoral = tleTopPerDoctoral, # 5
              tlePerFacultyUserSelected = tlePerFacultyUserSelected, # 6
              tlePerStudentUserSelected = tlePerStudentUserSelected, # 7
              tlePerGradStudentUserSelected = tlePerGradStudentUserSelected, # 8
              tlePerUndergradStudentUserSelected = tlePerUndergradStudentUserSelected, # 9
              tlePerDoctoralUserSelected = tlePerDoctoralUserSelected, # 10
              tleTopPerFacultyTable = tleTopPerFacultyTable, # 11
              tleTopPerStudentTable = tleTopPerStudentTable, # 12
              tleTopPerGradStudentTable = tleTopPerGradStudentTable, # 13
              tleTopPerUndergradStudentTable = tleTopPerUndergradStudentTable, # 14
              tleTopPerDoctoralTable = tleTopPerDoctoralTable, # 15
              tlePerFacultyUserSelectedTable = tlePerFacultyUserSelectedTable, # 16
              tlePerStudentUserSelectedTable = tlePerStudentUserSelectedTable, # 17
              tlePerGradStudentUserSelectedTable = tlePerGradStudentUserSelectedTable, # 18
              tlePerUndergradStudentUserSelectedTable = tlePerUndergradStudentUserSelectedTable, # 19
              tlePerDoctoralUserSelectedTable = tlePerDoctoralUserSelectedTable)) # 20
}

# [END]



