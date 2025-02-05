# staffCounts
#' Plots to Compare Support Library Staff Counts Over Years
#'
#' A function to visualize library support staff counts, full-time
#' equivalent (FTE), as ratios in comparison to various statistics reported
#' in the annual survey of Association of Research Libraries (ARL) as bar
#' plots. Note, this function provides question 13b of ARL survey as the
#' numerator of the ratio.
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
#' @return Returns bar plots showing varying ratios specified below:
#' \itemize{
#'   \item supFTETopPerFaculty - A barplot showing ARL members with highest
#'         ratio of total library support staff counts (FTE) per
#'         teaching faculty, over user selected number of years.
#'   \item supFTETopPerStudent - A barplot showing ARL members with highest
#'         ratio of total library support staff counts (FTE) per
#'         student (full-time, FT, and part-time, PT), over user selected
#'         number of years.
#'   \item supFTETopPerGradStudent - A barplot showing ARL members with highest
#'         ratio of total library support staff counts (FTE) per graduate
#'         student (full-time, FT, and part-time, PT), over user selected
#'         number of years.
#'   \item supFTETopUndergradStudent - A barplot showing ARL members with highest
#'         ratio of total library support staff counts (FTE) per undergraduate
#'         student (full-time, FT, and part-time, PT), over user selected
#'         number of years.
#'   \item supFTETopPerDoctoral - A barplot showing ARL members with highest
#'         ratio of total library support staff counts (FTE) per doctoral
#'         degree awarded, over user selected number of years.
#'   \item supFTEPerFacultyUserSelected - A barplot showing ratio of library
#'         support staff counts (FTE) per teaching faculty for user
#'         selected ARL members, over user selected number of years.
#'   \item supPerStudentUserSelected - A barplot showing ratio of library
#'         support staff counts (FTE) per student (full-time, FT, and
#'         part-time, PT) for user selected ARL members, over user selected
#'         number of years.
#'   \item supPerGradStudentUserSelected - A barplot showing ratio of total
#'         library support staff counts (FTE) per graduate student
#'         (full-time, FT, and part-time, PT) for user selected ARL members,
#'         over user selected number of years.
#'   \item supPerUndergradStudentUserSelected - A barplot showing ratio of total
#'         library support staff counts (FTE) per undergraduate student
#'         (full-time, FT, and part-time, PT) for user selected ARL members,
#'         over user selected number of years.
#'   \item supPerDoctoralUserSelected - A barplot showing ratio of total
#'         library expenditures per per doctoral degree awarded for user
#'         selected ARL members, over user selected number of years.
#'   \item supFTETopPerFacultyTable - A table showing the original values used
#'         for calculating the ratios for ARL members with highest ratio of
#'         total library professional staff counts (FTE) per teaching faculty, over
#'         user selected number of years.
#'   \item supFTETopPerStudentTable - A table showing the original values used
#'         for calculating the ratios for ARL members with highest ratio of total
#'         total library professional staff counts (FTE) per student (full-time, FT,
#'         and part-time, PT), over user selected number of years.
#'   \item supFTETopPerGradStudentTable - A table showing the original values
#'         used for calculating the ratios for ARL members with highest ratio
#'         of total library professional staff counts (FTE) per graduate student
#'         (full-time, FT, and part-time, PT), over user selected number of years.
#'   \item supFTETopPerUndergradStudentTable - A table showing the original values
#'         used for calculating the ratios for ARL members with highest ratio of
#'         total library professional staff counts (FTE) per undergraduate student
#'         (full-time, FT, and part-time, PT), over user selected number of years.
#'   \item supFTETopPerDoctoralTable - A table showing the original values used
#'         for calculating the ratios for ARL members with highest ratio of total
#'         library professional staff counts (FTE) per doctoral degree awarded, over
#'         user selected number of years.
#'   \item supFTEPerFacultyUserSelectedTable - A table showing the original values
#'         used for calculating the ratios for total library professional staff
#'         counts (FTE) per teaching faculty for user selected ARL members, over user
#'         selected number of years.
#'   \item supFTEPerStudentUserSelectedTable - A table showing the original values
#'         used for calculating the ratios for total library professional staff counts
#'         (FTE) per graduate student (full-time, FT, and part-time, PT) for user
#'         selected ARL members, over user selected number of years.
#'   \item supFTEPerGradStudentUserSelectedTable - A table showing the original values
#'         used for calculating the ratios for total library professional staff
#'         salaries per graduate student (full-time, FT, and part-time, PT) for
#'         user selected ARL members, over user selected number of years.
#'   \item supFTEPerUndergradStudentUserSelectedTable - A table showing the
#'         original values used for calculating the ratios for total library
#'         professional staff counts (FTE)per undergraduate student (full-time,
#'         FT, and part-time, PT) for user selected ARL members, over user selected
#'         number of years.
#'   \item supFTEPerDoctoralUserSelectedTable - A table showing the original values
#'         used for calculating the ratios for total library professional staff
#'         counts (FTE) per per doctoral degree awarded for user selected ARL members,
#'         over user selected number of years.

#' }
#'
#' @examples
#' # Reading R package example data
#' # ?ARLDataDownload
#' visSupStaffCounts(dataARL = ARLDataDownload,
#'                    members = c("Library A", "Library B", "Library C", "Library D", "Library E"),
#'                    years = c(2020, 2021, 2022))
#'
#' # Reading actual data downloaded from ARL (not run)
#' # Set file path
#' # ARLData <- readr::read_csv("~/ARLData.csv")
#' # visSupStaffCounts(dataARL = ARLData,
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
visSupStaffCounts <- function(dataARL, members, years = NA) {

  selectedData <- dataAdjustment(dataARL = dataARL)

  yearsToDisplay <- setYearsToDispaly(years = years,
                                      dataARL = dataARL)

  membersToDisplay <- setMemebersToDispaly(members = members,
                                           dataARL = dataARL)


  # Using support staff count per faculty by top contributors
  supFTETopPerFaculty <- selectedData %>%
    dplyr::filter(`Year` %in% yearsToDisplay) %>%
    # Remove median value as it is not a true entry
    dplyr::filter(! `Institution Name` %in% "MEDIAN") %>%
    # filter denominator with zero value to avoid Inf results
    dplyr::filter(`Total teaching faculty` != 0) %>%
    # Add an error message if no data is selected based on criteria
    { if (nrow(.) == 0) stop("No data available for selected years.") else . } %>%
    dplyr::mutate(supPerFaculty = `Support staff`/`Total teaching faculty`) %>%
    # Replace INF values with NA
    dplyr::mutate(supPerFaculty = na_if(supPerFaculty, Inf)) %>%
    dplyr::group_by(`Year`) %>%
    dplyr::top_n(5, supPerFaculty) %>%
    dplyr::arrange(`Year`, desc(supPerFaculty)) %>%
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    ggplot2::ggplot(aes(x = factor(`Year`),
                        y = `supPerFaculty`,
                        fill = factor(`Institution Name`),
                        width = .75)) +
    ggplot2::geom_bar(position = "dodge", stat = "identity") +
    ggplot2::labs(y = "Support Staff Count (FTE) \nPer Teaching Faculty",
                  x = "Year",
                  fill = "ARL Member") +
    ggplot2::theme_bw() +
    ggplot2::ggtitle(label = "Ratio of Support Staff Count (FTE) Per Teaching\nFaculty For Top 5 ARL Members Overall") +
    # subtitle = "ARL rank is shown on top of each bar.") +
    ggplot2::theme(text = element_text(size = 15, color = 'black'),
                   axis.text.x = element_text(angle = 90,
                                              hjust = 1,
                                              vjust = 0.5,
                                              color = 'black', size = 15),
                   axis.text.y = element_text(color = 'black', size = 15)) +
    ggplot2::scale_fill_manual(values = setColorPalette()[-c(1:5)]) +
    ggplot2::scale_y_continuous(labels = scales::label_comma(),
                                breaks = scales::pretty_breaks(n = 5)) # +
    # Add ranking labels on bars
    # ggplot2::geom_text(aes(label = `Rank in ARL investment index`),
    #                    position = position_dodge(width = 0.9),
    #                    vjust = 0,
    #                    size = 6)


  # Final table - support staff count per faculty by top contributors over 5 years
  supFTETopPerFacultyTable <- selectedData %>%
    dplyr::filter(`Year` %in% yearsToDisplay) %>%
    # Remove median value as it is not a true entry
    dplyr::filter(! `Institution Name` %in% "MEDIAN") %>%
    # filter denominator with zero value to avoid Inf results
    dplyr::filter(`Total teaching faculty` != 0) %>%
    # if the resulting number of rows is zero, end and tell user
    { if (nrow(.) == 0) stop("No data available for selected years.") else . } %>%
    dplyr::mutate(proPerFaculty = MASS::fractions(`Support staff`/`Total teaching faculty`)) %>%
    dplyr::select('Year', 'proPerFaculty', `Institution Name`) %>%
    dplyr::group_by(`Year`) %>%
    dplyr::top_n(5, proPerFaculty) %>%
    dplyr::arrange(`Year`, desc(proPerFaculty)) %>%
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    dplyr::mutate(proPerFaculty = as.character(proPerFaculty)) %>%  # Convert to character
    tidyr::pivot_wider(names_from = `Year`, values_from = 'proPerFaculty') %>%
    kableExtra::kbl() %>%
    kableExtra::kable_paper(lightable_options = "striped")




  # ---
  # Using support staff count per student by top contributors
  supFTETopPerStudent <- selectedData %>%
    dplyr::filter(`Year` %in% yearsToDisplay) %>%
    # Remove median value as it is not a true entry
    dplyr::filter(! `Institution Name` %in% "MEDIAN") %>%
    dplyr::mutate(allStudents = `Total fulltime students` + `Part-time students, undergraduate and graduate`) %>%
    # filter denominator with zero value to avoid Inf results
    dplyr::filter(allStudents != 0) %>%
    # if the resulting number of rows is zero, end and tell user
    { if (nrow(.) == 0) stop("No data available for selected years.") else . } %>%
    dplyr::mutate(supPerStudent = `Support staff`/ allStudents) %>%
    # Replace INF values with NA
    dplyr::mutate(supPerStudent = na_if(supPerStudent, Inf)) %>%
    dplyr::group_by(`Year`) %>%
    dplyr::top_n(5, supPerStudent) %>%
    dplyr::arrange(`Year`, desc(supPerStudent)) %>%
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    ggplot2::ggplot(aes(x = factor(`Year`),
                        y = `supPerStudent`,
                        fill = factor(`Institution Name`),
                        width = .75)) +
    ggplot2::geom_bar(position = "dodge", stat = "identity") +
    ggplot2::labs(y = "Support Staff Count (FTE) \nPer Student",
                  x = "Year",
                  fill = "ARL Member") +
    ggplot2::theme_bw() +
    ggplot2::ggtitle(label = "Ratio of Support Staff Count (FTE) Per Student\n(FT + PT)  For Top 5 ARL Members Overall") +
    # subtitle = "ARL rank is shown on top of each bar.") +
    ggplot2::theme(text = element_text(size = 15, color = 'black'),
                   axis.text.x = element_text(angle = 90,
                                              hjust = 1,
                                              vjust = 0.5,
                                              color = 'black', size = 15),
                   axis.text.y = element_text(color = 'black', size = 15)) +
    ggplot2::scale_fill_manual(values = setColorPalette()[-c(1:5)]) +
    ggplot2::scale_y_continuous(labels = scales::label_comma(),
                                breaks = scales::pretty_breaks(n = 5)) # +
    # Add ranking labels on bars
    # ggplot2::geom_text(aes(label = `Rank in ARL investment index`),
    #                    position = position_dodge(width = 0.9),
    #                    vjust = 0,
    #                    size = 6)


  # Final table -  support staff count per student by top contributors
  supFTETopPerStudentTable <- selectedData %>%
    dplyr::filter(`Year` %in% yearsToDisplay) %>%
    # Remove median value as it is not a true entry
    dplyr::filter(! `Institution Name` %in% "MEDIAN") %>%
    dplyr::mutate(allStudents = `Total fulltime students` + `Part-time students, undergraduate and graduate`) %>%
    # filter denominator with zero value to avoid Inf results
    dplyr::filter(allStudents != 0) %>%
    # if the resulting number of rows is zero, end and tell user
    { if (nrow(.) == 0) stop("No data available for selected years.") else . } %>%
    dplyr::mutate(proPerStudent = MASS::fractions(`Support staff`/ allStudents)) %>%
    dplyr::select('Year', 'proPerStudent', `Institution Name`) %>%
    dplyr::group_by(`Year`) %>%
    dplyr::top_n(5, proPerStudent) %>%
    dplyr::arrange(`Year`, desc(proPerStudent)) %>%
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    dplyr::mutate(proPerStudent = as.character(proPerStudent)) %>%  # Convert to character
    tidyr::pivot_wider(names_from = `Year`, values_from = 'proPerStudent') %>%
    kableExtra::kbl() %>%
    kableExtra::kable_paper(lightable_options = "striped")



  # ---
  # Using support staff count per graduate student by top contributors
  supFTETopPerGradStudent <- selectedData %>%
    dplyr::filter(`Year` %in% yearsToDisplay) %>%
    # Remove median value as it is not a true entry
    dplyr::filter(! `Institution Name` %in% "MEDIAN") %>%
    dplyr::mutate(allGradStudents = `Part-time graduate students` + `Total fulltime graduate students`) %>%
    # filter denominator with zero value to avoid Inf results
    dplyr::filter(allGradStudents != 0) %>%
    # if the resulting number of rows is zero, end and tell user
    { if (nrow(.) == 0) stop("No data available for selected years.") else . } %>%
    dplyr::mutate(profPerStudent = `Support staff`/ allGradStudents) %>%
    # Replace INF values with NA
    dplyr::mutate(profPerStudent = na_if(profPerStudent, Inf)) %>%
    dplyr::group_by(`Year`) %>%
    dplyr::top_n(5, profPerStudent) %>%
    dplyr::arrange(`Year`, desc(profPerStudent)) %>%
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    ggplot2::ggplot(aes(x = factor(`Year`),
                        y = `profPerStudent`,
                        fill = factor(`Institution Name`),
                        width = .75)) +
    ggplot2::geom_bar(position = "dodge", stat = "identity") +
    ggplot2::labs(y = "Support Staff Count (FTE) \nPer Grad Student",
                  x = "Year",
                  fill = "ARL Member") +
    ggplot2::theme_bw() +
    ggplot2::ggtitle(label = "Ratio of Support Staff Count (FTE) Per Grad Student\n(FT + PT) For Top 5 ARL Members Overall") +
    # subtitle = "ARL rank is shown on top of each bar.") +
    ggplot2::theme(text = element_text(size = 15, color = 'black'),
                   axis.text.x = element_text(angle = 90,
                                              hjust = 1,
                                              vjust = 0.5,
                                              color = 'black', size = 15),
                   axis.text.y = element_text(color = 'black', size = 15)) +
    ggplot2::scale_fill_manual(values = setColorPalette()[-c(1:5)]) +
    ggplot2::scale_y_continuous(labels = scales::label_comma(),
                                breaks = scales::pretty_breaks(n = 5)) # +
    # Add ranking labels on bars
    # ggplot2::geom_text(aes(label = `Rank in ARL investment index`),
    #                    position = position_dodge(width = 0.9),
    #                    vjust = 0,
    #                    size = 6)


  # Final table - staff staff count per graduate student by top contributors
  supFTETopPerGradStudentTable <- selectedData %>%
    dplyr::filter(`Year` %in% yearsToDisplay) %>%
    # Remove median value as it is not a true entry
    dplyr::filter(! `Institution Name` %in% "MEDIAN") %>%
    dplyr::mutate(allGradStudents = `Part-time graduate students` + `Total fulltime graduate students`) %>%
    # filter denominator with zero value to avoid Inf results
    dplyr::filter(allGradStudents != 0) %>%
    # if the resulting number of rows is zero, end and tell user
    { if (nrow(.) == 0) stop("No data available for selected years.") else . } %>%
    dplyr::mutate(profPerGradStudent = MASS::fractions(`Support staff`/ allGradStudents)) %>%
    dplyr::select('Year', 'profPerGradStudent', `Institution Name`) %>%
    dplyr::group_by(`Year`) %>%
    dplyr::top_n(5, profPerGradStudent) %>%
    dplyr::arrange(`Year`, desc(profPerGradStudent)) %>%
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    dplyr::mutate(profPerGradStudent = as.character(profPerGradStudent)) %>%  # Convert to character
    tidyr::pivot_wider(names_from = `Year`, values_from = 'profPerGradStudent') %>%
    kableExtra::kbl() %>%
    kableExtra::kable_paper(lightable_options = "striped")



  # ---
  # Using support staff count per undergraduate student by top contributors (not ARL)
  supFTETopUndergradStudent <- selectedData %>%
    dplyr::filter(`Year` %in% yearsToDisplay) %>%
    # Remove median value as it is not a true entry
    dplyr::filter(! `Institution Name` %in% "MEDIAN") %>%
    dplyr::mutate(totalUndergradStudents = ((`Total fulltime students` + `Part-time students, undergraduate and graduate`) -
                                              (`Part-time graduate students` + `Total fulltime graduate students`))) %>%
    # filter denominator with zero value to avoid Inf results
    dplyr::filter(totalUndergradStudents != 0) %>%
    # if the resulting number of rows is zero, end and tell user
    { if (nrow(.) == 0) stop("No data available for selected years.") else . } %>%
    dplyr::mutate(supPerUndergradStudent = `Support staff`/ totalUndergradStudents) %>%
    # Replace INF values with NA
    dplyr::mutate(supPerUndergradStudent = na_if(supPerUndergradStudent, Inf)) %>%
    dplyr::group_by(`Year`) %>%
    dplyr::top_n(5, supPerUndergradStudent) %>%
    dplyr::arrange(`Year`, desc(supPerUndergradStudent)) %>%
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    ggplot2::ggplot(aes(x = factor(`Year`),
                        y = `supPerUndergradStudent`,
                        fill = factor(`Institution Name`),
                        width = .75)) +
    ggplot2::geom_bar(position = "dodge", stat = "identity") +
    ggplot2::labs(y = "Support Staff Count (FTE)\nPer Undergrad Student",
                  x = "Year",
                  fill = "ARL Member") +
    ggplot2::theme_bw() +
    ggplot2::ggtitle(label = "Ratio of Support Staff Count (FTE) Per Undergrad Student\n(FT + PT) For Top 5 ARL Members Overall") +
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



  # Final table - support staff count per undergraduate student by top contributors
  supFTETopPerUndergradStudentTable <- selectedData %>%
    dplyr::filter(`Year` %in% yearsToDisplay) %>%
    # Remove median value as it is not a true entry
    dplyr::filter(! `Institution Name` %in% "MEDIAN") %>%
    dplyr::mutate(totalUndergradStudents = ((`Total fulltime students` + `Part-time students, undergraduate and graduate`) -
                                              (`Part-time graduate students` + `Total fulltime graduate students`))) %>%
    # filter denominator with zero value to avoid Inf results
    dplyr::filter(totalUndergradStudents != 0) %>%
    # if the resulting number of rows is zero, end and tell user
    { if (nrow(.) == 0) stop("No data available for selected years.") else . } %>%
    dplyr::mutate(proPerUndergradStudent = MASS::fractions(`Support staff`/ totalUndergradStudents)) %>%
    dplyr::select('Year', 'proPerUndergradStudent', `Institution Name`) %>%
    dplyr::group_by(`Year`) %>%
    dplyr::top_n(5, proPerUndergradStudent) %>%
    dplyr::arrange(`Year`, desc(proPerUndergradStudent)) %>%
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    dplyr::mutate(proPerUndergradStudent = as.character(proPerUndergradStudent)) %>%  # Convert to character
    tidyr::pivot_wider(names_from = `Year`, values_from = 'proPerUndergradStudent') %>%
    kableExtra::kbl() %>%
    kableExtra::kable_paper(lightable_options = "striped")




  # ---
  # Using support staff count per doctoral degree by top contributors (not ARL)
  supFTETopPerDoctoral <- selectedData %>%
    dplyr::filter(`Year` %in% yearsToDisplay) %>%
    # Remove median value as it is not a true entry
    dplyr::filter(! `Institution Name` %in% "MEDIAN") %>%
    # filter denominator with zero value to avoid Inf results
    dplyr::filter(`Doctor's degrees awarded` != 0) %>%
    # if the resulting number of rows is zero, end and tell user
    { if (nrow(.) == 0) stop("No data available for selected years.") else . } %>%
    dplyr::mutate(supPerStudent = `Support staff`/ `Doctor's degrees awarded`) %>%
    # Replace INF values with NA
    dplyr::mutate(supPerStudent = na_if(supPerStudent, Inf)) %>%
    dplyr::group_by(`Year`) %>%
    dplyr::top_n(5, supPerStudent) %>%
    dplyr::arrange(`Year`, desc(supPerStudent)) %>%
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    dplyr::filter(! `Institution Name` %in% "MEDIAN") %>%
    ggplot2::ggplot(aes(x = factor(`Year`),
                        y = `supPerStudent`,
                        fill = factor(`Institution Name`),
                        width = .75)) +
    ggplot2::geom_bar(position = "dodge", stat = "identity") +
    ggplot2::labs(y = "Support Staff Count (FTE) \nPer Doctoral Degree",
                  x = "Year",
                  fill = "ARL Member") +
    ggplot2::theme_bw() +
    ggplot2::ggtitle(label = "Ratio of Support Staff Count (FTE) Per Doctoral\nDegree For Top 5 ARL Members Overall") +
    # subtitle = "ARL rank is shown on top of each bar.") +
    ggplot2::theme(text = element_text(size = 15, color = 'black'),
                   axis.text.x = element_text(angle = 90,
                                              hjust = 1,
                                              vjust = 0.5,
                                              color = 'black', size = 15),
                   axis.text.y = element_text(color = 'black', size = 15)) +
    ggplot2::scale_fill_manual(values = setColorPalette()[-c(1:5)]) +
    ggplot2::scale_y_continuous(labels = scales::label_comma(),
                                breaks = scales::pretty_breaks(n = 5)) # +
    # Add ranking labels on bars
    # ggplot2::geom_text(aes(label = `Rank in ARL investment index`),
    #                    position = position_dodge(width = 0.9),
    #                    vjust = 0,
    #                    size = 6)


  # Final table - support staff count per doctoral degree by top contributors
  supFTETopPerDoctoralTable <- selectedData %>%
    dplyr::filter(`Year` %in% yearsToDisplay) %>%
    # Remove median value as it is not a true entry
    dplyr::filter(! `Institution Name` %in% "MEDIAN") %>%
    # filter denominator with zero value to avoid Inf results
    dplyr::filter(`Doctor's degrees awarded` != 0) %>%
    # if the resulting number of rows is zero, end and tell user
    { if (nrow(.) == 0) stop("No data available for selected years.") else . } %>%
    dplyr::mutate(proPerDoctoral = MASS::fractions(`Support staff`/ `Doctor's degrees awarded`)) %>%
    dplyr::select('Year', 'proPerDoctoral', `Institution Name`) %>%
    dplyr::group_by(`Year`) %>%
    dplyr::top_n(5, proPerDoctoral) %>%
    dplyr::arrange(`Year`, desc(proPerDoctoral)) %>%
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    dplyr::mutate(proPerDoctoral = as.character(proPerDoctoral)) %>%  # Convert to character
    tidyr::pivot_wider(names_from = `Year`, values_from = 'proPerDoctoral') %>%
    kableExtra::kbl() %>%
    kableExtra::kable_paper(lightable_options = "striped")



  # ---
  # Using support staff count per faculty by user selection
  supFTEPerFacultyUserSelected <- selectedData %>%
    dplyr::filter(`Year` %in% yearsToDisplay) %>%
    dplyr::filter(`Institution Name` %in% membersToDisplay) %>%
    # Remove median value as it is not a true entry
    dplyr::filter(! `Institution Name` %in% "MEDIAN") %>%
    # filter denominator with zero value to avoid Inf results
    dplyr::filter(`Total teaching faculty` != 0) %>%
    # if the resulting number of rows is zero, end and tell user
    { if (nrow(.) == 0) stop("No data available for selected years.") else . } %>%
    dplyr::mutate(supPerFaculty = `Support staff`/`Total teaching faculty`) %>%
    # Replace INF values with NA
    dplyr::mutate(supPerFaculty = na_if(supPerFaculty, Inf)) %>%
    dplyr::group_by(`Year`) %>%
    dplyr::top_n(5, supPerFaculty) %>%
    dplyr::arrange(`Year`, desc(supPerFaculty)) %>%
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    ggplot2::ggplot(aes(x = factor(`Year`),
                        y = `supPerFaculty`,
                        fill = factor(`Institution Name`),
                        width = .75)) +
    ggplot2::geom_bar(position = "dodge", stat = "identity") +
    ggplot2::labs(y = "Support Staff Count (FTE) Per\nTeaching Faculty",
                  x = "Year",
                  fill = "ARL Member") +
    ggplot2::theme_bw() +
    ggplot2::ggtitle(label = "Ratio of Support Staff Count (FTE) Per\nTeaching Faculty For User Selected Members") +
                    # subtitle = "ARL rank is shown on top of each bar.") +
    ggplot2::theme(text = element_text(size = 15, color = 'black'),
                   axis.text.x = element_text(angle = 90,
                                              hjust = 1,
                                              vjust = 0.5,
                                              color = 'black', size = 15),
                   axis.text.y = element_text(color = 'black', size = 15)) +
    ggplot2::scale_fill_manual(values = setColorPalette()) +
    ggplot2::scale_y_continuous(labels = scales::label_comma(),
                                breaks = scales::pretty_breaks(n = 5)) # +
    # Add ranking labels on bars
    # ggplot2::geom_text(aes(label = `Rank in ARL investment index`),
    #                    position = position_dodge(width = 0.9),
    #                    vjust = 0,
    #                    size = 6)


  # Final table - support staff count per faculty by user selection
  supFTEPerFacultyUserSelectedTable <- selectedData %>%
    dplyr::filter(`Year` %in% yearsToDisplay) %>%
    dplyr::filter(`Institution Name` %in% membersToDisplay) %>%
    # Remove median value as it is not a true entry
    dplyr::filter(! `Institution Name` %in% "MEDIAN") %>%
    # filter denominator with zero value to avoid Inf results
    dplyr::filter(`Total teaching faculty` != 0) %>%
    # if the resulting number of rows is zero, end and tell user
    { if (nrow(.) == 0) stop("No data available for selected years.") else . } %>%
    dplyr::mutate(proPerFaculty = MASS::fractions(`Support staff`/`Total teaching faculty`)) %>%
    dplyr::select('Year', 'proPerFaculty', `Institution Name`) %>%
    dplyr::group_by(`Year`) %>%
    dplyr::top_n(5, proPerFaculty) %>%
    dplyr::arrange(`Year`, desc(proPerFaculty)) %>%
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    dplyr::mutate(proPerFaculty = as.character(proPerFaculty)) %>%  # Convert to character
    tidyr::pivot_wider(names_from = `Year`, values_from = 'proPerFaculty') %>%
    kableExtra::kbl() %>%
    kableExtra::kable_paper(lightable_options = "striped")


  # ---
  # Using support staff stats per student by user selection
  supFTEPerStudentUserSelected <- selectedData %>%
    dplyr::filter(`Year` %in% yearsToDisplay) %>%
    dplyr::filter(`Institution Name` %in% membersToDisplay) %>%
    # Remove median value as it is not a true entry
    dplyr::filter(! `Institution Name` %in% "MEDIAN") %>%
    dplyr::mutate(allStudents = `Total fulltime students` + `Part-time students, undergraduate and graduate`) %>%
    # filter denominator with zero value to avoid Inf results
    dplyr::filter(`allStudents` != 0) %>%
    # if the resulting number of rows is zero, end and tell user
    { if (nrow(.) == 0) stop("No data available for selected years.") else . } %>%
    dplyr::mutate(supPerStudent = `Support staff`/ allStudents) %>%
    # Replace INF values with NA
    dplyr::mutate(supPerStudent = na_if(supPerStudent, Inf)) %>%
    dplyr::group_by(`Year`) %>%
    dplyr::top_n(5, supPerStudent) %>%
    dplyr::arrange(`Year`, desc(supPerStudent))  %>%
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    ggplot2::ggplot(aes(x = factor(`Year`),
                        y = `supPerStudent`,
                        fill = factor(`Institution Name`),
                        width = .75)) +
    ggplot2::geom_bar(position = "dodge", stat = "identity") +
    ggplot2::labs(y = "Support Staff Count (FTE)\nPer Student",
                  x = "Year",
                  fill = "ARL Member") +
    ggplot2::theme_bw() +
    ggplot2::ggtitle(label = "Ratio of Support Staff Count (FTE) Per Student\n(FT + PT) For User Selected Members") +
                    # subtitle = "ARL rank is shown on top of each bar.") +
    ggplot2::theme(text = element_text(size = 15, color = 'black'),
                   axis.text.x = element_text(angle = 90,
                                              hjust = 1,
                                              vjust = 0.5,
                                              color = 'black', size = 15),
                   axis.text.y = element_text(color = 'black', size = 15)) +
    ggplot2::scale_fill_manual(values = setColorPalette()) +
    ggplot2::scale_y_continuous(labels = scales::label_comma(),
                                breaks = scales::pretty_breaks(n = 5)) # +
    # Add ranking labels on bars
    # ggplot2::geom_text(aes(label = `Rank in ARL investment index`),
    #                    position = position_dodge(width = 0.9),
    #                    vjust = 0,
    #                    size = 6)



  # Final table - support staff count per student by user selection
  supFTEPerStudentUserSelectedTable <- selectedData %>%
    dplyr::filter(`Year` %in% yearsToDisplay) %>%
    dplyr::filter(`Institution Name` %in% membersToDisplay) %>%
    # Remove median value as it is not a true entry
    dplyr::filter(! `Institution Name` %in% "MEDIAN") %>%
    dplyr::mutate(allStudents = `Total fulltime students` + `Part-time students, undergraduate and graduate`) %>%
    # filter denominator with zero value to avoid Inf results
    dplyr::filter(`allStudents` != 0) %>%
    # if the resulting number of rows is zero, end and tell user
    { if (nrow(.) == 0) stop("No data available for selected years.") else . } %>%
    dplyr::mutate(proPerStudent = MASS::fractions(`Support staff`/ allStudents)) %>%
    dplyr::select('Year', 'proPerStudent', `Institution Name`) %>%
    dplyr::group_by(`Year`) %>%
    dplyr::top_n(5, proPerStudent) %>%
    dplyr::arrange(`Year`, desc(proPerStudent)) %>%
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    dplyr::mutate(proPerStudent = as.character(proPerStudent)) %>%  # Convert to character
    tidyr::pivot_wider(names_from = `Year`, values_from = 'proPerStudent') %>%
    kableExtra::kbl() %>%
    kableExtra::kable_paper(lightable_options = "striped")



  # ---
  # Using support staff stats per graduate student by user selection
  supFTEGradStudentUserSelected <- selectedData %>%
    dplyr::filter(`Year` %in% yearsToDisplay) %>%
    dplyr::filter(`Institution Name` %in% membersToDisplay) %>%
    # Remove median value as it is not a true entry
    dplyr::filter(! `Institution Name` %in% "MEDIAN") %>%
    dplyr::mutate(allgradStudents = `Part-time graduate students` + `Total fulltime graduate students`) %>%
    # filter denominator with zero value to avoid Inf results
    dplyr::filter(`allgradStudents` != 0) %>%
    # if the resulting number of rows is zero, end and tell user
    { if (nrow(.) == 0) stop("No data available for selected years.") else . } %>%
    dplyr::mutate(supPerGradStudent = `Support staff`/ allgradStudents) %>%
    # Replace INF values with NA
    dplyr::mutate(supPerGradStudent = na_if(supPerGradStudent, Inf)) %>%
    dplyr::group_by(`Year`) %>%
    dplyr::top_n(5, supPerGradStudent) %>%
    dplyr::arrange(`Year`, desc(supPerGradStudent)) %>%
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    ggplot2::ggplot(aes(x = factor(`Year`),
                        y = `supPerGradStudent`,
                        fill = factor(`Institution Name`),
                        width = .75)) +
    ggplot2::geom_bar(position = "dodge", stat = "identity") +
    ggplot2::labs(y = "Support Staff Count (FTE)\nPer Grad Student",
                  x = "Year",
                  fill = "ARL Member") +
    ggplot2::theme_bw() +
    ggplot2::ggtitle(label = "Ratio of Support Staff Count (FTE) Per Grad Student\n(FT + PT) For User Selected Members") +
                    # subtitle = "ARL rank is shown on top of each bar.") +
    ggplot2::theme(text = element_text(size = 15, color = 'black'),
                   axis.text.x = element_text(angle = 90,
                                              hjust = 1,
                                              vjust = 0.5,
                                              color = 'black', size = 15),
                   axis.text.y = element_text(color = 'black', size = 15)) +
    ggplot2::scale_fill_manual(values = setColorPalette()) +
    ggplot2::scale_y_continuous(labels = scales::label_comma(),
                                breaks = scales::pretty_breaks(n = 5)) # +
    # Add ranking labels on bars
    # ggplot2::geom_text(aes(label = `Rank in ARL investment index`),
    #                    position = position_dodge(width = 0.9),
    #                    vjust = 0,
    #                    size = 6)


  # Final table - support staff count per graduate student by user selection
  supFTEPerGradStudentUserSelectedTable <- selectedData %>%
    dplyr::filter(`Year` %in% yearsToDisplay) %>%
    dplyr::filter(`Institution Name` %in% membersToDisplay) %>%
    # Remove median value as it is not a true entry
    dplyr::filter(! `Institution Name` %in% "MEDIAN") %>%
    dplyr::mutate(allgradStudents = `Part-time graduate students` + `Total fulltime graduate students`) %>%
    # filter denominator with zero value to avoid Inf results
    dplyr::filter(`allgradStudents` != 0) %>%
    # if the resulting number of rows is zero, end and tell user
    { if (nrow(.) == 0) stop("No data available for selected years.") else . } %>%
    dplyr::mutate(proPerGradStudent = MASS::fractions(`Support staff`/ allgradStudents)) %>%
    dplyr::select('Year', 'proPerGradStudent', `Institution Name`) %>%
    dplyr::group_by(`Year`) %>%
    dplyr::top_n(5, proPerGradStudent) %>%
    dplyr::arrange(`Year`, desc(proPerGradStudent)) %>%
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    dplyr::mutate(proPerGradStudent = as.character(proPerGradStudent)) %>%  # Convert to character
    tidyr::pivot_wider(names_from = `Year`, values_from = 'proPerGradStudent') %>%
    kableExtra::kbl() %>%
    kableExtra::kable_paper(lightable_options = "striped")


  # ---
  # Using support staff count per undergraduate student by user selection
  supFTEUndergradStudentUserSelected <- selectedData %>%
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
    dplyr::mutate(supPerUndergradStudent = `Support staff`/ totalUndergradStudents) %>%
    # Replace INF values with NA
    dplyr::mutate(supPerUndergradStudent = na_if(supPerUndergradStudent, Inf)) %>%
    dplyr::group_by(`Year`) %>%
    dplyr::top_n(5, supPerUndergradStudent) %>%
    dplyr::arrange(`Year`, desc(supPerUndergradStudent)) %>%
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    ggplot2::ggplot(aes(x = factor(`Year`),
                        y = `supPerUndergradStudent`,
                        fill = factor(`Institution Name`),
                        width = .75)) +
    ggplot2::geom_bar(position = "dodge", stat = "identity") +
    ggplot2::labs(y = "Support Staff Count (FTE)\nPer Undergrad Student",
                  x = "Year",
                  fill = "ARL Member") +
    ggplot2::theme_bw() +
    ggplot2::ggtitle(label = "Ratio of Support Staff Count (FTE) Per\nUndergrad Student (FT + PT)") +
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



  # Final table - support staff count per undergraduate student by user selection
  supFTEPerUndergradStudentUserSelectedTable <- selectedData %>%
    dplyr::filter(`Year` %in% yearsToDisplay) %>%
    dplyr::filter(`Institution Name` %in% membersToDisplay) %>%
    # Remove median value as it is not a true entry
    dplyr::filter(! `Institution Name` %in% "MEDIAN") %>%
    { if (nrow(.) == 0) stop("No data available for selected years.") else . } %>%
    dplyr::mutate(totalUndergradStudents = ((`Total fulltime students` + `Part-time students, undergraduate and graduate`) -
                                              (`Part-time graduate students` + `Total fulltime graduate students`))) %>%
    # filter denominator with zero value to avoid Inf results
    dplyr::filter(`totalUndergradStudents` != 0) %>%
    # if the resulting number of rows is zero, end and tell user
    { if (nrow(.) == 0) stop("No data available for selected years.") else . } %>%
    dplyr::mutate(proPerUndergradStudent = MASS::fractions(`Support staff`/ totalUndergradStudents)) %>%
    dplyr::select('Year', 'proPerUndergradStudent', `Institution Name`) %>%
    dplyr::group_by(`Year`) %>%
    dplyr::top_n(5, proPerUndergradStudent) %>%
    dplyr::arrange(`Year`, desc(proPerUndergradStudent)) %>%
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    dplyr::mutate(proPerUndergradStudent = as.character(proPerUndergradStudent)) %>%  # Convert to character
    tidyr::pivot_wider(names_from = `Year`, values_from = 'proPerUndergradStudent') %>%
    kableExtra::kbl() %>%
    kableExtra::kable_paper(lightable_options = "striped")


  # ---
  # Using support staff stats per doctoral degree by top contributors (not ARL)
  supFTEDoctoralUserSelected <- selectedData %>%
    dplyr::filter(`Year` %in% yearsToDisplay) %>%
    dplyr::filter(`Institution Name` %in% membersToDisplay) %>%
    # Remove median value as it is not a true entry
    dplyr::filter(! `Institution Name` %in% "MEDIAN") %>%
    # filter denominator with zero value to avoid Inf results
    dplyr::filter(`Doctor's degrees awarded` != 0) %>%
    # if the resulting number of rows is zero, end and tell user
    { if (nrow(.) == 0) stop("No data available for selected years.") else . } %>%
    dplyr::mutate(supPerDoctoral = `Support staff`/ `Doctor's degrees awarded`) %>%
    # Replace INF values with NA
    dplyr::mutate(supPerDoctoral = na_if(supPerDoctoral, Inf)) %>%
    dplyr::group_by(`Year`) %>%
    dplyr::top_n(5, supPerDoctoral) %>%
    dplyr::arrange(`Year`, desc(supPerDoctoral))  %>%
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    ggplot2::ggplot(aes(x = factor(`Year`),
                        y = `supPerDoctoral`,
                        fill = factor(`Institution Name`),
                        width = .75)) +
    ggplot2::geom_bar(position = "dodge", stat = "identity") +
    ggplot2::labs(y = "Support Staff Count (FTE)\nPer Doctoral Degree",
                  x = "Year",
                  fill = "ARL Member") +
    ggplot2::theme_bw() +
    ggplot2::ggtitle(label = "Ratio of Support Staff Count (FTE) Per\nDoctoral Degree For User Selected Members") +
                    # subtitle = "ARL rank is shown on top of each bar.") +
    ggplot2::theme(text = element_text(size = 15, color = 'black'),
                   axis.text.x = element_text(angle = 90,
                                              hjust = 1,
                                              vjust = 0.5,
                                              color = 'black', size = 15),
                   axis.text.y = element_text(color = 'black', size = 15)) +
    ggplot2::scale_fill_manual(values = setColorPalette()) +
    ggplot2::scale_y_continuous(labels = scales::label_comma(),
                                breaks = scales::pretty_breaks(n = 5)) # +
    # Add ranking labels on bars
    # ggplot2::geom_text(aes(label = `Rank in ARL investment index`),
    #                    position = position_dodge(width = 0.9),
    #                    vjust = 0,
    #                    size = 6)


  # Final table - support staff count per doctoral degree by user selection
  supFTEPerDoctoralUserSelectedTable <- selectedData %>%
    dplyr::filter(`Year` %in% yearsToDisplay) %>%
    dplyr::filter(`Institution Name` %in% membersToDisplay) %>%
    # Remove median value as it is not a true entry
    dplyr::filter(! `Institution Name` %in% "MEDIAN") %>%
    # filter denominator with zero value to avoid Inf results
    dplyr::filter(`Doctor's degrees awarded` != 0) %>%
    # if the resulting number of rows is zero, end and tell user
    { if (nrow(.) == 0) stop("No data available for selected years.") else . } %>%
    # filter denominator with zero value to avoid Inf results
    dplyr::filter(`Doctor's degrees awarded` != 0) %>%
    dplyr::mutate(proPerDoctoral = MASS::fractions(`Support staff`/ `Doctor's degrees awarded`)) %>%
    dplyr::select('Year', 'proPerDoctoral', `Institution Name`) %>%
    dplyr::group_by(`Year`) %>%
    dplyr::top_n(5, proPerDoctoral) %>%
    dplyr::arrange(`Year`, desc(proPerDoctoral)) %>%
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    dplyr::mutate(proPerDoctoral = as.character(proPerDoctoral)) %>%  # Convert to character
    tidyr::pivot_wider(names_from = `Year`, values_from = 'proPerDoctoral') %>%
    kableExtra::kbl() %>%
    kableExtra::kable_paper(lightable_options = "striped")



  return(list(supFTETopPerFaculty = supFTETopPerFaculty,
              supFTETopPerStudent = supFTETopPerStudent,
              supFTETopPerGradStudent = supFTETopPerGradStudent,
              supFTETopUndergradStudent = supFTETopUndergradStudent,
              supFTETopPerDoctoral = supFTETopPerDoctoral,
              supFTEPerFacultyUserSelected = supFTEPerFacultyUserSelected,
              supFTEPerStudentUserSelected = supFTEPerStudentUserSelected,
              supFTEGradStudentUserSelected = supFTEGradStudentUserSelected,
              supFTEUndergradStudentUserSelected = supFTEUndergradStudentUserSelected,
              supFTEDoctoralUserSelected = supFTEDoctoralUserSelected,
              supFTETopPerFacultyTable = supFTETopPerFacultyTable,
              supFTETopPerStudentTable = supFTETopPerStudentTable,
              supFTETopPerGradStudentTable = supFTETopPerGradStudentTable,
              supFTETopPerUndergradStudentTable = supFTETopPerUndergradStudentTable,
              supFTETopPerDoctoralTable = supFTETopPerDoctoralTable,
              supFTEPerFacultyUserSelectedTable = supFTEPerFacultyUserSelectedTable,
              supFTEPerStudentUserSelectedTable = supFTEPerStudentUserSelectedTable,
              supFTEPerGradStudentUserSelectedTable = supFTEPerGradStudentUserSelectedTable,
              supFTEPerUndergradStudentUserSelectedTable = supFTEPerUndergradStudentUserSelectedTable,
              supFTEPerDoctoralUserSelectedTable = supFTEPerDoctoralUserSelectedTable))
}
# [END]
