# staffCounts
#' Plots to Compare Professional Library Staff Counts Over Years
#'
#' A function to visualize library professional staff counts, full-time
#' equivalent (FTE), as ratios in comparison to various statistics reported
#' in the annual survey of Association of Research Libraries (ARL) as bar
#' plots. Note, this function provides question 13a of ARL survey as the
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
#'   \item proFTETopPerFaculty - A barplot showing ARL members with highest
#'         ratio of total library professional staff counts (FTE) per
#'         teaching faculty, over user selected number of years.
#'   \item proFTETopPerStudent - A barplot showing ARL members with highest
#'         ratio of total library professional staff counts (FTE) per
#'         student (full-time, FT, and part-time, PT), over user selected
#'         number of years.
#'   \item proFTETopPerGradStudent - A barplot showing ARL members with highest
#'         ratio of total library professional staff counts (FTE) per graduate
#'         student (full-time, FT, and part-time, PT), over user selected
#'         number of years.
#'   \item proFTETopPerUndergradStudent - A barplot showing ARL members with highest
#'         ratio of total library professional staff counts (FTE) per undergraduate
#'         student (full-time, FT, and part-time, PT), over user selected
#'         number of years.
#'   \item proFTETopPerDoctoral - A barplot showing ARL members with highest
#'         ratio of total library professional staff counts (FTE) per doctoral
#'         degree awarded, over user selected number of years.
#'   \item proFTEFacultyUserSelected - A barplot showing ratio of library
#'         professional staff counts (FTE) per teaching faculty for user
#'         selected ARL members, over user selected number of years.
#'   \item proFTEStudentUserSelected - A barplot showing ratio of library
#'         professional staff counts (FTE) per student (full-time, FT, and
#'         part-time, PT) for user selected ARL members, over user selected
#'         number of years.
#'   \item proFTEGradStudentUserSelected - A barplot showing ratio of total
#'         library professional staff counts (FTE) per graduate student
#'         (full-time, FT, and part-time, PT) for user selected ARL members,
#'         over user selected number of years.
#'   \item proFTEPerUndergradStudentUserSelected - A barplot showing ratio of total
#'         library professional staff counts (FTE) per undergraduate student
#'         (full-time, FT, and part-time, PT) for user selected ARL members,
#'         over user selected number of years.
#'   \item proFTEDoctoralUserSelected - A barplot showing ratio of total
#'         library expenditures per per doctoral degree awarded for user
#'         selected ARL members, over user selected number of years.
#'   \item proFTETopPerFacultyTable - A table showing the original values used
#'         for calculating the ratios for ARL members with highest ratio of
#'         total library professional staff counts (FTE) per teaching faculty, over
#'         user selected number of years.
#'   \item proFTETopPerStudentTable - A table showing the original values used
#'         for calculating the ratios for ARL members with highest ratio of total
#'         total library professional staff counts (FTE) per student (full-time, FT,
#'         and part-time, PT), over user selected number of years.
#'   \item proFTETopPerGradStudentTable - A table showing the original values
#'         used for calculating the ratios for ARL members with highest ratio
#'         of total library professional staff counts (FTE) per graduate student
#'         (full-time, FT, and part-time, PT), over user selected number of years.
#'   \item proFTETopPerUndergradStudentTable - A table showing the original values
#'         used for calculating the ratios for ARL members with highest ratio of
#'         total library professional staff counts (FTE) per undergraduate student
#'         (full-time, FT, and part-time, PT), over user selected number of years.
#'   \item proFTETopPerDoctoralTable - A table showing the original values used
#'         for calculating the ratios for ARL members with highest ratio of total
#'         library professional staff counts (FTE) per doctoral degree awarded, over
#'         user selected number of years.
#'   \item proFTEPerFacultyUserSelectedTable - A table showing the original values
#'         used for calculating the ratios for total library professional staff
#'         counts (FTE) per teaching faculty for user selected ARL members, over user
#'         selected number of years.
#'   \item proFTEPerStudentUserSelectedTable - A table showing the original values
#'         used for calculating the ratios for total library professional staff counts
#'         (FTE) per graduate student (full-time, FT, and part-time, PT) for user
#'         selected ARL members, over user selected number of years.
#'   \item proFTEPerGradStudentUserSelectedTable - A table showing the original values
#'         used for calculating the ratios for total library professional staff
#'         salaries per graduate student (full-time, FT, and part-time, PT) for
#'         user selected ARL members, over user selected number of years.
#'   \item proFTEPerUndergradStudentUserSelectedTable - A table showing the
#'         original values used for calculating the ratios for total library
#'         professional staff counts (FTE)per undergraduate student (full-time,
#'         FT, and part-time, PT) for user selected ARL members, over user selected
#'         number of years.
#'   \item proFTEPerDoctoralUserSelectedTable - A table showing the original values
#'         used for calculating the ratios for total library professional staff
#'         counts (FTE) per per doctoral degree awarded for user selected ARL members,
#'         over user selected number of years.
#' }
#'
#' @examples
#' # Reading R package example data
#' # ?ARLDataDownload
#' visProfStaffCounts(dataARL = ARLDataDownload,
#'                    members = c("Library A", "Library B", "Library C", "Library D", "Library E"),
#'                    years = c(2020, 2021, 2022))
#'
#' # Reading actual data downloaded from ARL (not run)
#' # Set file path
#' # ARLData <- readr::read_csv("~/ARLData.csv")
#' # visProfStaffCounts(dataARL = ARLData,
#' #                   members = c("BOSTON", "TORONTO", "OTTAWA", "LAVAL", "HARVARD"),
#' #                   years = c(2015, 2016, 2017, 2022, 2018, 2019))
#'
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
visProfStaffCounts <- function(dataARL, members, years = NA) {

  selectedData <- dataAdjustment(dataARL = dataARL)

  yearsToDisplay <- setYearsToDispaly(years = years,
                                      dataARL = dataARL)

  membersToDisplay <- setMemebersToDispaly(members = members,
                                           dataARL = dataARL)


  # Using professional staff count per faculty by top contributors
  proFTETopPerFaculty <- selectedData %>%
    dplyr::filter(`Year` %in% yearsToDisplay) %>%
    # Remove median value as it is not a true entry
    dplyr::filter(! `Institution Name` %in% "MEDIAN") %>%
    dplyr::mutate(proPerFaculty = `Professional staff`/`Total teaching faculty`) %>%
    # Replace INF values with NA
    dplyr::mutate(proPerFaculty = na_if(proPerFaculty, Inf)) %>%
    dplyr::group_by(`Year`) %>%
    dplyr::top_n(5, proPerFaculty) %>%
    dplyr::arrange(`Year`, desc(proPerFaculty)) %>%
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    ggplot2::ggplot(aes(x = factor(`Year`),
                        y = `proPerFaculty`,
                        fill = factor(`Institution Name`),
                        width = .75)) +
    ggplot2::geom_bar(position = "dodge", stat = "identity") +
    ggplot2::labs(y = "Professional Staff Count (FTE) \nPer Teaching Faculty",
                  x = "Year",
                  fill = "ARL Member") +
    ggplot2::theme_bw() +
    ggplot2::ggtitle(label = "Ratio of Professional Staff Count (FTE) Per Teaching\nFaculty For Top 5 ARL Members Overall") +
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


  # Final table - professional staff count per faculty by top contributors over 5 years
  proFTETopPerFacultyTable <- selectedData %>%
    dplyr::filter(`Year` %in% yearsToDisplay) %>%
    # Remove median value as it is not a true entry
    dplyr::filter(! `Institution Name` %in% "MEDIAN") %>%
    # filter denominator with zero value to avoid Inf results
    dplyr::filter(`Total teaching faculty` != 0) %>%
    { if (nrow(.) == 0) stop("No data available for selected years.") else . } %>%
    dplyr::mutate(proPerFaculty = MASS::fractions(`Professional staff`/`Total teaching faculty`)) %>%
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
  # Using professional staff count per student by top contributors
  proFTETopPerStudent <- selectedData %>%
    dplyr::filter(`Year` %in% yearsToDisplay) %>%
    # Remove median value as it is not a true entry
    dplyr::filter(! `Institution Name` %in% "MEDIAN") %>%
    dplyr::mutate(proPerStudent = `Professional staff`/
                    (`Total fulltime students` + `Part-time students, undergraduate and graduate`)) %>%
    # Replace INF values with NA
    dplyr::mutate(proPerStudent = na_if(proPerStudent, Inf)) %>%
    dplyr::group_by(`Year`) %>%
    dplyr::top_n(5, proPerStudent) %>%
    dplyr::arrange(`Year`, desc(proPerStudent)) %>%
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    ggplot2::ggplot(aes(x = factor(`Year`),
                        y = `proPerStudent`,
                        fill = factor(`Institution Name`),
                        width = .75)) +
    ggplot2::geom_bar(position = "dodge", stat = "identity") +
    ggplot2::labs(y = "Professional Staff Count (FTE) \nPer Student",
                  x = "Year",
                  fill = "ARL Member") +
    ggplot2::theme_bw() +
    ggplot2::ggtitle(label = "Ratio of Professional Staff Count (FTE) Per Student\n(FT + PT) For Top 5 ARL Members Overall") +
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



  # Final table -  professional staff count per student by top contributors
  proFTETopPerStudentTable <- selectedData %>%
    dplyr::filter(`Year` %in% yearsToDisplay) %>%
    # Remove median value as it is not a true entry
    dplyr::filter(! `Institution Name` %in% "MEDIAN") %>%
    dplyr::mutate(allStudents = `Total fulltime students` + `Part-time students, undergraduate and graduate`) %>%
    # filter denominator with zero value to avoid Inf results
    dplyr::filter(allStudents != 0) %>%
    { if (nrow(.) == 0) stop("No data available for selected years.") else . } %>%
    dplyr::mutate(proPerStudent = MASS::fractions(`Professional staff`/ allStudents)) %>%
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
  # Using professional staff count per graduate student by top contributors
  proFTETopPerGradStudent <- selectedData %>%
    dplyr::filter(`Year` %in% yearsToDisplay) %>%
    # Remove median value as it is not a true entry
    dplyr::filter(! `Institution Name` %in% "MEDIAN") %>%
    dplyr::mutate(profPerGradStudent = `Professional staff`/
                    (`Part-time graduate students` + `Total fulltime graduate students`)) %>%
    # Replace INF values with NA
    dplyr::mutate(profPerGradStudent = na_if(profPerGradStudent, Inf)) %>%
    dplyr::group_by(`Year`) %>%
    dplyr::top_n(5, profPerGradStudent) %>%
    dplyr::arrange(`Year`, desc(profPerGradStudent)) %>%
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    ggplot2::ggplot(aes(x = factor(`Year`),
                        y = `profPerGradStudent`,
                        fill = factor(`Institution Name`),
                        width = .75)) +
    ggplot2::geom_bar(position = "dodge", stat = "identity") +
    ggplot2::labs(y = "Professional Staff Count (FTE) \nPer Grad Student",
                  x = "Year",
                  fill = "ARL Member") +
    ggplot2::theme_bw() +
    ggplot2::ggtitle(label = "Ratio of Professional Staff Count (FTE) Per Grad Student\n(FT + PT) For Top 5 ARL Members Overall") +
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



  # Final table - professional staff count per graduate student by top contributors
  proFTETopPerGradStudentTable <- selectedData %>%
    dplyr::filter(`Year` %in% yearsToDisplay) %>%
    # Remove median value as it is not a true entry
    dplyr::filter(! `Institution Name` %in% "MEDIAN") %>%
    dplyr::mutate(allGradStudents = `Part-time graduate students` + `Total fulltime graduate students`) %>%
    # filter denominator with zero value to avoid Inf results
    dplyr::filter(allGradStudents != 0) %>%
    { if (nrow(.) == 0) stop("No data available for selected years.") else . } %>%
    dplyr::mutate(profPerGradStudent = MASS::fractions(`Professional staff`/ allGradStudents)) %>%
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
  # Using professional staff count per undergraduate student by top contributors (not ARL)
  proFTETopPerUndergradStudent <- selectedData %>%
    dplyr::filter(`Year` %in% yearsToDisplay) %>%
    # Remove median value as it is not a true entry
    dplyr::filter(! `Institution Name` %in% "MEDIAN") %>%
    dplyr::mutate(totalUndergradStudents = ((`Total fulltime students` + `Part-time students, undergraduate and graduate`) -
                                              (`Part-time graduate students` + `Total fulltime graduate students`))) %>%
    dplyr::mutate(proPerUndergradStudent = `Professional staff`/ totalUndergradStudents) %>%
    # Replace INF values with NA
    dplyr::mutate(proPerUndergradStudent = na_if(proPerUndergradStudent, Inf)) %>%
    dplyr::group_by(`Year`) %>%
    dplyr::top_n(5, proPerUndergradStudent) %>%
    dplyr::arrange(`Year`, desc(proPerUndergradStudent)) %>%
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    ggplot2::ggplot(aes(x = factor(`Year`),
                        y = `proPerUndergradStudent`,
                        fill = factor(`Institution Name`),
                        width = .75)) +
    ggplot2::geom_bar(position = "dodge", stat = "identity") +
    ggplot2::labs(y = "Professional Staff Count (FTE)\nPer Undergrad Student",
                  x = "Year",
                  fill = "ARL Member") +
    ggplot2::theme_bw() +
    ggplot2::ggtitle(label = "Ratio of Professional Staff Count (FTE) Per Undergrad Student\n(FT + PT) For Top 5 ARL Members Overall") +
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



  # Final table - professional staff count per undergraduate student by top contributors
  proFTETopPerUndergradStudentTable <- selectedData %>%
    dplyr::filter(`Year` %in% yearsToDisplay) %>%
    # Remove median value as it is not a true entry
    dplyr::filter(! `Institution Name` %in% "MEDIAN") %>%
    dplyr::mutate(totalUndergradStudents = ((`Total fulltime students` + `Part-time students, undergraduate and graduate`) -
                                              (`Part-time graduate students` + `Total fulltime graduate students`))) %>%
    # filter denominator with zero value to avoid Inf results
    dplyr::filter(totalUndergradStudents != 0) %>%
    { if (nrow(.) == 0) stop("No data available for selected years.") else . } %>%
    dplyr::mutate(proPerUndergradStudent = MASS::fractions(`Professional staff`/ totalUndergradStudents)) %>%
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
  # Using professional staff count per doctoral degree by top contributors (not ARL)
  proFTETopPerDoctoral <- selectedData %>%
    dplyr::filter(`Year` %in% yearsToDisplay) %>%
    # Remove median value as it is not a true entry
    dplyr::filter(! `Institution Name` %in% "MEDIAN") %>%
    dplyr::mutate(proPerDoctoral = `Professional staff`/ `Doctor's degrees awarded`) %>%
    # Replace INF values with NA
    dplyr::mutate(proPerDoctoral = na_if(proPerDoctoral, Inf)) %>%
    dplyr::group_by(`Year`) %>%
    dplyr::top_n(5, proPerDoctoral) %>%
    dplyr::arrange(`Year`, desc(proPerDoctoral)) %>%
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    dplyr::filter(! `Institution Name` %in% "MEDIAN") %>%
    ggplot2::ggplot(aes(x = factor(`Year`),
                        y = `proPerDoctoral`,
                        fill = factor(`Institution Name`),
                        width = .75)) +
    ggplot2::geom_bar(position = "dodge", stat = "identity") +
    ggplot2::labs(y = "Professional Staff Count (FTE) \nPer Doctoral Degree",
                  x = "Year",
                  fill = "ARL Member") +
    ggplot2::theme_bw() +
    ggplot2::ggtitle(label = "Ratio of Professional Staff Count (FTE) Per Doctoral\nDegree For Top 5 ARL Members Overall") +
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



  # Final table - professional staff count per doctoral degree by top contributors
  proFTETopPerDoctoralTable <- selectedData %>%
    dplyr::filter(`Year` %in% yearsToDisplay) %>%
    # Remove median value as it is not a true entry
    dplyr::filter(! `Institution Name` %in% "MEDIAN") %>%
    # filter denominator with zero value to avoid Inf results
    dplyr::filter(`Doctor's degrees awarded` != 0) %>%
    { if (nrow(.) == 0) stop("No data available for selected years.") else . } %>%
    dplyr::mutate(proPerDoctoral = MASS::fractions(`Professional staff`/ `Doctor's degrees awarded`)) %>%
    # Replace INF values with NA
    dplyr::mutate(proPerDoctoral = na_if(proPerDoctoral, "Inf")) %>%
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
  # Using professional staff count per faculty by user selection
  proFTEPerFacultyUserSelected <- selectedData %>%
    dplyr::filter(`Year` %in% yearsToDisplay) %>%
    dplyr::filter(`Institution Name` %in% membersToDisplay) %>%
    # Remove median value as it is not a true entry
    dplyr::filter(! `Institution Name` %in% "MEDIAN") %>%
    dplyr::mutate(proPerFaculty = `Professional staff`/`Total teaching faculty`) %>%
    # Replace INF values with NA
    dplyr::mutate(proPerFaculty = na_if(proPerFaculty, Inf)) %>%
    dplyr::group_by(`Year`) %>%
    dplyr::top_n(5, proPerFaculty) %>%
    dplyr::arrange(`Year`, desc(proPerFaculty)) %>%
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    ggplot2::ggplot(aes(x = factor(`Year`),
                        y = `proPerFaculty`,
                        fill = factor(`Institution Name`),
                        width = .75)) +
    ggplot2::geom_bar(position = "dodge", stat = "identity") +
    ggplot2::labs(y = "Professional Staff Count (FTE) Per\nTeaching Faculty",
                  x = "Year",
                  fill = "ARL Member") +
    ggplot2::theme_bw() +
    ggplot2::ggtitle(label = "Ratio of Professional Staff Count (FTE) Per\nTeaching Faculty For User Selected Members") +
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


  # Final table - professional staff count per faculty by user selection
  proFTEPerFacultyUserSelectedTable <- selectedData %>%
    dplyr::filter(`Year` %in% yearsToDisplay) %>%
    dplyr::filter(`Institution Name` %in% membersToDisplay) %>%
    # Remove median value as it is not a true entry
    dplyr::filter(! `Institution Name` %in% "MEDIAN") %>%
    { if (nrow(.) == 0) stop("No data available for selected years.") else . } %>%
    dplyr::mutate(proPerFaculty = MASS::fractions(`Professional staff`/`Total teaching faculty`)) %>%
    # Replace INF values with NA
    dplyr::mutate(proPerFaculty = na_if(proPerFaculty, "Inf")) %>%
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
  # Using prof staff stats per student by user selection
  proFTEPerStudentUserSelected <- selectedData %>%
    dplyr::filter(`Year` %in% yearsToDisplay) %>%
    dplyr::filter(`Institution Name` %in% membersToDisplay) %>%
    # Remove median value as it is not a true entry
    dplyr::filter(! `Institution Name` %in% "MEDIAN") %>%
    dplyr::mutate(proPerStudent = `Professional staff`/
                    (`Total fulltime students` + `Part-time students, undergraduate and graduate`)) %>%
    # Replace INF values with NA
    dplyr::mutate(proPerStudent = na_if(proPerStudent, Inf)) %>%
    dplyr::group_by(`Year`) %>%
    dplyr::top_n(5, proPerStudent) %>%
    dplyr::arrange(`Year`, desc(proPerStudent))  %>%
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    ggplot2::ggplot(aes(x = factor(`Year`),
                        y = `proPerStudent`,
                        fill = factor(`Institution Name`),
                        width = .75)) +
    ggplot2::geom_bar(position = "dodge", stat = "identity") +
    ggplot2::labs(y = "Professional Staff Count (FTE)\nPer Student",
                  x = "Year",
                  fill = "ARL Member") +
    ggplot2::theme_bw() +
    ggplot2::ggtitle(label = "Ratio of Professional Staff Count (FTE) Per Student\n(FT + PT) For User Selected Members") +
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


  # Final table - prof staff count per student by user selection
  proFTEPerStudentUserSelectedTable <- selectedData %>%
    dplyr::filter(`Year` %in% yearsToDisplay) %>%
    dplyr::filter(`Institution Name` %in% membersToDisplay) %>%
    # Remove median value as it is not a true entry
    dplyr::filter(! `Institution Name` %in% "MEDIAN") %>%
    { if (nrow(.) == 0) stop("No data available for selected years.") else . } %>%
    dplyr::mutate(allStudents = `Total fulltime students` + `Part-time students, undergraduate and graduate`) %>%
    dplyr::mutate(proPerStudent = MASS::fractions(`Professional staff`/ allStudents)) %>%
    # Replace INF values with NA
    dplyr::mutate(proPerStudent = na_if(proPerStudent, "Inf")) %>%
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
  # Using prof staff stats per graduate student by user selection
  proFTEPerGradStudentUserSelected <- selectedData %>%
    dplyr::filter(`Year` %in% yearsToDisplay) %>%
    dplyr::filter(`Institution Name` %in% membersToDisplay) %>%
    # Remove median value as it is not a true entry
    dplyr::filter(! `Institution Name` %in% "MEDIAN") %>%
    dplyr::mutate(proPerGradStudent = `Professional staff`/
                    (`Part-time graduate students` + `Total fulltime graduate students`)) %>%
    # Replace INF values with NA
    dplyr::mutate(proPerGradStudent = na_if(proPerGradStudent, Inf)) %>%
    dplyr::group_by(`Year`) %>%
    dplyr::top_n(5, proPerGradStudent) %>%
    dplyr::arrange(`Year`, desc(proPerGradStudent)) %>%
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    ggplot2::ggplot(aes(x = factor(`Year`),
                        y = `proPerGradStudent`,
                        fill = factor(`Institution Name`),
                        width = .75)) +
    ggplot2::geom_bar(position = "dodge", stat = "identity") +
    ggplot2::labs(y = "Professional Staff Count (FTE)\nPer Grad Student",
                  x = "Year",
                  fill = "ARL Member") +
    ggplot2::theme_bw() +
    ggplot2::ggtitle(label = "Ratio of Professional Staff Count (FTE) Per Grad Student\n(FT + PT) For User Selected Members") +
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



  # Final table - prof staff count per graduate student by user selection
  proFTEPerGradStudentUserSelectedTable <- selectedData %>%
    dplyr::filter(`Year` %in% yearsToDisplay) %>%
    dplyr::filter(`Institution Name` %in% membersToDisplay) %>%
    # Remove median value as it is not a true entry
    dplyr::filter(! `Institution Name` %in% "MEDIAN") %>%
    { if (nrow(.) == 0) stop("No data available for selected years.") else . } %>%
    dplyr::mutate(allgradStudents = `Part-time graduate students` + `Total fulltime graduate students`) %>%
    dplyr::mutate(proPerGradStudent = MASS::fractions(`Professional staff`/ allgradStudents)) %>%
    # Replace INF values with NA
    dplyr::mutate(proPerGradStudent = na_if(proPerGradStudent, "Inf")) %>%
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
  # Using professional staff count per undergraduate student by user selection
  proFTEPerUndergradStudentUserSelected <- selectedData %>%
    dplyr::filter(`Year` %in% yearsToDisplay) %>%
    dplyr::filter(`Institution Name` %in% membersToDisplay) %>%
    # Remove median value as it is not a true entry
    dplyr::filter(! `Institution Name` %in% "MEDIAN") %>%
    dplyr::mutate(totalUndergradStudents = ((`Total fulltime students` + `Part-time students, undergraduate and graduate`) -
                                              (`Part-time graduate students` + `Total fulltime graduate students`))) %>%
    dplyr::mutate(proPerUndergradStudent = `Professional staff`/ totalUndergradStudents) %>%
    # Replace INF values with NA
    dplyr::mutate(proPerUndergradStudent = na_if(proPerUndergradStudent, Inf)) %>%
    dplyr::group_by(`Year`) %>%
    dplyr::top_n(5, proPerUndergradStudent) %>%
    dplyr::arrange(`Year`, desc(proPerUndergradStudent)) %>%
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    ggplot2::ggplot(aes(x = factor(`Year`),
                        y = `proPerUndergradStudent`,
                        fill = factor(`Institution Name`),
                        width = .75)) +
    ggplot2::geom_bar(position = "dodge", stat = "identity") +
    ggplot2::labs(y = "Professional Staff Count (FTE)\nPer Undergrad Student",
                  x = "Year",
                  fill = "ARL Member") +
    ggplot2::theme_bw() +
    ggplot2::ggtitle(label = "Ratio of Professional Staff Count (FTE) Per\nUndergrad Student (FT + PT)") +
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




  # Final table - prof staff count per undergraduate student by user selection
  proFTEPerUndergradStudentUserSelectedTable <- selectedData %>%
    dplyr::filter(`Year` %in% yearsToDisplay) %>%
    dplyr::filter(`Institution Name` %in% membersToDisplay) %>%
    # Remove median value as it is not a true entry
    dplyr::filter(! `Institution Name` %in% "MEDIAN") %>%
    { if (nrow(.) == 0) stop("No data available for selected years.") else . } %>%
    dplyr::mutate(totalUndergradStudents = ((`Total fulltime students` + `Part-time students, undergraduate and graduate`) -
                                              (`Part-time graduate students` + `Total fulltime graduate students`))) %>%
    dplyr::mutate(proPerUndergradStudent = MASS::fractions(`Professional staff`/ totalUndergradStudents)) %>%
    # Replace INF values with NA
    dplyr::mutate(proPerUndergradStudent = na_if(proPerUndergradStudent, "Inf")) %>%
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
  # Using prof staff stats per doctoral degree by user selection
  proFTEPerDoctoralUserSelected <- selectedData %>%
    dplyr::filter(`Year` %in% yearsToDisplay) %>%
    dplyr::filter(`Institution Name` %in% membersToDisplay) %>%
    # Remove median value as it is not a true entry
    dplyr::filter(! `Institution Name` %in% "MEDIAN") %>%
    dplyr::mutate(proPerDoctoral = `Professional staff`/ `Doctor's degrees awarded`) %>%
    # Replace INF values with NA
    dplyr::mutate(proPerDoctoral = na_if(proPerDoctoral, Inf)) %>%
    dplyr::group_by(`Year`) %>%
    dplyr::top_n(5, proPerDoctoral) %>%
    dplyr::arrange(`Year`, desc(proPerDoctoral))  %>%
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    ggplot2::ggplot(aes(x = factor(`Year`),
                        y = `proPerDoctoral`,
                        fill = factor(`Institution Name`),
                        width = .75)) +
    ggplot2::geom_bar(position = "dodge", stat = "identity") +
    ggplot2::labs(y = "Professional Staff Count (FTE)\nPer Doctoral Degree",
                  x = "Year",
                  fill = "ARL Member") +
    ggplot2::theme_bw() +
    ggplot2::ggtitle(label = "Ratio of Professional Staff Count (FTE) Per\nDoctoral Degree For User Selected Members") +
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



  # Final table - prof staff count per doctoral degree by user selection
  proFTEPerDoctoralUserSelectedTable <- selectedData %>%
    dplyr::filter(`Year` %in% yearsToDisplay) %>%
    dplyr::filter(`Institution Name` %in% membersToDisplay) %>%
    # Remove median value as it is not a true entry
    dplyr::filter(! `Institution Name` %in% "MEDIAN") %>%
    { if (nrow(.) == 0) stop("No data available for selected years.") else . } %>%
    dplyr::mutate(proPerDoctoral = MASS::fractions(`Professional staff`/ `Doctor's degrees awarded`)) %>%
    # Replace INF values with NA
    dplyr::mutate(proPerDoctoral = na_if(proPerDoctoral, "Inf")) %>%
    dplyr::select('Year', 'proPerDoctoral', `Institution Name`) %>%
    dplyr::group_by(`Year`) %>%
    dplyr::top_n(5, proPerDoctoral) %>%
    dplyr::arrange(`Year`, desc(proPerDoctoral)) %>%
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    dplyr::mutate(proPerDoctoral = as.character(proPerDoctoral)) %>%  # Convert to character
    tidyr::pivot_wider(names_from = `Year`, values_from = 'proPerDoctoral') %>%
    kableExtra::kbl() %>%
    kableExtra::kable_paper(lightable_options = "striped")


  return(list(proFTETopPerFaculty = proFTETopPerFaculty,
              proFTETopPerStudent = proFTETopPerStudent,
              proFTETopPerGradStudent = proFTETopPerGradStudent,
              proFTETopPerUndergradStudent = proFTETopPerUndergradStudent,
              proFTETopPerDoctoral = proFTETopPerDoctoral,
              proFTEPerFacultyUserSelected = proFTEPerFacultyUserSelected,
              proFTEPerStudentUserSelected = proFTEPerStudentUserSelected,
              proFTEPerGradStudentUserSelected = proFTEPerGradStudentUserSelected,
              proFTEPerUndergradStudentUserSelected = proFTEPerUndergradStudentUserSelected,
              proFTEPerDoctoralUserSelected = proFTEPerDoctoralUserSelected,
              proFTETopPerFacultyTable = proFTETopPerFacultyTable,
              proFTETopPerStudentTable = proFTETopPerStudentTable,
              proFTETopPerGradStudentTable = proFTETopPerGradStudentTable,
              proFTETopPerUndergradStudentTable = proFTETopPerUndergradStudentTable,
              proFTETopPerDoctoralTable = proFTETopPerDoctoralTable,
              proFTEPerFacultyUserSelectedTable = proFTEPerFacultyUserSelectedTable,
              proFTEPerStudentUserSelectedTable = proFTEPerStudentUserSelectedTable,
              proFTEPerGradStudentUserSelectedTable = proFTEPerGradStudentUserSelectedTable,
              proFTEPerUndergradStudentUserSelectedTable = proFTEPerUndergradStudentUserSelectedTable,
              proFTEPerDoctoralUserSelectedTable = proFTEPerDoctoralUserSelectedTable))

}
# [END]
