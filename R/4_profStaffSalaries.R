# profStaffSalaries
#' Plots to Compare Salaries Professional Library Staff Over Years
#'
#' A function to visualize salaries of professional library staff
#' in United States Dollars (USD), as ratios in comparison to various
#' statistics reported in the annual survey of Association of Research
#' Libraries (ARL) as bar plots. Note, this function provides question
#' 8a of ARL survey as the numerator of the ratio.
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
#'   \item proSalTopPerFaculty - A barplot showing ARL members with highest
#'         ratio of total library professional staff salaries per
#'         teaching faculty, over user selected number of years.
#'   \item proSalTopPerStudent - A barplot showing ARL members with highest
#'         ratio of total library professional staff salaries per
#'         student (full-time, FT, and part-time, PT), over user selected
#'         number of years.
#'   \item proSalTopPerGradStudent - A barplot showing ARL members with highest
#'         ratio of total library professional staff salaries per graduate
#'         student (full-time, FT, and part-time, PT), over user selected
#'         number of years.
#'   \item proSalTopPerUndergradStudent - A barplot showing ARL members with highest
#'         ratio of total library professional staff salaries per undergraduate
#'         student (full-time, FT, and part-time, PT), over user selected
#'         number of years.
#'   \item proSalTopPerDoctoral - A barplot showing ARL members with highest
#'         ratio of total library professional staff salaries per doctoral
#'         degree awarded, over user selected number of years.
#'   \item proSalFacultyUserSelected - A barplot showing ratio of library
#'         professional staff salaries per teaching faculty for user
#'         selected ARL members, over user selected number of years.
#'   \item proSalPerStudentUserSelected - A barplot showing ratio of library
#'         professional staff salaries per student (full-time, FT, and
#'         part-time, PT) for user selected ARL members, over user selected
#'         number of years.
#'   \item proSalGradStudentUserSelected - A barplot showing ratio of library
#'         professional staff salaries per graduate student (full-time, FT,
#'         and part-time, PT) for user selected ARL members, over user
#'         selected number of years.
#'   \item proSalPerUndergradStudentUserSelected - A barplot showing ratio of library
#'         professional staff salaries per undergraduate student (full-time, FT,
#'         and part-time, PT) for user selected ARL members, over user
#'         selected number of years.
#'   \item proSalPerDoctoralUserSelected - A barplot showing ratio of library
#'         professional staff salaries per per doctoral degree awarded for user
#'         selected ARL members, over user selected number of years.
#'   \item proSalTopPerFacultyTable - A table showing the original values used
#'         for calculating the ratios for ARL members with highest ratio of
#'         total library professional staff salaries per teaching faculty, over
#'         user selected number of years.
#'   \item proSalTopPerStudentTable - A table showing the original values used
#'         for calculating the ratios for ARL members with highest ratio of total
#'         library professional staff salaries per student (full-time, FT,
#'         and part-time, PT), over user selected number of years.
#'   \item proSalTopPerGradStudentTable - A table showing the original values
#'         used for calculating the ratios for ARL members with highest ratio
#'         of total library professional staff salaries per graduate student
#'         (full-time, FT, and part-time, PT), over user selected number of years.
#'   \item proSalTopPerUndergradStudentTable - A table showing the original values
#'         used for calculating the ratios for ARL members with highest ratio of total
#'         library professional staff salaries per undergraduate student
#'         (full-time, FT, and part-time, PT), over user selected number of years.
#'   \item proSalTopPerDoctoralTable - A table showing the original values used
#'         for calculating the ratios for ARL members with highest ratio of total
#'         library professional staff salaries per doctoral degree awarded, over
#'         user selected number of years.
#'   \item proSalPerFacultyUserSelectedTable - A table showing the original values
#'         used for calculating the ratios for total library professional staff
#'         salaries per teaching faculty for user selected ARL members, over user
#'         selected number of years.
#'   \item proSalPerStudentUserSelectedTable - A table showing the original values
#'         used for calculating the ratios for total library professional staff
#'         salaries per graduate student (full-time, FT, and part-time, PT) for user
#'         selected ARL members, over user selected number of years.
#'   \item proSalPerGradStudentUserSelectedTable - A table showing the original values
#'         used for calculating the ratios for total library professional staff
#'         salaries per graduate student (full-time, FT, and part-time, PT) for
#'         user selected ARL members, over user selected number of years.
#'   \item proSalPerUndergradStudentUserSelectedTable - A table showing the
#'         original values used for calculating the ratios for total library professional
#'         staff salaries per undergraduate student (full-time, FT, and part-time, PT)
#'         for user selected ARL members, over user selected number of years.
#'   \item proSalPerDoctoralUserSelectedTable - A table showing the original values
#'         used for calculating the ratios for total library professional staff
#'         salaries per per doctoral degree awarded for user selected ARL members,
#'         over user selected number of years.
#' }
#'
#' @examples
#' # Reading R package example data
#' # ?ARLDataDownload
#' visProfStaffSalaries(dataARL = ARLDataDownload,
#'                    members = c("Library A", "Library B", "Library C", "Library D", "Library E"),
#'                    years = c(2020, 2021, 2022))
#'
#' # Reading actual data downloaded from ARL (not run)
#' # Set file path
#' # ARLData <- readr::read_csv("~/ARLData.csv")
#' # visProfStaffSalaries(dataARL = ARLData,
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
visProfStaffSalaries <- function(dataARL, members, years = NA) {

  selectedData <- dataAdjustment(dataARL = dataARL)

  yearsToDisplay <- setYearsToDispaly(years = years,
                                      dataARL = dataARL)

  membersToDisplay <- setMemebersToDispaly(members = members,
                                           dataARL = dataARL)


  # Using professional staff salaries per faculty by top contributors
  proSalTopPerFaculty <- selectedData %>%
    dplyr::filter(`Year` %in% yearsToDisplay) %>%
    # Remove median value as it is not a true entry
    dplyr::filter(! `Institution Name` %in% "MEDIAN") %>%
    dplyr::mutate(proPerFaculty = `Professional salaries & wages`/`Total teaching faculty`) %>%
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
    ggplot2::labs(y = "Professional Staff Salaries \nPer Teaching Faculty",
                  x = "Year",
                  fill = "ARL Member") +
    ggplot2::theme_bw() +
    ggplot2::ggtitle(label = "Ratio of Professional Staff Salaries Per Teaching\nFaculty For Top 5 ARL Members Overall") +
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




  # Final table - professional staff salaries per faculty by top contributors over 5 years
  proSalTopPerFacultyTable <- selectedData %>%
    dplyr::filter(`Year` %in% yearsToDisplay) %>%
    # Remove median value as it is not a true entry
    dplyr::filter(! `Institution Name` %in% "MEDIAN") %>%
    # filter denominator with zero value to avoid Inf results
    dplyr::filter(`Total teaching faculty` != 0) %>%
    { if (nrow(.) == 0) stop("No data available for selected years.") else . } %>%
    dplyr::mutate(expPerFaculty = MASS::fractions(`Professional salaries & wages`/`Total teaching faculty`)) %>%
    dplyr::select('Year', 'expPerFaculty', `Institution Name`) %>%
    dplyr::group_by(`Year`) %>%
    dplyr::top_n(5, expPerFaculty) %>%
    dplyr::arrange(`Year`, desc(expPerFaculty)) %>%
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    dplyr::mutate(expPerFaculty = as.character(expPerFaculty)) %>%  # Convert to character
    tidyr::pivot_wider(names_from = `Year`, values_from = 'expPerFaculty') %>%
    kableExtra::kbl() %>%
    kableExtra::kable_paper(lightable_options = "striped")




  # ---
  # Using professional staff salaries per student by top contributors
  proSalTopPerStudent <- selectedData %>%
    dplyr::filter(`Year` %in% yearsToDisplay) %>%
    # Remove median value as it is not a true entry
    dplyr::filter(! `Institution Name` %in% "MEDIAN") %>%
    dplyr::mutate(proPerStudent = `Professional salaries & wages`/
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
    ggplot2::labs(y = "Professional Staff Salaries\nPer Student",
                  x = "Year",
                  fill = "ARL Member") +
    ggplot2::theme_bw() +
    ggplot2::ggtitle(label = "Ratio of Professional Staff Salaries (FTE) Per Student\n(FT + PT) For Top 5 ARL Members Overall") +
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


  # Final table -  professional staff salaries per student by top contributors
  proSalTopPerStudentTable <- selectedData %>%
    dplyr::filter(`Year` %in% yearsToDisplay) %>%
    # Remove median value as it is not a true entry
    dplyr::filter(! `Institution Name` %in% "MEDIAN") %>%
    dplyr::mutate(allStudents = `Total fulltime students` + `Part-time students, undergraduate and graduate`) %>%
    # filter denominator with zero value to avoid Inf results
    dplyr::filter(allStudents != 0) %>%
    { if (nrow(.) == 0) stop("No data available for selected years.") else . } %>%
    dplyr::mutate(expPerStudent = MASS::fractions(`Professional salaries & wages`/ allStudents)) %>%
    dplyr::select('Year', 'expPerStudent', `Institution Name`) %>%
    dplyr::group_by(`Year`) %>%
    dplyr::top_n(5, expPerStudent) %>%
    dplyr::arrange(`Year`, desc(expPerStudent)) %>%
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    dplyr::mutate(expPerStudent = as.character(expPerStudent)) %>%  # Convert to character
    tidyr::pivot_wider(names_from = `Year`, values_from = 'expPerStudent') %>%
    kableExtra::kbl() %>%
    kableExtra::kable_paper(lightable_options = "striped")


  # ---
  # Using professional staff salaries per graduate student by top contributors
  proSalTopPerGradStudent <- selectedData %>%
    dplyr::filter(`Year` %in% yearsToDisplay) %>%
    # Remove median value as it is not a true entry
    dplyr::filter(! `Institution Name` %in% "MEDIAN") %>%
    dplyr::mutate(profPerGradStudent = `Professional salaries & wages`/
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
    ggplot2::labs(y = "Professional Staff Salaries\nPer Grad Student",
                  x = "Year",
                  fill = "ARL Member") +
    ggplot2::theme_bw() +
    ggplot2::ggtitle(label = "Ratio of Professional Staff Salaries Per Grad\nStudent (FT + PT)  For Top 5 ARL Members Overall") +
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



  # Final table - professional staff salaries per graduate student by top contributors
  proSalTopPerGradStudentTable <- selectedData %>%
    dplyr::filter(`Year` %in% yearsToDisplay) %>%
    # Remove median value as it is not a true entry
    dplyr::filter(! `Institution Name` %in% "MEDIAN") %>%
    dplyr::mutate(allGradStudents = `Part-time graduate students` + `Total fulltime graduate students`) %>%
    # filter denominator with zero value to avoid Inf results
    dplyr::filter(allGradStudents != 0) %>%
    { if (nrow(.) == 0) stop("No data available for selected years.") else . } %>%
    dplyr::mutate(expPerGradStudent = MASS::fractions(`Professional salaries & wages`/ allGradStudents)) %>%
    dplyr::select('Year', 'expPerGradStudent', `Institution Name`) %>%
    dplyr::group_by(`Year`) %>%
    dplyr::top_n(5, expPerGradStudent) %>%
    dplyr::arrange(`Year`, desc(expPerGradStudent)) %>%
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    dplyr::mutate(expPerGradStudent = as.character(expPerGradStudent)) %>%  # Convert to character
    tidyr::pivot_wider(names_from = `Year`, values_from = 'expPerGradStudent') %>%
    kableExtra::kbl() %>%
    kableExtra::kable_paper(lightable_options = "striped")




  # ---
  # Using professional staff salaries per undergraduate student by top contributors (not ARL)
  proSalTopPerUndergradStudent <- selectedData %>%
    dplyr::filter(`Year` %in% yearsToDisplay) %>%
    # Remove median value as it is not a true entry
    dplyr::filter(! `Institution Name` %in% "MEDIAN") %>%
    dplyr::mutate(totalUndergradStudents = ((`Total fulltime students` + `Part-time students, undergraduate and graduate`) -
                                              (`Part-time graduate students` + `Total fulltime graduate students`))) %>%
    dplyr::mutate(proPerUndergradStudent = `Professional salaries & wages`/ totalUndergradStudents) %>%
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
    ggplot2::labs(y = "Professional Staff Salaries\nPer Undergrad Student",
                  x = "Year",
                  fill = "ARL Member") +
    ggplot2::theme_bw() +
    ggplot2::ggtitle(label = "Ratio of Professional Staff Salaries Per Undergrad\nStudent (FT + PT) For Top 5 ARL Members Overall") +
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



  # Final table - professional staff salaries per undergraduate student by top contributors
  proSalTopPerUndergradStudentTable <- selectedData %>%
    dplyr::filter(`Year` %in% yearsToDisplay) %>%
    # Remove median value as it is not a true entry
    dplyr::filter(! `Institution Name` %in% "MEDIAN") %>%
    dplyr::mutate(totalUndergradStudents = ((`Total fulltime students` + `Part-time students, undergraduate and graduate`) -
                                              (`Part-time graduate students` + `Total fulltime graduate students`))) %>%
    # filter denominator with zero value to avoid Inf results
    dplyr::filter(totalUndergradStudents != 0) %>%
    { if (nrow(.) == 0) stop("No data available for selected years.") else . } %>%
    dplyr::mutate(expPerUndergradStudent = MASS::fractions(`Professional salaries & wages`/ totalUndergradStudents)) %>%
    dplyr::select('Year', 'expPerUndergradStudent', `Institution Name`) %>%
    dplyr::group_by(`Year`) %>%
    dplyr::top_n(5, expPerUndergradStudent) %>%
    dplyr::arrange(`Year`, desc(expPerUndergradStudent)) %>%
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    dplyr::mutate(expPerUndergradStudent = as.character(expPerUndergradStudent)) %>%  # Convert to character
    tidyr::pivot_wider(names_from = `Year`, values_from = 'expPerUndergradStudent') %>%
    kableExtra::kbl() %>%
    kableExtra::kable_paper(lightable_options = "striped")


  # ---
  # Using professional staff salaries per doctoral degree by top contributors (not ARL)
  proSalTopPerDoctoral <- selectedData %>%
    dplyr::filter(`Year` %in% yearsToDisplay) %>%
    # Remove median value as it is not a true entry
    dplyr::filter(! `Institution Name` %in% "MEDIAN") %>%
    dplyr::mutate(proPerDoctoral = `Professional salaries & wages`/ `Doctor's degrees awarded`) %>%
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
    ggplot2::labs(y = "Professional Staff Salaries\nPer Doctoral Degree",
                  x = "Year",
                  fill = "ARL Member") +
    ggplot2::theme_bw() +
    ggplot2::ggtitle(label = "Ratio of Professional Staff Salaries Per\nDoctoral Degree For Top 5 ARL Members Overall") +
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


  # Final table - professional staff salaries per doctoral degree by top contributors
  proSalTopPerDoctoralTable <- selectedData %>%
    dplyr::filter(`Year` %in% yearsToDisplay) %>%
    # Remove median value as it is not a true entry
    dplyr::filter(! `Institution Name` %in% "MEDIAN") %>%
    # filter denominator with zero value to avoid Inf results
    dplyr::filter(`Doctor's degrees awarded` != 0) %>%
    { if (nrow(.) == 0) stop("No data available for selected years.") else . } %>%
    dplyr::mutate(expPerDoctoral = MASS::fractions(`Professional salaries & wages`/ `Doctor's degrees awarded`)) %>%
    # Replace INF values with NA
    dplyr::mutate(expPerDoctoral = na_if(expPerDoctoral, "Inf")) %>%
    dplyr::select('Year', 'expPerDoctoral', `Institution Name`) %>%
    dplyr::group_by(`Year`) %>%
    dplyr::top_n(5, expPerDoctoral) %>%
    dplyr::arrange(`Year`, desc(expPerDoctoral)) %>%
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    dplyr::mutate(expPerDoctoral = as.character(expPerDoctoral)) %>%  # Convert to character
    tidyr::pivot_wider(names_from = `Year`, values_from = 'expPerDoctoral') %>%
    kableExtra::kbl() %>%
    kableExtra::kable_paper(lightable_options = "striped")




  # ---
  # Using professional staff salaries per faculty by user selection
  proSalFacultyUserSelected <- selectedData %>%
    dplyr::filter(`Year` %in% yearsToDisplay) %>%
    dplyr::filter(`Institution Name` %in% membersToDisplay) %>%
    # Remove median value as it is not a true entry
    dplyr::filter(! `Institution Name` %in% "MEDIAN") %>%
    dplyr::mutate(proPerFaculty = `Professional salaries & wages`/`Total teaching faculty`) %>%
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
    ggplot2::labs(y = "Professional Staff Salaries Per\nTeaching Faculty",
                  x = "Year",
                  fill = "ARL Member") +
    ggplot2::theme_bw() +
    ggplot2::ggtitle(label = "Ratio of Professional Staff Salaries Per\nTeaching Faculty For User Selected Members") +
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



  # Final table - professional staff salaries per faculty by user selection
  proSalPerFacultyUserSelectedTable <- selectedData %>%
    dplyr::filter(`Year` %in% yearsToDisplay) %>%
    dplyr::filter(`Institution Name` %in% membersToDisplay) %>%
    # Remove median value as it is not a true entry
    dplyr::filter(! `Institution Name` %in% "MEDIAN") %>%
    { if (nrow(.) == 0) stop("No data available for selected years.") else . } %>%
    dplyr::mutate(expPerFaculty = MASS::fractions(`Professional salaries & wages`/`Total teaching faculty`)) %>%
    # Replace INF values with NA
    dplyr::mutate(expPerFaculty = na_if(expPerFaculty, "Inf")) %>%
    dplyr::select('Year', 'expPerFaculty', `Institution Name`) %>%
    dplyr::group_by(`Year`) %>%
    dplyr::top_n(5, expPerFaculty) %>%
    dplyr::arrange(`Year`, desc(expPerFaculty)) %>%
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    dplyr::mutate(expPerFaculty = as.character(expPerFaculty)) %>%  # Convert to character
    tidyr::pivot_wider(names_from = `Year`, values_from = 'expPerFaculty') %>%
    kableExtra::kbl() %>%
    kableExtra::kable_paper(lightable_options = "striped")




  # ---
  # Using prof staff salaries per student by user selection
  proSalPerStudentUserSelected <- selectedData %>%
    dplyr::filter(`Year` %in% yearsToDisplay) %>%
    dplyr::filter(`Institution Name` %in% membersToDisplay) %>%
    # Remove median value as it is not a true entry
    dplyr::filter(! `Institution Name` %in% "MEDIAN") %>%
    dplyr::mutate(proPerStudent = `Professional salaries & wages`/
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
    ggplot2::labs(y = "Professional Staff Salaries\nPer Student",
                  x = "Year",
                  fill = "ARL Member") +
    ggplot2::theme_bw() +
    ggplot2::ggtitle(label = "Ratio of Professional Staff Salaries Per Student\n(FT + PT) For User Selected Members") +
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


  # Final table - prof staff salaries per student by user selection
  proSalPerStudentUserSelectedTable <- selectedData %>%
    dplyr::filter(`Year` %in% yearsToDisplay) %>%
    dplyr::filter(`Institution Name` %in% membersToDisplay) %>%
    # Remove median value as it is not a true entry
    dplyr::filter(! `Institution Name` %in% "MEDIAN") %>%
    { if (nrow(.) == 0) stop("No data available for selected years.") else . } %>%
    dplyr::mutate(allStudents = `Total fulltime students` + `Part-time students, undergraduate and graduate`) %>%
    dplyr::mutate(expPerStudent = MASS::fractions(`Professional salaries & wages`/ allStudents)) %>%
    # Replace INF values with NA
    dplyr::mutate(expPerStudent = na_if(expPerStudent, "Inf")) %>%
    dplyr::select('Year', 'expPerStudent', `Institution Name`) %>%
    dplyr::group_by(`Year`) %>%
    dplyr::top_n(5, expPerStudent) %>%
    dplyr::arrange(`Year`, desc(expPerStudent)) %>%
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    dplyr::mutate(expPerStudent = as.character(expPerStudent)) %>%  # Convert to character
    tidyr::pivot_wider(names_from = `Year`, values_from = 'expPerStudent') %>%
    kableExtra::kbl() %>%
    kableExtra::kable_paper(lightable_options = "striped")



  # ---
  # Using prof staff salaries per graduate student by user selection
  proSalPerGradStudentUserSelected <- selectedData %>%
    dplyr::filter(`Year` %in% yearsToDisplay) %>%
    dplyr::filter(`Institution Name` %in% membersToDisplay) %>%
    # Remove median value as it is not a true entry
    dplyr::filter(! `Institution Name` %in% "MEDIAN") %>%
    dplyr::mutate(proPerGradStudent = `Professional salaries & wages`/
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
    ggplot2::labs(y = "Professional Staff Salaries\nPer Grad Student",
                  x = "Year",
                  fill = "ARL Member") +
    ggplot2::theme_bw() +
    ggplot2::ggtitle(label = "Ratio of Professional Staff Salaries Per Grad Student\n(FT + PT) For User Selected Members") +
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




  # Final table - prof staff salaries per graduate student by user selection
  proSalPerGradStudentUserSelectedTable <- selectedData %>%
    dplyr::filter(`Year` %in% yearsToDisplay) %>%
    dplyr::filter(`Institution Name` %in% membersToDisplay) %>%
    # Remove median value as it is not a true entry
    dplyr::filter(! `Institution Name` %in% "MEDIAN") %>%
    { if (nrow(.) == 0) stop("No data available for selected years.") else . } %>%
    dplyr::mutate(allgradStudents = `Part-time graduate students` + `Total fulltime graduate students`) %>%
    dplyr::mutate(expPerGradStudent = MASS::fractions(`Professional salaries & wages`/ allgradStudents)) %>%
    # Replace INF values with NA
    dplyr::mutate(expPerGradStudent = na_if(expPerGradStudent, "Inf")) %>%
    dplyr::select('Year', 'expPerGradStudent', `Institution Name`) %>%
    dplyr::group_by(`Year`) %>%
    dplyr::top_n(5, expPerGradStudent) %>%
    dplyr::arrange(`Year`, desc(expPerGradStudent)) %>%
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    dplyr::mutate(expPerGradStudent = as.character(expPerGradStudent)) %>%  # Convert to character
    tidyr::pivot_wider(names_from = `Year`, values_from = 'expPerGradStudent') %>%
    kableExtra::kbl() %>%
    kableExtra::kable_paper(lightable_options = "striped")



  # ---
  # Using prof staff salaries per undergraduate student by user selection
  proSalPerUndergradStudentUserSelected <- selectedData %>%
    dplyr::filter(`Year` %in% yearsToDisplay) %>%
    dplyr::filter(`Institution Name` %in% membersToDisplay) %>%
    # Remove median value as it is not a true entry
    dplyr::filter(! `Institution Name` %in% "MEDIAN") %>%
    dplyr::mutate(totalUndergradStudents = ((`Total fulltime students` + `Part-time students, undergraduate and graduate`) -
                                              (`Part-time graduate students` + `Total fulltime graduate students`))) %>%
    dplyr::mutate(proPerUndergradStudent = `Professional salaries & wages`/ totalUndergradStudents) %>%
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
    ggplot2::labs(y = "Professional Staff Salaries\nPer Undergrad Student",
                  x = "Year",
                  fill = "ARL Member") +
    ggplot2::theme_bw() +
    ggplot2::ggtitle(label = "Ratio of Professional Staff Salaries Per\nUndergrad Student (FT + PT) For User Selected Members") +
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



  # Final table - prof staff salaries per undergraduate student by user selection
  proSalPerUndergradStudentUserSelectedTable <- selectedData %>%
    dplyr::filter(`Year` %in% yearsToDisplay) %>%
    dplyr::filter(`Institution Name` %in% membersToDisplay) %>%
    # Remove median value as it is not a true entry
    dplyr::filter(! `Institution Name` %in% "MEDIAN") %>%
    { if (nrow(.) == 0) stop("No data available for selected years.") else . } %>%
    dplyr::mutate(totalUndergradStudents = ((`Total fulltime students` + `Part-time students, undergraduate and graduate`) -
                                              (`Part-time graduate students` + `Total fulltime graduate students`))) %>%
    dplyr::mutate(expPerUndergradStudent = MASS::fractions(`Professional salaries & wages`/ totalUndergradStudents)) %>%
    # Replace INF values with NA
    dplyr::mutate(expPerUndergradStudent = na_if(expPerUndergradStudent, "Inf")) %>%
    dplyr::select('Year', 'expPerUndergradStudent', `Institution Name`) %>%
    dplyr::group_by(`Year`) %>%
    dplyr::top_n(5, expPerUndergradStudent) %>%
    dplyr::arrange(`Year`, desc(expPerUndergradStudent)) %>%
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    dplyr::mutate(expPerUndergradStudent = as.character(expPerUndergradStudent)) %>%  # Convert to character
    tidyr::pivot_wider(names_from = `Year`, values_from = 'expPerUndergradStudent') %>%
    kableExtra::kbl() %>%
    kableExtra::kable_paper(lightable_options = "striped")


  # ---
  # Using prof staff salaries per doctoral degree by user selection
  proSalPerDoctoralUserSelected <- selectedData %>%
    dplyr::filter(`Year` %in% yearsToDisplay) %>%
    dplyr::filter(`Institution Name` %in% membersToDisplay) %>%
    # Remove median value as it is not a true entry
    dplyr::filter(! `Institution Name` %in% "MEDIAN") %>%
    dplyr::mutate(proPerDoctoral = `Professional salaries & wages`/ `Doctor's degrees awarded`) %>%
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
    ggplot2::labs(y = "Professional Staff Salaries\nPer Doctoral Degree",
                  x = "Year",
                  fill = "ARL Member") +
    ggplot2::theme_bw() +
    ggplot2::ggtitle(label = "Ratio of Professional Staff Salaries Per\nDoctoral Degree For User Selected Members") +
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



  # Final table - prof staff salaries per doctoral degree by user selection
  proSalPerDoctoralUserSelectedTable <- selectedData %>%
    dplyr::filter(`Year` %in% yearsToDisplay) %>%
    dplyr::filter(`Institution Name` %in% membersToDisplay) %>%
    # Remove median value as it is not a true entry
    dplyr::filter(! `Institution Name` %in% "MEDIAN") %>%
    { if (nrow(.) == 0) stop("No data available for selected years.") else . } %>%
    dplyr::mutate(expPerDoctoral = MASS::fractions(`Professional salaries & wages`/ `Doctor's degrees awarded`)) %>%
    # Replace INF values with NA
    dplyr::mutate(expPerDoctoral = na_if(expPerDoctoral, "Inf")) %>%
    dplyr::select('Year', 'expPerDoctoral', `Institution Name`) %>%
    dplyr::group_by(`Year`) %>%
    dplyr::top_n(5, expPerDoctoral) %>%
    dplyr::arrange(`Year`, desc(expPerDoctoral)) %>%
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    dplyr::mutate(expPerDoctoral = as.character(expPerDoctoral)) %>%  # Convert to character
    tidyr::pivot_wider(names_from = `Year`, values_from = 'expPerDoctoral') %>%
    kableExtra::kbl() %>%
    kableExtra::kable_paper(lightable_options = "striped")



  return(list(proSalTopPerFaculty = proSalTopPerFaculty,
              proSalTopPerStudent = proSalTopPerStudent,
              proSalTopPerGradStudent = proSalTopPerGradStudent,
              proSalTopPerUndergradStudent = proSalTopPerUndergradStudent,
              proSalTopPerDoctoral = proSalTopPerDoctoral,
              proSalFacultyUserSelected = proSalFacultyUserSelected,
              proSalPerStudentUserSelected = proSalPerStudentUserSelected,
              proSalPerGradStudentUserSelected = proSalPerGradStudentUserSelected,
              proSalPerUndergradStudentUserSelected = proSalPerUndergradStudentUserSelected,
              proSalPerDoctoralUserSelected = proSalPerDoctoralUserSelected,
              proSalTopPerFacultyTable = proSalTopPerFacultyTable,
              proSalTopPerStudentTable = proSalTopPerStudentTable,
              proSalTopPerGradStudentTable = proSalTopPerGradStudentTable,
              proSalTopPerUndergradStudentTable = proSalTopPerUndergradStudentTable,
              proSalTopPerDoctoralTable = proSalTopPerDoctoralTable,
              proSalPerFacultyUserSelectedTable = proSalPerFacultyUserSelectedTable,
              proSalPerStudentUserSelectedTable = proSalPerStudentUserSelectedTable,
              proSalPerGradStudentUserSelectedTable = proSalPerGradStudentUserSelectedTable,
              proSalPerUndergradStudentUserSelectedTable = proSalPerUndergradStudentUserSelectedTable,
              proSalPerDoctoralUserSelectedTable = proSalPerDoctoralUserSelectedTable))
}

# [END]



