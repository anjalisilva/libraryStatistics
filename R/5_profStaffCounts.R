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
#'   \item proFTETopPerDoctoral - A barplot showing ARL members with highest
#'         ratio of total library professional staff counts (FTE) per doctoral
#'         degree awarded, over user selected number of years.
#'   \item proPerFacultyUserSelected - A barplot showing ratio of library
#'         professional staff counts (FTE) per teaching faculty for user
#'         selected ARL members, over user selected number of years.
#'   \item proPerStudentUserSelected - A barplot showing ratio of library
#'         professional staff counts (FTE) per student (full-time, FT, and
#'         part-time, PT) for user selected ARL members, over user selected
#'         number of years.
#'   \item proPerGradStudentUserSelected - A barplot showing ratio of total
#'         library professional staff counts (FTE) per graduate student
#'         (full-time, FT, and part-time, PT) for user selected ARL members,
#'         over user selected number of years.
#'   \item proPerDoctoralUserSelected - A barplot showing ratio of total
#'         library expenditures per per doctoral degree awarded for user
#'         selected ARL members, over user selected number of years.
#' }
#'
#' @examples
#' visProfStaffCounts(dataARL = ARLDataDownload,
#'                    members = c("BOSTON", "TORONTO", "OTTAWA", "LAVAL", "HARVARD"),
#'                    years = c(2015, 2016, 2017, 2022, 2018, 2019))
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
    ggplot2::ggtitle(label = "Members with Highest Ratio of Professional Staff\nCount (FTE) Per Teaching Faculty") +
    # subtitle = "ARL rank is shown on top of each bar.") +
    ggplot2::theme(text = element_text(size = 15, color = 'black'),
                   axis.text.x = element_text(angle = 90,
                                              hjust = 1,
                                              vjust = 0.5,
                                              color = 'black', size = 15),
                   axis.text.y = element_text(color = 'black', size = 15)) +
    ggplot2::scale_fill_manual(values = setColorPalette()[-1]) +
    ggplot2::scale_y_continuous(labels = scales::label_comma(),
                                breaks = scales::pretty_breaks(n = 5)) # +
    # Add ranking labels on bars
    # ggplot2::geom_text(aes(label = `Rank in ARL investment index`),
    #                    position = position_dodge(width = 0.9),
    #                    vjust = 0,
    #                    size = 6)


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
    ggplot2::ggtitle(label = "Members with Highest Ratio of Professional Staff\nCount (FTE) Per Student (FT + PT)") +
    # subtitle = "ARL rank is shown on top of each bar.") +
    ggplot2::theme(text = element_text(size = 15, color = 'black'),
                   axis.text.x = element_text(angle = 90,
                                              hjust = 1,
                                              vjust = 0.5,
                                              color = 'black', size = 15),
                   axis.text.y = element_text(color = 'black', size = 15)) +
    ggplot2::scale_fill_manual(values = setColorPalette()[-1]) +
    ggplot2::scale_y_continuous(labels = scales::label_comma(),
                                breaks = scales::pretty_breaks(n = 5)) # +
    # Add ranking labels on bars
    # ggplot2::geom_text(aes(label = `Rank in ARL investment index`),
    #                    position = position_dodge(width = 0.9),
    #                    vjust = 0,
    #                    size = 6)


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
    ggplot2::ggtitle(label = "Members with Highest Ratio of Professional Staff\nCount (FTE) Per Grad Student (FT + PT)") +
    # subtitle = "ARL rank is shown on top of each bar.") +
    ggplot2::theme(text = element_text(size = 15, color = 'black'),
                   axis.text.x = element_text(angle = 90,
                                              hjust = 1,
                                              vjust = 0.5,
                                              color = 'black', size = 15),
                   axis.text.y = element_text(color = 'black', size = 15)) +
    ggplot2::scale_fill_manual(values = setColorPalette()[-1]) +
    ggplot2::scale_y_continuous(labels = scales::label_comma(),
                                breaks = scales::pretty_breaks(n = 5)) # +
    # Add ranking labels on bars
    # ggplot2::geom_text(aes(label = `Rank in ARL investment index`),
    #                    position = position_dodge(width = 0.9),
    #                    vjust = 0,
    #                    size = 6)


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
    ggplot2::ggtitle(label = "Members with Highest Ratio of Professional Staff\nCount (FTE) Per Doctoral Degree") +
    # subtitle = "ARL rank is shown on top of each bar.") +
    ggplot2::theme(text = element_text(size = 15, color = 'black'),
                   axis.text.x = element_text(angle = 90,
                                              hjust = 1,
                                              vjust = 0.5,
                                              color = 'black', size = 15),
                   axis.text.y = element_text(color = 'black', size = 15)) +
    ggplot2::scale_fill_manual(values = setColorPalette()[-1]) +
    ggplot2::scale_y_continuous(labels = scales::label_comma(),
                                breaks = scales::pretty_breaks(n = 5)) # +
    # Add ranking labels on bars
    # ggplot2::geom_text(aes(label = `Rank in ARL investment index`),
    #                    position = position_dodge(width = 0.9),
    #                    vjust = 0,
    #                    size = 6)


  # ---
  # Using professional staff count per faculty by user selection
  proPerFacultyUserSelected <- selectedData %>%
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
    ggplot2::scale_fill_manual(values = setColorPalette()[-1]) +
    ggplot2::scale_y_continuous(labels = scales::label_comma(),
                                breaks = scales::pretty_breaks(n = 5)) # +
    # Add ranking labels on bars
    ggplot2::geom_text(aes(label = `Rank in ARL investment index`),
                       position = position_dodge(width = 0.9),
                       vjust = 0,
                       size = 6)


  # ---
  # Using prof staff stats per student by user selection
  proPerStudentUserSelected <- selectedData %>%
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
    ggplot2::scale_fill_manual(values = setColorPalette()[-1]) +
    ggplot2::scale_y_continuous(labels = scales::label_comma(),
                                breaks = scales::pretty_breaks(n = 5)) +
    # Add ranking labels on bars
    ggplot2::geom_text(aes(label = `Rank in ARL investment index`),
                       position = position_dodge(width = 0.9),
                       vjust = 0,
                       size = 6)


  # ---
  # Using prof staff stats per graduate student by user selection
  proPerGradStudentUserSelected <- selectedData %>%
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
    ggplot2::scale_fill_manual(values = setColorPalette()[-1]) +
    ggplot2::scale_y_continuous(labels = scales::label_comma(),
                                breaks = scales::pretty_breaks(n = 5)) +
    # Add ranking labels on bars
    ggplot2::geom_text(aes(label = `Rank in ARL investment index`),
                       position = position_dodge(width = 0.9),
                       vjust = 0,
                       size = 6)


  # ---
  # Using prof staff stats per doctoral degree by top contributors (not ARL)
  proPerDoctoralUserSelected <- selectedData %>%
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
    ggplot2::scale_fill_manual(values = setColorPalette()[-1]) +
    ggplot2::scale_y_continuous(labels = scales::label_comma(),
                                breaks = scales::pretty_breaks(n = 5)) +
    # Add ranking labels on bars
    ggplot2::geom_text(aes(label = `Rank in ARL investment index`),
                       position = position_dodge(width = 0.9),
                       vjust = 0,
                       size = 6)

  return(list(proFTETopPerFaculty = proFTETopPerFaculty,
              proFTETopPerStudent = proFTETopPerStudent,
              proFTETopPerGradStudent = proFTETopPerGradStudent,
              proFTETopPerDoctoral = proFTETopPerDoctoral,
              proPerFacultyUserSelected = proPerFacultyUserSelected,
              proPerStudentUserSelected = proPerStudentUserSelected,
              proPerGradStudentUserSelected = proPerGradStudentUserSelected,
              proPerDoctoralUserSelected = proPerDoctoralUserSelected))

}
# [END]
