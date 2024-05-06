# staffCounts
#' Plots to Compare Library Staff Counts Over Years
#'
#' A function to visualize library professional staff counts, full-time
#' equivalent (FTE), as ratios in comparison to various statistics reported
#' in the annual survey of Association of Research Libraries (ARL) as bar plots.
#'
#'
#'@param dataARL A dataframe containing ARL survey data directly
#'   downloaded from ARL platform. The years should be placed along
#'   rows. The first column must be 'Year', followed by other variables
#'   in no particular order, e.g., 'Institution Name', 'Institution type',
#'   etc. To download data from ARL Data Portal, it is recommended that
#'   all variables are selected, with columns being 'Variables' and data
#'   sorted by 'Institution Name' (default options).
#'@param members A character vector specifying up to five ARL member
#'   institutes of interest, as identified in the dataset. E.g.,
#'   c("BOSTON", "TORONTO", "OTTAWA", "LAVAL", "HARVARD").
#'@param years A numeric vector specifying up to 5 calendar years
#'   for which data should be plotted, e.g., c(2015, 2016, 2017,
#'   2018, 2019). If no value is provided (i.e., NA), then most
#'   recent five years available in the uploaded data will be used.
#'   If more than 5 values provided, last 5 values will be selected.
#'   Default is NA.
#'
#' @return Returns three bar plots showing staff count statistics
#' \itemize{
#'   \item staffFTEUserInstitute - A lineplot comparing user selected
#'         institute over user selected number of years for staff counts.
#'         The median line is provided for comparison.
#' }
#'
#' @examples
#' visStaffCounts(dataARL = ARLDataDownload,
#'                members = c("BOSTON", "TORONTO", "OTTAWA", "LAVAL", "HARVARD"),
#'                years = c(2015, 2016, 2017, 2022, 2018, 2019))
#'
#' @export
#' @importFrom ggplot2 ggplot
#' @import magrittr
visStaffCounts <- function(dataARL, members, years = NA) {

  selectedData <- dataAdjustment(dataARL = dataARL)

  yearsToDisplay <- setYearsToDispaly(years = years,
                                      dataARL = dataARL)

  membersToDisplay <- setMemebersToDispaly(members = members,
                                           dataARL = dataARL)


  # Using professional staff count per faculty by top contributors
  proFTEPerFaculty <- selectedData %>%
    dplyr::filter(`Year` %in% yearsToDisplay) %>%
    # Remove median value as it is not a true entry
    dplyr::filter(! `Institution Name` %in% "MEDIAN") %>%
    dplyr::mutate(proPerFaculty = `Professional staff`/`Total teaching faculty`) %>%
    # Replace INF values with NA
    dplyr::mutate(proPerFaculty = na_if(proPerFaculty, Inf)) %>%
    dplyr::group_by(`Year`) %>%
    dplyr::top_n(5, proPerFaculty) %>%
    dplyr::arrange(`Year`, desc(proPerFaculty))

  proTopPerFaculty <- proFTEPerFaculty %>%
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
    ggplot2::ggtitle(label = "Members with Highest Ratio of Professional Staff\n Count (FTE) Per Teaching Faculty") +
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
  # Using professional staff count per student by top contributors
  proFTEPerStudent <- selectedData %>%
    dplyr::filter(`Year` %in% yearsToDisplay) %>%
    # Remove median value as it is not a true entry
    dplyr::filter(! `Institution Name` %in% "MEDIAN") %>%
    dplyr::mutate(proPerStudent = `Professional staff`/
                    (`Total fulltime students` + `Part-time students, undergraduate and graduate`)) %>%
    # Replace INF values with NA
    dplyr::mutate(proPerStudent = na_if(proPerStudent, Inf)) %>%
    dplyr::group_by(`Year`) %>%
    dplyr::top_n(5, proPerStudent) %>%
    dplyr::arrange(`Year`, desc(proPerStudent))

  proTopPerStudent <- proFTEPerStudent %>%
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
    ggplot2::ggtitle(label = "Members with Highest Ratio of Professional Staff\n Count (FTE) Per Student (FT + PT)") +
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
  # Using professional staff count per graduate student by top contributors
  proFTEPerGradStudent <- selectedData %>%
    dplyr::filter(`Year` %in% yearsToDisplay) %>%
    # Remove median value as it is not a true entry
    dplyr::filter(! `Institution Name` %in% "MEDIAN") %>%
    dplyr::mutate(profPerStudent = `Professional staff`/
                    (`Part-time graduate students` + `Total fulltime graduate students`)) %>%
    # Replace INF values with NA
    dplyr::mutate(profPerStudent = na_if(profPerStudent, Inf)) %>%
    dplyr::group_by(`Year`) %>%
    dplyr::top_n(5, profPerStudent) %>%
    dplyr::arrange(`Year`, desc(profPerStudent))

  proTopPerGradStudent <- proFTEPerGradStudent %>%
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    ggplot2::ggplot(aes(x = factor(`Year`),
                        y = `profPerStudent`,
                        fill = factor(`Institution Name`),
                        width = .75)) +
    ggplot2::geom_bar(position = "dodge", stat = "identity") +
    ggplot2::labs(y = "Professional Staff Count (FTE) \nPer Grad Student",
                  x = "Year",
                  fill = "ARL Member") +
    ggplot2::theme_bw() +
    ggplot2::ggtitle(label = "Members with Highest Ratio of Professional Staff\n Count (FTE) Per Grad Student (FT + PT)") +
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
  # Using professional staff count per doctoral degree by top contributors (not ARL)
  proFTETopPerDoctoral <- selectedData %>%
    dplyr::filter(`Year` %in% yearsToDisplay) %>%
    # Remove median value as it is not a true entry
    dplyr::filter(! `Institution Name` %in% "MEDIAN") %>%
    dplyr::mutate(proPerStudent = `Professional staff`/ `Doctor's degrees awarded`) %>%
    # Replace INF values with NA
    dplyr::mutate(proPerStudent = na_if(proPerStudent, Inf)) %>%
    dplyr::group_by(`Year`) %>%
    dplyr::top_n(5, proPerStudent) %>%
    dplyr::arrange(`Year`, desc(proPerStudent))


  proTopPerDoctoral <- proFTETopPerDoctoral %>%
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    dplyr::filter(! `Institution Name` %in% "MEDIAN") %>%
    ggplot2::ggplot(aes(x = factor(`Year`),
                        y = `proPerStudent`,
                        fill = factor(`Institution Name`),
                        width = .75)) +
    ggplot2::geom_bar(position = "dodge", stat = "identity") +
    ggplot2::labs(y = "Total Library Expenditures\nPer Doctoral Degree",
                  x = "Year",
                  fill = "ARL Member") +
    ggplot2::theme_bw() +
    ggplot2::ggtitle(label = "Members with Highest Ratio of Professional Staff\n Count (FTE) Per Doctoral Degree") +
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
    dplyr::arrange(`Year`, desc(proPerFaculty))

  proTopPerFacultyUser <- proPerFacultyUserSelected %>%
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    ggplot2::ggplot(aes(x = factor(`Year`),
                        y = `proPerFaculty`,
                        fill = factor(`Institution Name`),
                        width = .75)) +
    ggplot2::geom_bar(position = "dodge", stat = "identity") +
    ggplot2::labs(y = "Total Library Expenditures\nPer Teaching Faculty",
                  x = "Year",
                  fill = "ARL Member") +
    ggplot2::theme_bw() +
    ggplot2::ggtitle(label = "Ratio of Total Library Expenditures Per Teaching Faculty\nFor User Selected Members") +
                    # subtitle = "ARL rank is shown on top of each bar.") +
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
  # Using total lib stats per student by user selection
  proPerStudentUserSelected <- selectedData %>%
    dplyr::filter(`Year` %in% yearsToDisplay) %>%
    dplyr::filter(`Institution Name` %in% membersToDisplay) %>%
    # Remove median value as it is not a true entry
    dplyr::filter(! `Institution Name` %in% "MEDIAN") %>%
    dplyr::mutate(expPerStudent = `Total library expenditures`/
                    (`Total fulltime students` + `Part-time students, undergraduate and graduate`)) %>%
    # Replace INF values with NA
    dplyr::mutate(expPerStudent = na_if(expPerStudent, Inf)) %>%
    dplyr::group_by(`Year`) %>%
    dplyr::top_n(5, expPerStudent) %>%
    dplyr::arrange(`Year`, desc(expPerStudent))

  tleTopPerStudentUser <- tlePerStudentUserSelected %>%
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
    ggplot2::ggtitle(label = "Ratio of Total Library Expenditures Per Student (FT + PT)\nFor User Selected Members") +
                    # subtitle = "ARL rank is shown on top of each bar.") +
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
  # Using total lib stats per graduate student by user selection
  tlePerGradStudentUserSelected <- selectedData %>%
    dplyr::filter(`Year` %in% yearsToDisplay) %>%
    dplyr::filter(`Institution Name` %in% membersToDisplay) %>%
    # Remove median value as it is not a true entry
    dplyr::filter(! `Institution Name` %in% "MEDIAN") %>%
    dplyr::mutate(expPerGradStudent = `Total library expenditures`/
                    (`Part-time graduate students` + `Total fulltime graduate students`)) %>%
    # Replace INF values with NA
    dplyr::mutate(expPerGradStudent = na_if(expPerGradStudent, Inf)) %>%
    dplyr::group_by(`Year`) %>%
    dplyr::top_n(5, expPerGradStudent) %>%
    dplyr::arrange(`Year`, desc(expPerGradStudent))

  tleTopPerGradStudentUser <- tlePerGradStudentUserSelected %>%
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
    ggplot2::ggtitle(label = "Ratio of Total Library Expenditures Per Grad Student (FT + PT)\nFor User Selected Members") +
                    # subtitle = "ARL rank is shown on top of each bar.") +
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
  tleTopPerDoctoralUserSelected <- selectedData %>%
    dplyr::filter(`Year` %in% yearsToDisplay) %>%
    dplyr::filter(`Institution Name` %in% membersToDisplay) %>%
    # Remove median value as it is not a true entry
    dplyr::filter(! `Institution Name` %in% "MEDIAN") %>%
    dplyr::mutate(expPerDoctoral = `Total library expenditures`/ `Doctor's degrees awarded`) %>%
    # Replace INF values with NA
    dplyr::mutate(expPerDoctoral = na_if(expPerDoctoral, Inf)) %>%
    dplyr::group_by(`Year`) %>%
    dplyr::top_n(5, expPerDoctoral) %>%
    dplyr::arrange(`Year`, desc(expPerDoctoral))


  tleTopPerDoctoralUser <- tleTopPerDoctoralUserSelected %>%
    dplyr::mutate(expPerStudent = `Total library expenditures`/ `Doctor's degrees awarded`) %>%
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
    ggplot2::ggtitle(label = "Ratio of Total Library Expenditures Per Doctoral Degree\nFor User Selected Members") +
                    # subtitle = "ARL rank is shown on top of each bar.") +
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




}
# [END]
