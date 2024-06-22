# customRatioBuilder
#' Compute and Plot Custom Ratio of User's Choice Over Selected Years
#'
#' A function to build and visualize a custom ratio based on user
#' selected numerator and denominator from various statistics reported
#' in the annual survey of Association of Research Libraries (ARL). The
#' ratio can be visualized for user selected ARL members over user selected
#' years, as bar plots.
#'
#'@param dataARL A dataframe containing ARL survey data directly
#'   downloaded from ARL platform. The years should be placed along
#'   rows. The first column must be 'Year', followed by other variables
#'   in no particular order, e.g., 'Institution Name', 'Institution type',
#'   etc. To download data from ARL Data Portal, it is recommended that
#'   all variables are selected, with columns being 'Variables' and data
#'   sorted by 'Institution Name' (default options).
#'@param numerator A character vector specifying a numeric statistic from the
#'   ARL survey data to be used as the numerator. The vector should be
#'   an exact match to the selected column from the input data.
#'   E.g., "Electronic books".
#'@param denominator A character vector specifying a numeric statistic from the
#'   ARL survey data to be used as the numerator. The vector should be
#'   an exact match to the selected column from the input data.
#'   E.g., "Total fulltime students".
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
#' @return Returns bar plots showing varying ratios specified below:
#' \itemize{
#'   \item customRatioTop - A barplot showing members with highest
#'         ratio of custom ratio based on user selected numerator
#'         and denominator, over user selected years.
#'   \item customRatioUser - A barplot showing ratio based on user
#'         selected numerator and denominator, for user selected
#'         ARL members, over user selected number of years.
#' }
#'
#' @examples
#' # Reading R package example data
#' # ?ARLDataDownload
#' customRatioBuilder(dataARL = ARLDataDownload,
#'                    numerator = "Electronic books",
#'                    denominator = "Total fulltime students",
#'                    members = c("Institute A", "Institute B", "Institute C", "Institute D", "Institute E"),
#'                    years = c(2020, 2021, 2022))
#'
#' # Reading actual data downloaded from ARL (not run)
#' # Set file path
#' # ARLData <- readr::read_csv("~/ARLData.csv")
#' # customRatioBuilder(dataARL = ARLData,
#' #                    numerator = "Electronic books",
#' #                    denominator = "Total fulltime students",
#' #                    members = c("BOSTON", "TORONTO", "OTTAWA", "LAVAL", "HARVARD"),
#' #                    years = c(2015, 2016, 2017, 2022, 2018, 2019))
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
customRatioBuilder <- function(dataARL, numerator, denominator, members, years = NA) {

  selectedData <- dataAdjustment(dataARL = dataARL)

  yearsToDisplay <- setYearsToDispaly(years = years,
                                      dataARL = dataARL)

  membersToDisplay <- setMemebersToDispaly(members = members,
                                           dataARL = dataARL)


  # Using support staff count per faculty by top contributors

  combinedString <- paste(paste(numerator, "per"),
                          tolower(denominator), sep = "\n")

  customRatioTop <- selectedData %>%
    dplyr::select(`Year`, `Institution Name`, `Rank in ARL investment index`, numerator, denominator) %>%
    dplyr::filter(`Year` %in% yearsToDisplay) %>%
    # Remove median value as it is not a true entry
    dplyr::filter(! `Institution Name` %in% "MEDIAN") %>%
    # sym() convert column names to symbols and !! unquote them
    dplyr::mutate(customRatio = !!sym(numerator) / !!sym(denominator)) %>%
    # Replace INF values with NA
    dplyr::mutate(customRatio = na_if(customRatio, Inf)) %>%
    dplyr::group_by(`Year`) %>%
    dplyr::top_n(5, customRatio) %>%
    dplyr::arrange(`Year`, desc(customRatio)) %>%
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    ggplot2::ggplot(aes(x = factor(`Year`),
                        y = `customRatio`,
                        fill = factor(`Institution Name`),
                        width = .75)) +
    ggplot2::geom_bar(position = "dodge", stat = "identity") +
    ggplot2::labs(y = combinedString,
                  x = "Year",
                  fill = "ARL Member") +
    ggplot2::theme_bw() +
    ggplot2::ggtitle(label = paste("Ratio of", stringr::str_to_title(combinedString),"\nFor Top 5 ARL Members Overall")) +
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



  # ---
  # Using support staff count per faculty by user selection
  customRatioUser <- selectedData %>%
    dplyr::filter(`Year` %in% yearsToDisplay) %>%
    dplyr::filter(`Institution Name` %in% membersToDisplay) %>%
    # Remove median value as it is not a true entry
    dplyr::filter(! `Institution Name` %in% "MEDIAN") %>%
    # sym() convert column names to symbols and !! unquote them
    dplyr::mutate(customRatio = !!sym(numerator) / !!sym(denominator)) %>%
    # Replace INF values with NA
    dplyr::mutate(customRatio = na_if(customRatio, Inf)) %>%
    dplyr::group_by(`Year`) %>%
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    ggplot2::ggplot(aes(x = factor(`Year`),
                        y = `customRatio`,
                        fill = factor(`Institution Name`),
                        width = .75)) +
    ggplot2::geom_bar(position = "dodge", stat = "identity") +
    ggplot2::labs(y = combinedString,
                  x = "Year",
                  fill = "ARL Member") +
    ggplot2::theme_bw() +
    ggplot2::ggtitle(label = paste("Ratio of ", stringr::str_to_title(combinedString), "\nfor user selected members")) +
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

  return(list(customRatioTop = customRatioTop,
              customRatioUser = customRatioUser))

}
# [END]
