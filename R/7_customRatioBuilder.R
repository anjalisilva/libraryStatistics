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
#'         custom ratio based on user selected numerator
#'         and denominator, over user selected years.
#'   \item customRatioUser - A barplot showing ratio based on user
#'         selected numerator and denominator, for user selected
#'         ARL members, over user selected number of years.
#'   \item customRatioTopTable - A table showing the original values used
#'         for calculating the ratios for ARL members with highest
#'         custom ratio based on user selected numerator
#'         and denominator, over user selected years.
#'   \item customRatioUserTable - A table showing the original values used
#'         for calculating the ratios for user selected numerator and
#'         denominator, for user selected ARL members, over user selected
#'         number of years.
#' }
#'
#' @examples
#' # Reading R package example data
#' # ?ARLDataDownload
#' customRatioBuilder(dataARL = ARLDataDownload,
#'                    numerator = "Electronic books",
#'                    denominator = "Total fulltime students",
#'                    members = c("Library A", "Library B", "Library C", "Library D", "Library E"),
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
#' @import stringr
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
    # filter denominator with zero value to avoid Inf results
    dplyr::filter(`denominator` != 0) %>%
    { if (nrow(.) == 0) stop("No data available for selected years.") else . } %>%
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
    ggplot2::ggtitle(label = paste("Ratio of", stringr::str_to_title(combinedString),"For Top 5 ARL Members Overall")) +
    # subtitle = "ARL rank is shown on top of each bar.") +
    ggplot2::theme(text = element_text(size = 15, color = 'black'),
                   axis.text.x = element_text(angle = 90,
                                              hjust = 1,
                                              vjust = 0.5,
                                              color = 'black', size = 15),
                   axis.text.y = element_text(color = 'black', size = 15)) +
    ggplot2::scale_fill_manual(values = setColorPalette()[-c(1:5)])


  # A table showing the original values used for
  # calculating the ratios for ARL members with highest
  # custom ratio based on user selected numerator
  # and denominator, over user selected years

  customRatioTopTable <- selectedData %>%
    dplyr::select(`Year`, `Institution Name`, `Rank in ARL investment index`, numerator, denominator) %>%
    dplyr::filter(`Year` %in% yearsToDisplay) %>%
    # Remove median value as it is not a true entry
    dplyr::filter(! `Institution Name` %in% "MEDIAN") %>%
    # remove NA values from denominator
    dplyr::filter(!is.na(!!sym(denominator))) %>%
    # remove 0 values from denominator
    dplyr::filter((!!sym(denominator)) != 0) %>%
    # Add an error message if no data is selected based on criteria
    { if (nrow(.) == 0) stop("No data available for selected years.") else . } %>%
    # sym() convert column names to symbols and !! unquote them
    dplyr::mutate(customRatio = MASS::fractions(!!sym(numerator)/!!sym(denominator))) %>%
    dplyr::select('Year', 'customRatio', `Institution Name`) %>%
    dplyr::group_by(`Year`) %>%
    dplyr::top_n(5, customRatio) %>%
    dplyr::arrange(`Year`, desc(customRatio)) %>%
    dplyr::mutate(`Institution Name` = factor(`Institution Name`)) %>%
    dplyr::mutate(customRatio = as.character(customRatio)) %>%  # Convert to character
    tidyr::pivot_wider(names_from = `Year`, values_from = 'customRatio') %>%
    kableExtra::kbl() %>%
    kableExtra::kable_paper(lightable_options = "striped")


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
    ggplot2::ggtitle(label = paste("Ratio of ", stringr::str_to_title(combinedString), "For User Selected Members")) +
    # subtitle = "ARL rank is shown on top of each bar.") +
    ggplot2::theme(text = element_text(size = 15, color = 'black'),
                   axis.text.x = element_text(angle = 90,
                                              hjust = 1,
                                              vjust = 0.5,
                                              color = 'black', size = 15),
                   axis.text.y = element_text(color = 'black', size = 15)) +
    ggplot2::scale_fill_manual(values = setColorPalette())


  # A table showing the original values used for
  # calculating the ratios for ARL members with highest
  # custom ratio based on user selected numerator
  # and denominator, over user selected years

  customRatioUserSelectedTable <- selectedData %>%
    dplyr::filter(`Year` %in% yearsToDisplay) %>%
    dplyr::filter(`Institution Name` %in% membersToDisplay) %>%
    # Remove median value as it is not a true entry
    dplyr::filter(! `Institution Name` %in% "MEDIAN") %>%
    # filter denominator with zero value to avoid Inf results
    dplyr::filter(`Total teaching faculty` != 0) %>%
    # Add an error message if no data is selected based on criteria
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



  checkForDollarSign <- function(numerator, denominator) {
    if (numerator == denominator) {
    ggplot2::scale_y_continuous(labels = scales::dollar_format(),
                                breaks = scales::pretty_breaks(n = 5))
    } else {

      ggplot2::scale_y_continuous(labels = scales::label_comma(),
                                  breaks = scales::pretty_breaks(n = 5)) }}





  return(list(customRatioTop = customRatioTop,
              customRatioUser = customRatioUser))

}
# [END]
