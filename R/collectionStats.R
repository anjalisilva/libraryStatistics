

ARLDataDownload <- read_csv("~/Desktop/ARL Data Download.csv")
ARLDataDownload$`Titles held`

#' A Bar Plot to Compare Collection Statistics
#'
#' A function to visualize collection statistics
#'
#'@param years A numeric vector specifying up to 5 calendar years
#'  for which data should be plotted, e.g., c(2015, 2016, 2017, 2018, 2019).
#'  If no value is provided (i.e., NA), then most recent five
#'  years available in the data will be used. If more than 5 values
#'  provided, last 5 values will be selected. Default is NA.
#'

visCollectionData <- function(dataARL, institute, years = NA) {

  # Display only 5 years of data
  is.na(years == TRUE) {
    yearsInData <- dataARL$Year %>%
                     unique() %>%
                     sort(decreasing = FALSE)
    # Obtain data for last 5 years
    fiveYears <-
      yearsInData[(length(yearsInData) - 4):length(yearsInData)]
  } else if (is.double(years) != TRUE || is.integer(years) != TRUE) {
    stop("Argument years should be set as a vector of numeric data
         containing 5 years or set to NA")
  } else {
    # If more than 5 years of data present
    if(length(years) > 5) {
      warning("More than five years provided in argument years. Most
              recent 5 years will be used.")
      yearsTrucated <- years %>%
        unique() %>%
        sort(decreasing = FALSE) %>%
        tail(x = 5)
      # Obtain data for last 5 years
      fiveYears <-
        yearsInData[(length(yearsInData) - 4):length(yearsInData)]
    }
  }

  selectedData <- dataARL %>% dplyr::select(
                            "Year",
                            "Institution number",
                            "Institution Name",
                            "Institution type",
                            "Region",
                            "Member year",
                            "Rank in ARL investment index",
                            "ARL investment index value",
                            "Titles held",
                            "Volumes held",
                            "Electronic books") %>%
    dplyr::mutate_at(c('Titles held',
                       'Volumes held',
                       'Electronic books'), as.numeric)
    tibble::as_tibble()

# Comparison within institute
# 1. Over years
# Compare within institute volumes held, vs electronic books
# Institute vs rest average
# Institute vs rest average Canada vs USA

  # set color palette
  colorPaletteCustom <- c(
    '#33a02c',
    '#fee08b',
    '#5e4fa2',
    '#66c2a5',
    '#3288bd',
    '#e6f598',
    '#a6cee3',
    '#c51b7d',
    '#fde0ef',
    '#e31a1c',
    '#cab2d6',
    '#ff7f00',
    '#b15928',
    '#dfc27d',
    '#8dd3c7',
    '#ccebc5',
    '#f1b6da')

  overYearsPlot <-  selectedData %>%
    dplyr::filter(`Institution type` %in% c("Canadian", ".")) %>%
    dplyr::filter(`Institution Name` != "MEDIAN") %>% # remove median value
    dplyr::filter(`Year` %in% c(fiveYearEarly:recentYear)) %>% # Limit to five years
    # width = .75 ensures space between groups
    ggplot2::ggplot(aes(x = reorder(factor(Year), +(`Titles held`)),
                        y = `Titles held`,
                        fill = factor(`Institution Name`),
                        width = .75)) +
    geom_bar(position = "dodge", stat="identity") +
    ggplot2::labs(y = "Titles Held",
                  x = "Year",
                  fill = "Institute",
                  title = "Titles Held") +
    ggplot2::theme_bw() +
    ggplot2::theme(text = element_text(size = 10),
                   axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    # ggbreak::scale_y_break(c(110000, 190000)) +
    ggplot2::scale_fill_manual(values = colorPaletteCustom)



}


visExpenditureData <- function(dataARL) {
  selectedData <- dataARL %>% dplyr::select(
    "Year",
    "Institution number",
    "Institution Name",
    "Institution type",
    "Region",
    "Member year",
    "Rank in ARL investment index",
    "ARL investment index value",
    "Total library expenditures",
    "Total materials expenditures",
    "One-time resource purchases",
    "Ongoing resource purchases",
    "Collection support",
    "Professional salaries & wages",
    "Support staff salaries & wages",
    "Student assistant wages",
    "Total salaries & wages",
    "Other operating expenditures",
    "Fringe benefits, dollar amount",
    "Fringe benefits, official designated percent",
    "External expenditures for bibliographic utilities, networks, etc."
    )


}

