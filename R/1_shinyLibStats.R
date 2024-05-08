#' Launch Shiny App For Package libraryStatistics
#'
#' A function that launches the shiny app for this package. The shiny app is a
#' dashboard that permit to visualize, track trends, and compare up to five years
#' of data downloaded directly from ARL data portal, with no data cleaning involved.
#' Upon uploading of data, the user is directed to select an institute of choice and
#' up to five years of choice. The user can then see the summary of dataset by
#' each year, and visually compare the selected institute against other libraries
#' for indicators defined by ARL. Additionally, useful ratios and percentages,
#' such as library expenditures per faculty, per student, and per doctoral
#' degree awarded, can be visualized and compared.
#'
#' @return Open up the shiny page.
#'
#' @examples
#' \dontrun{
#' libraryStatistics::shinyLibStats()
#' }
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
#' @importFrom shiny runApp
shinyLibStats <- function() {
  appDir <- system.file("shiny-scripts",
                        package = "libraryStatistics")
  shiny::runApp(appDir, display.mode = "normal")
  return()
}
