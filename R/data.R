#' Association of Research Libraries (ARL) Data Download Example
#'
#' A demonstration dataset showing the format required to be uploaded, as directly
#' downloaded from ARL Data Portal, without any data preprocessing. The data file must
#' be in a comma-separated value (.csv) format, where rows represent years and
#' columns correspond to ARL indicators (variables). The first column should be
#' labeled 'Year', followed by other indicators in any order, such as
#' 'Institution Name', 'Institution type', etc., as directly downloaded from the ARL Data
#' Portal.
#'
#' @source ARL Data Portal: {https://www.arlstatistics.org/data}
#'
#' @format A dataset with 18 rows and 80 columns.
#' \describe{
#'  The first column should be labeled 'Year', followed by other indicators
#'  in any order, such as 'Institution Name', 'Institution type', etc., as
#'  provided by the ARL Data Portal. In total this example dataset contain data
#'  for 5 institutions, called Library A to Library E across three years
#'  from 2020, 2021 to 2022. The median value is also provided, as it is reflected
#'  in data downloaded from ARL Data Portal.
#' }
#' @examples
#' \dontrun{
#'  ARLDataDownload
#' }
"ARLDataDownload"


# [END]
