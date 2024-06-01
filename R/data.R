#' Association of Research Libraries (ARL) Data Download Example
#'
#' A demonstration dataset showing the format required to be uploaded, as directly
#' downloaded from ARL Data Portal without any preprocessing. The data file must
#' be in a comma-separated value (.csv) format, where rows represent years and
#' columns correspond to ARL indicators (variables). The first column should be
#' labeled 'Year', followed by other indicators in any order, such as
#' 'Institution Name', 'Institution type', etc., as provided by the ARL Data
#' Portal.
#'
#' @source ARL Data Portal {https://www.arlstatistics.org/data}
#'
#' @format A dataset with 80 columns.
#' \describe{
#'  The first column should belabeled 'Year', followed by other indicators
#'  in any order, such as 'Institution Name', 'Institution type', etc., as
#'  provided by the ARL Data Portal.
#' }
#' @examples
#' \dontrun{
#'  ARLDataDownload
#' }
"ARLDataDownload"


# [END]
