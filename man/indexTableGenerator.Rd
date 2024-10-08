% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/8_investmentIndex.R
\name{indexTableGenerator}
\alias{indexTableGenerator}
\title{Table to Compare Association of Research Libraries Investment Index Over Years}
\usage{
indexTableGenerator(dataARL, members, years = NA)
}
\arguments{
\item{dataARL}{A dataframe containing ARL survey data directly
downloaded from ARL platform. The years should be placed along
rows. The first column must be 'Year', followed by other variables
in no particular order, e.g., 'Institution Name', 'Institution type',
etc. To download data from ARL Data Portal, it is recommended that
all variables are selected, with columns being 'Variables' and data
sorted by 'Institution Name' (default options).}

\item{members}{A character vector specifying up to five ARL members
of interest, as identified in the dataset. E.g.,
c("BOSTON", "TORONTO", "OTTAWA", "LAVAL", "HARVARD").}

\item{years}{A numeric vector specifying up to 5 calendar years
for which data should be plotted, e.g., c(2015, 2016, 2017,
2018, 2019). If no value is provided (i.e., NA), then most
recent five years available in the uploaded data will be used.
If more than 5 values provided, last 5 values will be selected.
Default is NA.}
}
\value{
Returns a table of class knitr_kable showing Association of
  Research Libraries (ARL) Investment Index for all institutes in the
  uploaded data and for user selected years.
}
\description{
A function to prepare a table containing Association of Research
Libraries (ARL) Investment Index over years as reported in the
annual survey.
}
\examples{
# Reading R package example data
# ?ARLDataDownload
indexTableGenerator(dataARL = ARLDataDownload,
                    members = c("Library A", "Library B", "Library C", "Library D", "Library E"),
                    years = c(2020, 2021, 2022))

# Reading actual data downloaded from ARL (not run)
# Set file path
# ARLData <- readr::read_csv("~/ARLData.csv")
# indexTableGenerator(dataARL = ARLData,
#                     members = c("BOSTON", "TORONTO", "OTTAWA", "LAVAL", "HARVARD"),
#                     years = c(2015, 2016, 2017, 2022, 2018, 2019))

}
\references{
Mian, A., & Gross, H. (2023). ARL Statistics 2022. Washington, DC:
Association of Research Libraries.
\href{https://publications.arl.org/ARL-Statistics-2022/}{Link}

Association of Research Libraries. (2023). ARL Statistics 2023
Instructions. \href{https://www.arlstatistics.org/resources/stats_instructions}{Link}
}
\author{
{Anjali Silva, \email{a.silva@utoronto.ca}, Klara Maidenberg, \email{klara.maidenberg@utoronto.ca}}
}
