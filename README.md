
<!-- README.md is generated from README.Rmd. Please edit that file -->

# libraryStatistics

An R Package with a Shiny Dashboard for Visualizing and Comparing
Library Statistics Data from Association of Research Libraries

<!-- badges: start -->

[![GitHub
issues](https://img.shields.io/github/issues/anjalisilva/MPLNClust)](https://github.com/anjalisilva/MPLNClust/issues)
[![License](https://img.shields.io/badge/license-MIT-green)](./LICENSE)
![GitHub language
count](https://img.shields.io/github/languages/count/anjalisilva/MPLNClust)
![GitHub commit activity
(branch)](https://img.shields.io/github/commit-activity/y/anjalisilva/MPLNClust/master)

<!-- https://shields.io/category/license -->
<!-- badges: end -->

## Description

`libraryStatistics` is an R package with a Shiny dashboard that permit
to visualize and compare data from the annual survey of Association of
Research Libraries (ARL; URL: www.arlstatistics.org/data/main). ARL data
describes the collections, staffing, expenditures, and service
activities of the ARL member libraries in the United States and Canada.
This R package is designed for the analysis and visualization of library
statistics published from the annual survey. The Shiny application
enables the generation of various statistical ratios for comparative
analysis. Through the interactive interface of the application, users
can dynamically visualize ratios by selecting ARL member libraries,
years, and also by creating custom ratios, which enhances the usability
of the published data. Library statistics data published from the annual
survey may be downloaded from ARL Data Portal for any number of years
and across any number of ARL member libraries. However, at one time,
both the R package and Shiny application would only enable to perform
analysis on 5 ARL member libraries and 5 years. The `libraryStatistics`
package was developed using `R version 4.3.2 (2023-10-31)`,
`Platform: x86_64-apple-darwin20 (64-bit)` and
`Running under: macOS Ventura 13.2`.

## Installation

To install the latest version of the package:

``` r
require("devtools")
devtools::install_github("anjalisilva/libraryStatistics", build_vignettes = TRUE)
library("libraryStatistics")
```

To run the Shiny app:

``` r
libraryStatistics::shinyLibStats()
```

## Overview

To list all functions available in the package:

``` r
ls("package:libraryStatistics")
data(package = "libraryStatistics")
browseVignettes("libraryStatistics>")
```

`libraryStatistics` contains 7 functions.

1.  ***shinyLibStats*** opens the Shiny application with visual
    analytics and comparison of upto to 5 years of data from the annual
    survey of ARL uploaded by the user.
2.  ***visTotalLibraryExp*** visualize total library expenditures in
    United States Dollars (USD) as ratios in comparison to various
    statistics reported in the annual survey of ARL as bar plots. This
    is question 6 on ARL survey as the numerator.
3.  ***visTotalLibMaterialsExp*** visualize total library materials
    expenditures in United States Dollars (USD) as ratios in comparison
    to various statistics reported in the annual survey of ARL as bar
    plots. This is question 7 on ARL survey as the numerator.
4.  ***visProfStaffSalaries*** visualize salaries of professional
    library staff in United States Dollars (USD), as ratios in
    comparison to various statistics reported in the annual survey of
    ARL as bar plots. This is question 8a on ARL survey as the
    numerator.
5.  ***visProfStaffCounts*** visualize library professional staff
    counts, full-time equivalent (FTE), as ratios in comparison to
    various statistics reported in the annual survey of ARL as bar
    plots. This is question 13a on ARL survey as the numerator.
6.  ***visSupStaffCounts*** visualize library support staff counts,
    full-time equivalent (FTE), as ratios in comparison to various
    statistics reported in the annual survey of ARL as bar plots. This
    is question 13b on ARL survey as the numerator.
7.  ***customRatioBuilder*** build and visualize a custom ratio based on
    user selected numerator and denominator from various statistics
    reported in the annual survey of ARL.
8.  ***indexTableGenerator*** build a table containing Association of
    Research Libraries (ARL) Investment Index over years as reported in
    the annual survey.

An overview of the package is illustrated below:

<div style="text-align:center">

<img src="inst/extdata/pipelineLS.png" width="800" height="450"/>

<div style="text-align:left">
<div style="text-align:left">

## Details

The R package and Shiny application permit to visualize, track trends,
and compare data downloaded directly from ARL data portal
(((www.arlstatistics.org/data/main))), up to 5 ARL member libraries and
5 years at a time, with no data cleaning involved. The R package is
designed for the analysis of library statistics published from the
annual survey conducted by the ARL. The R package contain functions that
permit the user to read in downloaded data downloaded from the ARL Data
Portal and perform various analyses. The input data file should be in
comma-separated value (.csv) format, with rows corresponding to years
and columns representing ARL indicators (variables). The first column
must be labeled ‘Year’, followed by other indicators in any order, such
as ‘Institution Name’, ‘Institution type’, etc., as directly downloaded
from the ARL Data Portal.

For the Shiny application, the user is able to upload data and navigate
through the different tabs of the application to generate various
statistical ratios for comparative analyses. Through the interactive
interface of the Shiny application, users can dynamically visualize
ratios by selecting ARL member libraries, years, and also by creating
custom ratios, which enhances the usability of the published data.

An example of an ratio is library expenditures per faculty, per student,
and per doctoral degree awarded, can be visualized and compared across
libraries. Overall, the tool enhances the utilization of ARL collected
data in making evidence-based decisions within UTL and other libraries,
to gain insights into the multifaceted ways in which library resources
contribute to its community, to support research and scholarship
endeavors.

## Shiny App

The Shiny app employing ***libraryStatistics*** could be run and results
could be visualized:

``` r
libraryStatistics::shinyLibStats()
```

<div style="text-align:center">

<img src="inst/extdata/shinyInterface.png" alt="ShinyApp1" width="650" height="400"/>

<div style="text-align:left">
<div style="text-align:left">

In simple, the ***shinyLibStats*** is a web applications available with
`libraryStatistics`.

## Tutorials

For tutorials and plot interpretation, refer to the vignette:

``` r
browseVignettes("libraryStatistics")
```

## Citation for Package

``` r
citation("libraryStatistics")
```

Silva, A. and K. Maidenberg (2024). libraryStatistics: An R Package with
a Shiny Dashboard for Visualizing and Comparing Library Statistics Data
from Association of Research Libraries. Unpublished.

``` r
A BibTeX entry for LaTeX users is

  @misc{,
    title = {libraryStatistics: An R Package with a Shiny Dashboard for Visualizing and Comparing Library Statistics Data from Association of Research Libraries},
    author = {A. Silva and K. Maidenberg},
    year = {2024},
    url = {https://github.com/anjalisilva/libraryStatistics},
  }
```

## Package References

- R Core Team (2023). R: A language and environment for statistical
  computing. R Foundation for Statistical Computing, Vienna, Austria.
  <https://www.R-project.org/>

- [Mian, A., & Gross, H. (2023). ARL Statistics 2022. Washington, DC:
  Association of Research
  Libraries.](https://publications.arl.org/ARL-Statistics-2022/)

- [Association of Research Libraries. (2023). ARL Statistics 2023
  Instructions.](https://www.arlstatistics.org/resources/stats_instructions)

## Maintainer

- Anjali Silva (<a.silva@utoronto.ca>).

## Contributions

`libraryStatistics` welcomes issues, enhancement requests, and other
contributions. To submit an issue, use the [GitHub
issues](https://github.com/anjalisilva/libraryStatistics).

## Acknowledgments

- Access to ARL annual survey data provided by University of Toronto
  Libraries, Ontario, Canada. We wish to thank Dr. Kevin Borden and
  Holly Gross for their feedback while developing this work.
