context("Checking for Plots to Compare Total Library Expenditures Over Years")
library(libraryStatistics)

test_that("Data generation is as expected", {

  visTotalLibraryExpOutput <- visTotalLibraryExp(dataARL = ARLDataDownload,
                     members = c("Library A", "Library B", "Library C", "Library D", "Library E"),
                     years = c(2020, 2021, 2022))

  expect_type(visTotalLibraryExpOutput, "list")
  expect_length(visTotalLibraryExpOutput, 20)
})


context("Checking for invalid user input")
test_that("Data generate error upon invalid user input", {

  # dataARL provided as a string
  expect_error(visTotalLibraryExp(dataARL = "ARLDataDownload",
                                  members = c("Library A", "Library B", "Library C", "Library D", "Library E"),
                                  years = c(2020, 2021, 2022)))


  # members provided are not in dataset
  expect_error(visTotalLibraryExp(dataARL = ARLDataDownload,
                                  members = c("Library X", "Library Y", "Library Z"),
                                  years = c(2020, 2021, 2022)))

  # years not included in data
  expect_error(visTotalLibraryExp(dataARL = ARLDataDownload,
                                  members = c("Library A", "Library B", "Library C", "Library D", "Library E"),
                                  years = c(2015, 2016, 2017)))


})
