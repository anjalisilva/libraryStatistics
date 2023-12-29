

ARLDataDownload <- read_csv("~/Desktop/ARL Data Download.csv")
ARLDataDownload$`Titles held`

visCollectionData <- function(dataARL) {
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
                            "Electronic books")


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

