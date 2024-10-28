#
# ARL_Data_Download_Aug2024 <- read_csv("~/Desktop/ARL Data Download_Aug2024.csv")
# dim(ARL_Data_Download_Aug2024) # 8481   80
# dataARL <- ARL_Data_Download_Aug2024

# 1) Convert other functions so they include tables: 4_ and onward

# 2) for all plots add line "
# { if (nrow(.) == 0) stop("No data available for selected years.") else . } %>%
#" to ensure proper error message if no data available

# Add an error message if no data is selected based on criteria
# { if (nrow(.) == 0) stop("No data available for selected years.") else . } %>%

# 3) Add $ sign to where needed in custom ratio function

# 4) Add text to graphics


# --

# To work on: Line 160 to 168 in 7_customRatioBuilder.R
# Add tables to  functions 4 to 8:
# Add following lines to functions 3 to 8:
#     # filter denominator with zero value to avoid Inf results
#     dplyr::filter(`Total teaching faculty` != 0) %>%
#     # Add an error message if no data is selected based on criteria
#     { if (nrow(.) == 0) stop("No data available for selected years.") else . } %>%
