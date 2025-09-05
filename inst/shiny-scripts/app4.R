# Actual with tabs

library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Custom Background with Tabs"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Instructions", tabName = "Instructions", icon = icon("dashboard")),
      menuItem("Charts", tabName = "charts", icon = icon("bar-chart")),
      menuItem("Tables", tabName = "tables", icon = icon("table"))
    )
  ),
  dashboardBody(
    # Custom CSS to change background to white
    tags$style(HTML("
      .content-wrapper, .right-side {
        background-color: white !important;
      }
    ")),

    tabItems(
      tabItem(tabName = "Instructions",
              h2("Instructions", align = "center"),
              br(),
              h3("Welcome to libraryStatistics Shiny application.", align = "center"),
              h3("This app is part of the libraryStatistics R package.", align = "center"),
              br(),
              h4("What is the libraryStatistics Shiny app?"),
              h5("The libraryStatistics Shiny application is a tool developed as part of the
                                      libraryStatistics R package. This R package is designed for the analysis and
                                      visualization of library statistics published from the annual survey conducted
                                      by the Association of Research Libraries (ARL). The Shiny application enables the
                                      generation of various statistical ratios for comparative analysis. Through the
                                      interactive interface of the application, users can dynamically visualize ratios
                                      by selecting ARL member libraries, years, and also by creating custom ratios, which
                                      enhances the usability of the published data.

                                      Library statistics data published from the annual survey may be downloaded from ARL Data
                                      Portal for any number of years and across any number of ARL member libraries. However,
                                      at one time, both the R package and Shiny application would only enable to perform analysis on
                                      5 ARL member libraries and 5 years."),
              br(),
              h4("How to use the libraryStatistics Shiny app?"),
              h5("1. Data Preparation: Begin by downloading the dataset from the ARL Data
                                      Portal (www.arlstatistics.org/data/main). Ensure that all variables are selected, with columns set to 'Variables'
                                      and the data sorted by 'Institution Name' (default options). Data should be
                                      downloaded in comma-separated value (.csv) format. Data may be downloaded for
                                      any number of years and across all member institutions available."),
              h5("2. Check Data: The downloaded dataset should have rows corresponding to years
                                      and columns to ARL indicators (variables). The first column must be 'Year',
                                      followed by other indicators in any order, such as 'Institution Name',
                                      'Institution type', etc., as downloaded directly from the ARL Data Portal."),
              h5("3. Uploading Data and Parameter Selection: Upload the dataset (.csv format)
                                      to the Shiny application. After uploading the dataset, a list of choices for
                                      'ARL Member Libraries' and 'Years' based on the uploaded dataset will appear. You may
                                      select up to 5 ARL member libraries and up to 5 years for analysis."),
              h5("4. Exploring Results: Navigate the tabs on the right side at the top of the
                                      application to explore the results. The left panel will remain static, allowing
                                      user to modify the selections for ARL member libraries or years, as needed. Changes
                                      to selections will automatically update the results displayed in the various
                                      tabs on the right."),
              shiny::img(src = 'pipelineLS.png', align = "centre", height = "85%", width = "85%"),
              br(),
              h4("Uncertain about what type of data to upload?"),
              h5("Data should be sourced directly from the ARL Data Portal without any preprocessing.
                                      The file must be in a comma-separated value (.csv) format, where rows represent
                                      years and columns correspond to ARL indicators (variables). The first column
                                      should be labeled 'Year', followed by other indicators in any order, such as
                                      'Institution Name', 'Institution type', etc., as provided by the ARL Data
                                      Portal. The following file can be used as a demonstration dataset to
                                      understand the required format."),
              actionButton(inputId = "data1",
                           label = "Demonstration Dataset for Testing"),
              br(),
              br(),
              h4("How to cite this work?"),
              h5("Silva, A. and K. Maidenberg (2024). libraryStatistics: An R Package with a Shiny Dashboard for Visualizing
                                   and Comparing Library Statistics Data from Association of Research Libraries. Unpublished."),
              h5("A BibTeX entry for LaTeX users is: "),
              h5("
                                     @misc{,
                                       title = {libraryStatistics: An R Package with a Shiny Dashboard for Visualizing and Comparing Library Statistics Data from Association of Research Libraries},
                                       author = {A. Silva and K. Maidenberg},
                                       year = {2024},
                                       url = {https://github.com/anjalisilva/libraryStatistics},}"),
              br(),
              br(),
      ),


      tabItem(tabName = "tables",
              # Sidebar layout with input and output definitions ----
              sidebarLayout(

                # Sidebar panel for inputs ----
                sidebarPanel(width = 3,

                             tags$p("Refer to the 'Instructions' tab located on the right side for detailed guidance."),

                             # br() element to introduce extra vertical spacing ----
                             br(),
                             br(),
                             # input
                             shinyalert::useShinyalert(),  # Set up shinyalert
                             uiOutput("tab2"),
                             fileInput(inputId = "file1",
                                       label = "1. Dataset: upload a dataset for analysis. The file should be
                           in comma-separated value (.csv) format, with rows corresponding to
                           years and columns representing ARL indicators (variables). The first
                           column must be labeled 'Year', followed by other indicators in any
                           order, such as 'Institution Name', 'Institution type', etc., as
                           directly downloaded from the ARL Data Portal.",
                                       accept = c(".csv")),
                             checkboxGroupInput(inputId = "instituteInput",
                                                label = "2. ARL Member Libraries: select up to five members. If
                                    more than five members are selected, only the last five
                                    will be retained. Subsequently, make the selections for
                                    the desired years from the option below."),
                             br(),
                             checkboxGroupInput(inputId = "yearsInput",
                                                label = "3. Years: select up to five years. If more than five
                                    years are selected, the most recent five years will be
                                    retained. Subsequently, navigate through the tabs to the right."),

                             # br() element to introduce extra vertical spacing ----
                             br(),

                             # actionButton
                             # actionButton(inputId = "button2",
                             #             label = "Analyze"),

                             # br() element to introduce extra vertical spacing -
                             br(),

                ), # End of side pannel


                # Main panel for displaying outputs
                mainPanel(width = 9,

                          # Output: Tabset
                          tabsetPanel(type = "tabs",
                                      # Adding space between columns of tables created
                                      tags$head(
                                        tags$style(HTML("
                                          .lightable-paper th, .lightable-paper td {
                                            padding-left: 20px;
                                            padding-right: 20px;
                                          }
                                   "))),

                                      tabPanel("Investment Index",
                                               h3("Investment Index Historical Table", align = "center"),
                                               br(),
                                               htmlOutput("indexTableGen")),
                                      tabPanel("Total Library Expenditures",
                                               h3("Total Library Expenditures (USD) Ratios", align = "center"),
                                               br(),
                                               h4("Total library expenditures in United States Dollars (USD) as ratios in comparison to various statistics
                                       reported in the annual survey of ARL. The chart title identifies the ratio being shown. Ratios for ARL member
                                       libraries selected by the user are shown on left. Plot on the right shows top 5 ARL member libraries
                                       with the highest ratio. If no plot is produced or missing bars, the selected library/libraries may not have submitted
                                       a data point for the given year."),
                                               br(),
                                               br(),
                                               fluidRow(
                                                 splitLayout(cellWidths = c("50%", "50%"), plotOutput("tlePerFacultyUser"), plotOutput("tleTopPerFaculty")),
                                                 splitLayout(cellWidths = c("50%", "50%"),
                                                             actionButton("tlePerFacultyUserToggle", "Show Table"),
                                                             actionButton("tleTopPerFacultyToggle", "Show Table")),
                                                 splitLayout(cellWidths = c("50%", "50%"),
                                                             htmlOutput("tlePerFacultyUserSelectedTable"),
                                                             htmlOutput("tleTopPerFacultyTable")),

                                                 splitLayout(cellWidths = c("50%", "50%"), plotOutput("tlePerStudentUser"), plotOutput("tleTopPerStudent")),
                                                 splitLayout(cellWidths = c("50%", "50%"),
                                                             actionButton("tlePerStudentUserToggle", "Show Table"),
                                                             actionButton("tleTopPerStudentToggle", "Show Table")),
                                                 splitLayout(cellWidths = c("50%", "50%"),
                                                             htmlOutput("tlePerStudentUserSelectedTable"),
                                                             htmlOutput("tleTopPerStudentTable")),

                                                 splitLayout(cellWidths = c("50%", "50%"), plotOutput("tlePerGradStudentUser"), plotOutput("tleTopPerGradStudent")),
                                                 splitLayout(cellWidths = c("50%", "50%"),
                                                             actionButton("tlePerGradStudentUserToggle", "Show Table"),
                                                             actionButton("tleTopPerGradStudentToggle", "Show Table")),
                                                 splitLayout(cellWidths = c("50%", "50%"),
                                                             htmlOutput("tlePerGradStudentUserSelectedTable"),
                                                             htmlOutput("tleTopPerGradStudentTable")),

                                                 splitLayout(cellWidths = c("50%", "50%"), plotOutput("tlePerUndergradStudentUser"), plotOutput("tleTopPerUndergradStudent")),
                                                 splitLayout(cellWidths = c("50%", "50%"),
                                                             actionButton("tlePerUndergradStudentUserToggle", "Show Table"),
                                                             actionButton("tleTopPerUndergradStudentToggle", "Show Table")),
                                                 splitLayout(cellWidths = c("50%", "50%"),
                                                             htmlOutput("tlePerUndergradStudentUserSelectedTable"),
                                                             htmlOutput("tleTopPerUndergradStudentTable")),

                                                 splitLayout(cellWidths = c("50%", "50%"), plotOutput("tlePerDoctoralUser"), plotOutput("tleTopPerDoctoral")),
                                                 splitLayout(cellWidths = c("50%", "50%"),
                                                             actionButton("tlePerDoctoralUserToggle", "Show Table"),
                                                             actionButton("tleTopPerDoctoralToggle", "Show Table")),
                                                 splitLayout(cellWidths = c("50%", "50%"),
                                                             htmlOutput("tlePerDoctoralUserSelectedTable"),
                                                             htmlOutput("tleTopPerDoctoralTable")),

                                                 # splitLayout(cellWidths = c("50%", "50%"), plotOutput("tlePerFacultyUser"), plotOutput("tleTopPerFaculty")),
                                                 # splitLayout(cellWidths = c("50%", "50%"), plotOutput("tlePerStudentUser"), plotOutput("tleTopPerStudent")),
                                                 # splitLayout(cellWidths = c("50%", "50%"), plotOutput("tlePerGradStudentUser"), plotOutput("tleTopPerGradStudent")),
                                                 # splitLayout(cellWidths = c("50%", "50%"), plotOutput("tlePerUndergradStudent"), plotOutput("tleTopPerUndergradStudent")),
                                                 # splitLayout(cellWidths = c("50%", "50%"), plotOutput("tlePerDoctoralUser"), plotOutput("tleTopPerDoctoral")),


                                               )),
                                      tabPanel("Total Library Materials Expenditures",
                                               h3("Total Library Materials Expenditures (USD) Ratios", align = "center"),
                                               br(),
                                               h4("Total library materials expenditures in United States Dollars (USD) as ratios in comparison to various statistics
                                       reported in the annual survey of ARL. The chart title identifies the ratio being shown. Ratios for ARL member
                                       libraries selected by the user are shown on left. Plot on the right shows top 5 ARL member libraries
                                       with the highest ratio. If no plot is produced or missing bars, the selected library/libraries may not have submitted
                                       a data point for the given year."),
                                               br(),
                                               br(),
                                               fluidRow(
                                                 splitLayout(cellWidths = c("50%", "50%"), plotOutput("tlmePerFacultyUserSelected"), plotOutput("tlmeTopPerFaculty")),
                                                 splitLayout(cellWidths = c("50%", "50%"),
                                                             actionButton("tlmePerFacultyUserToggle", "Show Table"),
                                                             actionButton("tlmeTopPerFacultyToggle", "Show Table")),
                                                 splitLayout(cellWidths = c("50%", "50%"),
                                                             htmlOutput("tlmePerFacultyUserSelectedTable"),
                                                             htmlOutput("tlmeTopPerFacultyTable")),

                                                 splitLayout(cellWidths = c("50%", "50%"), plotOutput("tlmePerStudentUserSelected"), plotOutput("tlmeTopPerStudent")),
                                                 splitLayout(cellWidths = c("50%", "50%"),
                                                             actionButton("tlmePerStudentUserToggle", "Show Table"),
                                                             actionButton("tlmeTopPerStudentToggle", "Show Table")),
                                                 splitLayout(cellWidths = c("50%", "50%"),
                                                             htmlOutput("tlmePerStudentUserSelectedTable"),
                                                             htmlOutput("tlmeTopPerStudentTable")),

                                                 splitLayout(cellWidths = c("50%", "50%"), plotOutput("tlmePerGradStudentUserSelected"), plotOutput("tlmeTopPerGradStudent")),
                                                 splitLayout(cellWidths = c("50%", "50%"),
                                                             actionButton("tlmePerGradStudentUserToggle", "Show Table"),
                                                             actionButton("tlmeTopPerGradStudentToggle", "Show Table")),
                                                 splitLayout(cellWidths = c("50%", "50%"),
                                                             htmlOutput("tlmePerGradStudentUserSelectedTable"),
                                                             htmlOutput("tlmeTopPerGradStudentTable")),

                                                 splitLayout(cellWidths = c("50%", "50%"), plotOutput("tlmePerUndergradStudentUserSelected"), plotOutput("tlmeTopPerUndergradStudent")),
                                                 splitLayout(cellWidths = c("50%", "50%"),
                                                             actionButton("tlmePerUndergradStudentUserToggle", "Show Table"),
                                                             actionButton("tlmeTopPerUndergradStudentToggle", "Show Table")),
                                                 splitLayout(cellWidths = c("50%", "50%"),
                                                             htmlOutput("tlmePerUndergradStudentUserSelectedTable"),
                                                             htmlOutput("tlmeTopPerUndergradStudentTable")),

                                                 splitLayout(cellWidths = c("50%", "50%"), plotOutput("tlmeTopPerDoctoralUserSelected"), plotOutput("tlmeTopPerDoctoral")),
                                                 splitLayout(cellWidths = c("50%", "50%"),
                                                             actionButton("tlmePerDoctoralUserToggle", "Show Table"),
                                                             actionButton("tlmeTopPerDoctoralToggle", "Show Table")),
                                                 splitLayout(cellWidths = c("50%", "50%"),
                                                             htmlOutput("tlmePerDoctoralUserSelectedTable"),
                                                             htmlOutput("tlmeTopPerDoctoralTable")),


                                                 # splitLayout(cellWidths = c("50%", "50%"), plotOutput("tlmePerFacultyUserSelected"), plotOutput("tlmeTopPerFaculty")),
                                                 # splitLayout(cellWidths = c("50%", "50%"), plotOutput("tlmePerStudentUserSelected"), plotOutput("tlmeTopPerStudent")),
                                                 # splitLayout(cellWidths = c("50%", "50%"), plotOutput("tlmePerGradStudentUserSelected"), plotOutput("tlmeTopPerGradStudent")),
                                                 # splitLayout(cellWidths = c("50%", "50%"), plotOutput("tlmePerUndergradStudentUserSelected"), plotOutput("tlmeTopPerUndergradStudent")),
                                                 # splitLayout(cellWidths = c("50%", "50%"), plotOutput("tlmeTopPerDoctoralUserSelected"), plotOutput("tlmeTopPerDoctoral")),
                                               )),
                                      tabPanel("Professional Staff Salaries",
                                               h3("Professional Staff Salaries (USD) Ratios", align = "center"),
                                               br(),
                                               h4("Professional staff salaries in United States Dollars (USD) as ratios in comparison to various statistics
                                       reported in the annual survey of ARL. The chart title identifies the ratio being shown. Ratios for ARL member
                                       libraries selected by the user are shown on left. Plot on the right shows top 5 ARL member libraries
                                       with the highest ratio. If no plot is produced or missing bars, the selected library/libraries may not have submitted
                                       a data point for the given year."),
                                               br(),
                                               br(),
                                               fluidRow(
                                                 splitLayout(cellWidths = c("50%", "50%"), plotOutput("proSalFacultyUserSelected"), plotOutput("proSalTopPerFaculty")),
                                                 splitLayout(cellWidths = c("50%", "50%"),
                                                             actionButton("proSalPerFacultyUserToggle", "Show Table"),
                                                             actionButton("proSalTopPerFacultyToggle", "Show Table")),
                                                 splitLayout(cellWidths = c("50%", "50%"),
                                                             htmlOutput("proSalPerFacultyUserSelectedTable"),
                                                             htmlOutput("proSalTopPerFacultyTable")),

                                                 splitLayout(cellWidths = c("50%", "50%"), plotOutput("proSalPerStudentUserSelected"), plotOutput("proSalTopPerStudent")),
                                                 splitLayout(cellWidths = c("50%", "50%"),
                                                             actionButton("proSalPerStudentUserToggle", "Show Table"),
                                                             actionButton("proSalTopPerStudentToggle", "Show Table")),
                                                 splitLayout(cellWidths = c("50%", "50%"),
                                                             htmlOutput("proSalPerStudentUserSelectedTable"),
                                                             htmlOutput("proSalTopPerStudentTable")),

                                                 splitLayout(cellWidths = c("50%", "50%"), plotOutput("proSalPerGradStudentUserSelected"), plotOutput("proSalTopPerGradStudent")),
                                                 splitLayout(cellWidths = c("50%", "50%"),
                                                             actionButton("proSalPerGradStudentUserToggle", "Show Table"),
                                                             actionButton("proSalTopPerGradStudentToggle", "Show Table")),
                                                 splitLayout(cellWidths = c("50%", "50%"),
                                                             htmlOutput("proSalPerGradStudentUserSelectedTable"),
                                                             htmlOutput("proSalTopPerGradStudentTable")),

                                                 splitLayout(cellWidths = c("50%", "50%"), plotOutput("proSalPerUndergradStudentUserSelected"), plotOutput("proSalTopPerUndergradStudent")),
                                                 splitLayout(cellWidths = c("50%", "50%"),
                                                             actionButton("proSalPerUndergradStudentUserToggle", "Show Table"),
                                                             actionButton("proSalTopPerUndergradStudentToggle", "Show Table")),
                                                 splitLayout(cellWidths = c("50%", "50%"),
                                                             htmlOutput("proSalPerUndergradStudentUserSelectedTable"),
                                                             htmlOutput("proSalTopPerUndergradStudentTable")),

                                                 splitLayout(cellWidths = c("50%", "50%"), plotOutput("proSalPerDoctoralUserSelected"), plotOutput("proSalTopPerDoctoral")),
                                                 splitLayout(cellWidths = c("50%", "50%"),
                                                             actionButton("proSalPerDoctoralUserToggle", "Show Table"),
                                                             actionButton("proSalTopPerDoctoralToggle", "Show Table")),
                                                 splitLayout(cellWidths = c("50%", "50%"),
                                                             htmlOutput("proSalPerDoctoralUserSelectedTable"),
                                                             htmlOutput("proSalTopPerDoctoralTable")),

                                               )),
                                      tabPanel("Professional Staff Counts",
                                               h3("Professional Library Staff Counts Full-time Equivalent (FTE) Ratios", align = "center"),
                                               br(),
                                               h4("Professional Library Staff Counts FTE as ratios in comparison to various statistics
                                       reported in the annual survey of ARL. The chart title identifies the ratio being shown. Ratios for ARL member
                                       libraries selected by the user are shown on left. Plot on the right shows top 5 ARL member libraries
                                       with the highest ratio. If no plot is produced or missing bars, the selected library/libraries may not have submitted
                                       a data point for the given year."),
                                               br(),
                                               br(),
                                               fluidRow(
                                                 splitLayout(cellWidths = c("50%", "50%"), plotOutput("proFTEPerFacultyUserSelected"), plotOutput("proFTETopPerFaculty")),
                                                 splitLayout(cellWidths = c("50%", "50%"),
                                                             actionButton("proFTEPerFacultyUserToggle", "Show Table"),
                                                             actionButton("proFTETopPerFacultyToggle", "Show Table")),
                                                 splitLayout(cellWidths = c("50%", "50%"),
                                                             htmlOutput("proFTEPerFacultyUserSelectedTable"),
                                                             htmlOutput("proFTETopPerFacultyTable")),

                                                 splitLayout(cellWidths = c("50%", "50%"), plotOutput("proFTEPerStudentUserSelected"), plotOutput("proFTETopPerStudent")),
                                                 splitLayout(cellWidths = c("50%", "50%"),
                                                             actionButton("proFTEPerStudentUserToggle", "Show Table"),
                                                             actionButton("proFTETopPerStudentToggle", "Show Table")),
                                                 splitLayout(cellWidths = c("50%", "50%"),
                                                             htmlOutput("proFTEPerStudentUserSelectedTable"),
                                                             htmlOutput("proFTETopPerStudentTable")),

                                                 splitLayout(cellWidths = c("50%", "50%"), plotOutput("proFTEPerGradStudentUserSelected"), plotOutput("proFTETopPerGradStudent")),
                                                 splitLayout(cellWidths = c("50%", "50%"),
                                                             actionButton("proFTEPerGradStudentUserToggle", "Show Table"),
                                                             actionButton("proFTETopPerGradStudentToggle", "Show Table")),
                                                 splitLayout(cellWidths = c("50%", "50%"),
                                                             htmlOutput("proFTEPerGradStudentUserSelectedTable"),
                                                             htmlOutput("proFTETopPerGradStudentTable")),

                                                 splitLayout(cellWidths = c("50%", "50%"), plotOutput("proFTEPerUndergradStudentUserSelected"), plotOutput("proFTETopPerUndergradStudent")),
                                                 splitLayout(cellWidths = c("50%", "50%"),
                                                             actionButton("proFTEPerUndergradStudentUserToggle", "Show Table"),
                                                             actionButton("proFTETopPerUndergradStudentToggle", "Show Table")),
                                                 splitLayout(cellWidths = c("50%", "50%"),
                                                             htmlOutput("proFTEPerUndergradStudentUserSelectedTable"),
                                                             htmlOutput("proFTETopPerUndergradStudentTable")),

                                                 splitLayout(cellWidths = c("50%", "50%"), plotOutput("proFTEPerDoctoralUserSelected"), plotOutput("proFTETopPerDoctoral")),
                                                 splitLayout(cellWidths = c("50%", "50%"),
                                                             actionButton("proFTEPerDoctoralUserToggle", "Show Table"),
                                                             actionButton("proFTETopPerDoctoralToggle", "Show Table")),
                                                 splitLayout(cellWidths = c("50%", "50%"),
                                                             htmlOutput("proFTEPerDoctoralUserSelectedTable"),
                                                             htmlOutput("proFTETopPerDoctoralTable")),
                                               )),
                                      tabPanel("Support Staff Counts",
                                               h3("Support Library Staff Counts Full-time Equivalent (FTE) Ratios", align = "center"),
                                               br(),
                                               h4("Support Library Staff Counts FTE as ratios in comparison to various statistics
                                       reported in the annual survey of ARL. The chart title identifies the ratio being shown. Ratios for ARL member
                                       libraries selected by the user are shown on left. Plot on the right shows top 5 ARL member libraries
                                       with the highest ratio. If no plot is produced or missing bars, the selected library/libraries may not have submitted
                                       a data point for the given year."),
                                               br(),
                                               br(),
                                               fluidRow(
                                                 splitLayout(cellWidths = c("50%", "50%"), plotOutput("supPerFacultyUserSelected"), plotOutput("supFTETopPerFaculty")),
                                                 splitLayout(cellWidths = c("50%", "50%"),
                                                             actionButton("supPerPerFacultyUserToggle", "Show Table"),
                                                             actionButton("supPerTopPerFacultyToggle", "Show Table")),
                                                 splitLayout(cellWidths = c("50%", "50%"),
                                                             htmlOutput("supPerPerFacultyUserSelectedTable"),
                                                             htmlOutput("supPerTopPerFacultyTable")),

                                                 splitLayout(cellWidths = c("50%", "50%"), plotOutput("supPerStudentUserSelected"), plotOutput("supFTETopPerStudent")),
                                                 splitLayout(cellWidths = c("50%", "50%"),
                                                             actionButton("supPerPerStudentUserToggle", "Show Table"),
                                                             actionButton("supPerTopPerStudentToggle", "Show Table")),
                                                 splitLayout(cellWidths = c("50%", "50%"),
                                                             htmlOutput("supPerPerStudentUserSelectedTable"),
                                                             htmlOutput("supPerTopPerStudentTable")),

                                                 splitLayout(cellWidths = c("50%", "50%"), plotOutput("supPerGradStudentUserSelected"), plotOutput("supFTETopPerGradStudent")),
                                                 splitLayout(cellWidths = c("50%", "50%"),
                                                             actionButton("supPerPerGradStudentUserToggle", "Show Table"),
                                                             actionButton("supPerTopPerGradStudentToggle", "Show Table")),
                                                 splitLayout(cellWidths = c("50%", "50%"),
                                                             htmlOutput("supPerPerGradStudentUserSelectedTable"),
                                                             htmlOutput("supPerTopPerGradStudentTable")),

                                                 splitLayout(cellWidths = c("50%", "50%"), plotOutput("supPerUndergradStudentUserSelected"), plotOutput("supFTETopUndergradStudent")),
                                                 splitLayout(cellWidths = c("50%", "50%"),
                                                             actionButton("supPerPerUndergradStudentUserToggle", "Show Table"),
                                                             actionButton("supPerTopPerUndergradStudentToggle", "Show Table")),
                                                 splitLayout(cellWidths = c("50%", "50%"),
                                                             htmlOutput("supPerPerUndergradStudentUserSelectedTable"),
                                                             htmlOutput("supPerTopPerUndergradStudentTable")),

                                                 splitLayout(cellWidths = c("50%", "50%"), plotOutput("supPerDoctoralUserSelected"), plotOutput("supFTETopPerDoctoral")),
                                                 splitLayout(cellWidths = c("50%", "50%"),
                                                             actionButton("supPerPerDoctoralUserToggle", "Show Table"),
                                                             actionButton("supPerTopPerDoctoralToggle", "Show Table")),
                                                 splitLayout(cellWidths = c("50%", "50%"),
                                                             htmlOutput("supPerPerDoctoralUserSelectedTable"),
                                                             htmlOutput("supPerTopPerDoctoralTable")),
                                               )),
                                      tabPanel("Calculate Custom Ratio",
                                               h3("Calculate Custom Ratio For Selected Members", align = "center"),
                                               br(),
                                               h4("Select two distinct statistics from the annual survey of ARL to be computed into
                                       a ratio, across your selected ARL member libraries and years. The plot will be
                                       produced at the bottom. If no plot is produced, data may not be avilable
                                      for selected statistics."),
                                               br(),
                                               br(),
                                               splitLayout(cellWidths = c("50%", "50%"),   # Copy the line below to make a select box
                                                           radioButtons("numeratorChoice", label = h3("Select Numerator"), choices = "",  selected = 1),
                                                           radioButtons("denominatorChoice", label = h3("Select Denominator"), choices = ""),  selected = 1),

                                               br(),
                                               br(),
                                               fluidRow(
                                                 splitLayout(cellWidths = c("50%", "50%"), plotOutput("customRatioUser"), plotOutput("customRatioTop"))),
                                               splitLayout(cellWidths = c("50%", "50%"),
                                                           actionButton("customRatioUserToggle", "Show Table"),
                                                           actionButton("customRatioTopToggle", "Show Table")),
                                               splitLayout(cellWidths = c("50%", "50%"),
                                                           htmlOutput("customRatioUserTable"),
                                                           htmlOutput("customRatioTopTable")),
                                      ),


      )
    )
  )
)

server <- function(input, output) {
  output$plot <- renderPlot({
    hist(rnorm(100), col = "blue", border = "white")
  })

  output$table <- renderTable({
    head(mtcars)
  })
}

shinyApp(ui, server)
