library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "libraryStatistics dashboard"),
  dashboardSidebar(disable = TRUE
                   #  menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                   #  menuItem("Widgets", tabName = "widgets", icon = icon("th"))
  ),
  dashboardBody(



    # Adding a white background
    tags$style(HTML("
      .content-wrapper, .right-side {
        background-color: white !important;
      }
    ")),

    # Boxes need to be put in a row (or column)
    fluidRow(


      # App title ----
      titlePanel(tags$h1(tags$b("libraryStatistics:"),"Visualize Statistics by Association of Research Libraries Survey")),

      # Sidebar layout with input and output definitions ----
      sidebarLayout(

        # Sidebar panel for inputs ----
        sidebarPanel(width = 3,

                     tags$p("Refer to the 'Instructions' tab located on the right side for detailed guidance."),

                     # br() element to introduce extra vertical spacing ----
                     br(),
                     br(),
                     # input
                     #shinyalert::useShinyalert(),  # Set up shinyalert
                     uiOutput("tab2"),
                     div(
                       tags$b("1. Dataset: Use built-in demo dataset OR upload a dataset for analysis.")),
                     br(),
                     actionButton("demo", "Click to use demonstration data"),
                     br(),
                     br(),
                     fileInput(inputId = "file1",
                               label = "Upload a dataset: The file should be in comma-separated value
                               (.csv) format, with rows corresponding to
                               years and columns representing ARL indicators (variables). The first
                               column must be labeled 'Year', followed by other indicators in any
                               order, such as 'Institution Name', 'Institution type', etc., as
                               present when directly downloaded from the ARL Data Portal.",
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
                              tabPanel("Instructions",
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
                                       h5("1. Data: Decide whether to use demonstration toy dataset available
                                       within the app or upload a dataset downloaded directly from ARL Data Portal.",
                                          tags$br(),
                                          tags$br(),
                                          "If demonstration dataset is to be used, click the button 'Click to use demonstration data'
                                       and proceed to step #4.",
                                          tags$br(),
                                          tags$br(),
                                          "Otherwise, begin by downloading the dataset from the ARL Data
                                       Portal (www.arlstatistics.org/data/main). Ensure that all variables are selected,
                                       with columns set to 'Variables' and the data sorted by 'Institution Name'
                                       (default options). Data should be downloaded in comma-separated value (.csv) format.
                                       Data may be downloaded for any number of years and across all member institutions
                                       available."),
                                       h5("2. Check Data: The downloaded dataset should have rows corresponding to years
                                       and columns to ARL indicators (variables). The first column must be 'Year',
                                       followed by other indicators in any order, such as 'Institution Name',
                                       'Institution type', etc., as downloaded directly from the ARL Data Portal."),
                                       h5("3. Uploading Data and Parameter Selection: Upload the dataset (.csv format)
                                       to the Shiny application."),
                                       h5("4. A list of choices for 'ARL Member Libraries' and 'Years' based
                                       on demo dataset or the uploaded dataset will appear. If the uploaded dataset contain
                                       over 5 ARL member libraries, user may select upto 5 ARL member libraries. Similarly,
                                       user may select upto 5 years for analysis. If more than 5 ARL member libraries are
                                       selected, only the last 5 will be retained. If more than 5 years are selected,
                                       the most recent 5 years will be retained."),
                                       h5("5. Exploring Results: Navigate the tabs at the top of the application to
                                       explore the results. The left panel will remain static, allowing user to modify
                                       the selections for ARL member libraries or years, as needed. Changes to
                                       selections will automatically update the results displayed in the various
                                       tabs on the right."),
                                       shiny::img(src = 'pipelineLS.png', align = "centre", height = "85%", width = "85%"),
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
                                       a data point for the given year. The 'Show Table' button below each plot may be used to view the underlying
                                       data in a table format."),
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
                                       a data point for the given year. The 'Show Table' button below each plot may be used to view the underlying
                                       data in a table format."),
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
                                       a data point for the given year. The 'Show Table' button below each plot may be used to view the underlying
                                       data in a table format."),
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
                                       a data point for the given year. The 'Show Table' button below each plot may be used to view the underlying
                                       data in a table format."),
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
                                       a data point for the given year. The 'Show Table' button below each plot may be used to view the underlying
                                       data in a table format."),
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
                                      for selected statistics. The 'Show Table' button below each plot may be used to view the underlying
                                       data in a table format."),
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



    ) # end of fluidRow
  )
)

# Define server logic for random distribution app ----
server <- function(input, output, session) {

  # Reactive expression to generate the requested distribution ----
  # This is called whenever the inputs change. The output functions
  # defined below then use the value computed from this expression


  # --- single reactive container for the currently active dataset ---
  dataset <- reactiveVal(NULL)

  # ---- Case 1: File upload ----
  observeEvent(input$file1, {
    req(input$file1)
    df <- tryCatch(
      readr::read_csv(file = input$file1$datapath),
      error = function(e) {
        showNotification("Error reading file. Please check format.", type = "error")
        NULL
      }
    )
    dataset(df)
  })

  # ---- Case 2: Demo button ----
  observeEvent(input$demo, {
    dataset(ARLDataDownload)   # replace with your built-in dataset name
  })

  # ---- Now use dataset() everywhere ----




  # Update Choices for Year for User
  observe({
    columns <- sort(unique(dataset()$Year), decreasing = TRUE)
    updateCheckboxGroupInput(session = session,
                             inputId = "yearsInput",
                             label = NULL,
                             choices = columns,
                             selected = columns[1])
  })

  # Update Choices for Institution Name for User
  observe({
    columns2 <- sort(setdiff(unique(dataset()$`Institution Name`), 'MEDIAN'))
    updateCheckboxGroupInput(session = session,
                             inputId = "instituteInput",
                             label = NULL,
                             choices = columns2, # remove median
                             selected = columns2[2])
  })

  # Update Create Own Ratio Choices for Numerator (top part)
  observe({
    columns3 <- unique(colnames(dataset())[12:80])
    updateRadioButtons(session = session,
                       inputId = "numeratorChoice",
                       label = NULL,
                       choices = columns3, # remove median
                       selected = columns3[61])
  })

  # Update Create Own Ratio Choices for Denominator (bottom part)
  observe({
    columns4 <- unique(colnames(dataset())[12:80])
    updateRadioButtons(session = session,
                       inputId = "denominatorChoice",
                       label = NULL,
                       choices = columns4, # remove median
                       selected = columns4[43])
  })

  # -- Total Library Expenditures
  expVisualization <- eventReactive(eventExpr = c(input$file1,
                                                  input$instituteInput,
                                                  input$yearsInput), {
                                                    visTotalLibraryExp(
                                                      dataARL = dataset(),
                                                      members = as.character(input$instituteInput),
                                                      years = as.vector(input$yearsInput, mode = "numeric"))
                                                  })

  # plot - tleTopPerFaculty
  output$tleTopPerFaculty <- renderPlot({
    expVisualization()[[1]]
  })

  # plot - tleTopPerStudent
  output$tleTopPerStudent <- renderPlot({
    expVisualization()[[2]]
  })

  # plot - tleTopPerGradStudent
  output$tleTopPerGradStudent <- renderPlot({
    expVisualization()[[3]]
  })

  # plot - tleTopPerUndergradStudent
  output$tleTopPerUndergradStudent <- renderPlot({
    expVisualization()[[4]]
  })

  # plot - tleTopPerDoctoral
  output$tleTopPerDoctoral <- renderPlot({
    expVisualization()[[5]]
  })

  # plot - tlePerFacultyUser
  output$tlePerFacultyUser <- renderPlot({
    expVisualization()[[6]]
  })

  # plot - tlePerStudentUser
  output$tlePerStudentUser <- renderPlot({
    expVisualization()[[7]]
  })

  # plot - tlePerGradStudentUser
  output$tlePerGradStudentUser <- renderPlot({
    expVisualization()[[8]]
  })


  # plot - tlePerUndergradStudentUser
  output$tlePerUndergradStudentUser <- renderPlot({
    expVisualization()[[9]]
  })


  # plot - tlePerDoctoralUser
  output$tlePerDoctoralUser <- renderPlot({
    expVisualization()[[10]]
  })


  # Initialize toggle states for tables
  toggleStates <- reactiveValues(
    tlePerFacultyUserT = FALSE,
    tleTopPerFacultyT = FALSE,
    tlePerStudentUserT = FALSE,
    tleTopPerStudentT = FALSE,
    tlePerGradStudentUserT = FALSE,
    tleTopPerGradStudentT = FALSE,
    tlePerUndergradStudentUserT = FALSE,
    tleTopPerUndergradStudentT = FALSE,
    tlePerDoctoralUserT = FALSE,
    tleTopPerDoctoralT = FALSE
  )

  # Observe events for toggle buttons
  observeEvent(input$tlePerFacultyUserToggle, {
    toggleStates$tlePerFacultyUserT <- !toggleStates$tlePerFacultyUserT
  })
  observeEvent(input$tleTopPerFacultyToggle, {
    toggleStates$tleTopPerFacultyT <- !toggleStates$tleTopPerFacultyT
  })
  observeEvent(input$tlePerStudentUserToggle, {
    toggleStates$tlePerStudentUserT <- !toggleStates$tlePerStudentUserT
  })
  observeEvent(input$tleTopPerStudentToggle, {
    toggleStates$tleTopPerStudentT <- !toggleStates$tleTopPerStudentT
  })
  observeEvent(input$tlePerGradStudentUserToggle, {
    toggleStates$tlePerGradStudentUserT <- !toggleStates$tlePerGradStudentUserT
  })
  observeEvent(input$tleTopPerGradStudentToggle, {
    toggleStates$tleTopPerGradStudentT <- !toggleStates$tleTopPerGradStudentT
  })
  observeEvent(input$tlePerUndergradStudentUserToggle, {
    toggleStates$tlePerUndergradStudentUserT <- !toggleStates$tlePerUndergradStudentUserT
  })
  observeEvent(input$tleTopPerUndergradStudentToggle, {
    toggleStates$tleTopPerUndergradStudentT <- !toggleStates$tleTopPerUndergradStudentT
  })
  observeEvent(input$tlePerDoctoralUserToggle, {
    toggleStates$tlePerDoctoralUserT <- !toggleStates$tlePerDoctoralUserT
  })
  observeEvent(input$tleTopPerDoctoralToggle, {
    toggleStates$tleTopPerDoctoralT <- !toggleStates$tleTopPerDoctoralT
  })


  # Render tables conditionally
  output$tlePerFacultyUserSelectedTable <- renderUI({
    if (toggleStates$tlePerFacultyUserT) {
      # table - tlePerFacultyUserSelectedTable
      output$tlePerFacultyUserSelectedTable <- renderTable({
        expVisualization()[[16]]
      }, sanitize.text.function = function(x) x, rownames = FALSE, colnames = FALSE)
      tableOutput("tlePerFacultyUserSelectedTable")
    }
  })

  output$tleTopPerFacultyTable <- renderUI({
    if (toggleStates$tleTopPerFacultyT) {
      # table - tleTopPerFacultyTable
      output$tleTopPerFacultyTable <- renderTable({
        expVisualization()[[11]]
      }, sanitize.text.function = function(x) x, rownames = FALSE, colnames = FALSE)
      tableOutput("tleTopPerFacultyTable")
    }
  })

  output$tlePerStudentUserSelectedTable <- renderUI({
    if (toggleStates$tlePerStudentUserT) {
      # table - tlePerStudentUserSelectedTable
      output$tlePerStudentUserSelectedTable <- renderTable({
        expVisualization()[[17]]
      }, sanitize.text.function = function(x) x, rownames = FALSE, colnames = FALSE)
      tableOutput("tlePerStudentUserSelectedTable")
    }
  })

  output$tleTopPerStudentTable <- renderUI({
    if (toggleStates$tleTopPerStudentT) {
      # table - tleTopPerStudentTable
      output$tleTopPerStudentTable <- renderTable({
        expVisualization()[[12]]
      }, sanitize.text.function = function(x) x, rownames = FALSE, colnames = FALSE)
      tableOutput("tleTopPerStudentTable")
    }
  })

  output$tlePerGradStudentUserSelectedTable <- renderUI({
    if (toggleStates$tlePerGradStudentUserT) {
      # table - tlePerGradStudentUserSelectedTable
      output$tlePerGradStudentUserSelectedTable <- renderTable({
        expVisualization()[[18]]
      }, sanitize.text.function = function(x) x, rownames = FALSE, colnames = FALSE)
      tableOutput("tlePerGradStudentUserSelectedTable")
    }
  })

  output$tleTopPerGradStudentTable <- renderUI({
    if (toggleStates$tleTopPerGradStudentT) {
      # table - tleTopPerGradStudentTable
      output$tleTopPerGradStudentTable <- renderTable({
        expVisualization()[[13]]
      }, sanitize.text.function = function(x) x, rownames = FALSE, colnames = FALSE)
      tableOutput("tleTopPerGradStudentTable")
    }
  })


  output$tlePerUndergradStudentUserSelectedTable <- renderUI({
    if (toggleStates$tlePerUndergradStudentUserT) {
      # table - tlePerUndergradStudentUserSelectedTable
      output$tlePerUndergradStudentUserSelectedTable <- renderTable({
        expVisualization()[[19]]
      }, sanitize.text.function = function(x) x, rownames = FALSE, colnames = FALSE)
      tableOutput("tlePerUndergradStudentUserSelectedTable")
    }
  })

  output$tleTopPerUndergradStudentTable <- renderUI({
    if (toggleStates$tleTopPerUndergradStudentT) {
      # table - tleTopPerUndergradStudentTable
      output$tleTopPerUndergradStudentTable <- renderTable({
        expVisualization()[[14]]
      }, sanitize.text.function = function(x) x, rownames = FALSE, colnames = FALSE)
      tableOutput("tleTopPerUndergradStudentTable")
    }
  })

  output$tlePerDoctoralUserSelectedTable <- renderUI({
    if (toggleStates$tlePerDoctoralUserT) {
      # table - tlePerDoctoralUserSelectedTable
      output$tlePerDoctoralUserSelectedTable <- renderTable({
        expVisualization()[[20]]
      }, sanitize.text.function = function(x) x, rownames = FALSE, colnames = FALSE)
      tableOutput("tlePerDoctoralUserSelectedTable")
    }
  })

  output$tleTopPerDoctoralTable <- renderUI({
    if (toggleStates$tleTopPerDoctoralT) {
      # table - tleTopPerDoctoralTable
      output$tleTopPerDoctoralTable <- renderTable({
        expVisualization()[[15]]
      }, sanitize.text.function = function(x) x, rownames = FALSE, colnames = FALSE)
      tableOutput("tleTopPerDoctoralTable")
    }
  })


  #
  #
  #
  # -- Total Library Materials Expenditures
  expMaterialsVis <- eventReactive(eventExpr = c(input$file1,
                                                 input$instituteInput,
                                                 input$yearsInput), {
                                                   visTotalLibMaterialsExp(
                                                     dataARL = dataset(),
                                                     members = as.character(input$instituteInput),
                                                     years = as.vector(input$yearsInput, mode = "numeric"))
                                                 })

  # plot - tlmeTopPerFaculty
  output$tlmeTopPerFaculty <- renderPlot({
    expMaterialsVis()[[1]]
  })

  # plot - tlmeTopPerStudent
  output$tlmeTopPerStudent <- renderPlot({
    expMaterialsVis()[[2]]
  })

  # plot - tlmeTopPerGradStudent
  output$tlmeTopPerGradStudent <- renderPlot({
    expMaterialsVis()[[3]]
  })

  # plot - tlmeTopPerUndergradStudent
  output$tlmeTopPerUndergradStudent <- renderPlot({
    expMaterialsVis()[[4]]
  })

  # plot - tlmeTopPerDoctoral
  output$tlmeTopPerDoctoral <- renderPlot({
    expMaterialsVis()[[5]]
  })

  # plot - tlmePerFacultyUserSelected
  output$tlmePerFacultyUserSelected <- renderPlot({
    expMaterialsVis()[[6]]
  })

  # plot - tlmePerStudentUserSelected
  output$tlmePerStudentUserSelected <- renderPlot({
    expMaterialsVis()[[7]]
  })

  # plot - tlmePerGradStudentUserSelected
  output$tlmePerGradStudentUserSelected <- renderPlot({
    expMaterialsVis()[[8]]
  })

  # plot - tlmePerUndergradStudentUserSelected
  output$tlmePerUndergradStudentUserSelected <- renderPlot({
    expMaterialsVis()[[9]]
  })

  # plot - tlmeTopPerDoctoralUserSelected
  output$tlmeTopPerDoctoralUserSelected <- renderPlot({
    expMaterialsVis()[[10]]
  })


  # Initialize toggle states for tables
  toggleStates2 <- reactiveValues(
    tlmePerFacultyUserT = FALSE,
    tlmeTopPerFacultyT = FALSE,
    tlmePerStudentUserT = FALSE,
    tlmeTopPerStudentT = FALSE,
    tlmePerGradStudentUserT = FALSE,
    tlmeTopPerGradStudentT = FALSE,
    tlmePerUndergradStudentUserT = FALSE,
    tlmeTopPerUndergradStudentT = FALSE,
    tlmePerDoctoralUserT = FALSE,
    tlmeTopPerDoctoralT = FALSE
  )

  # Observe events for toggle buttons
  observeEvent(input$tlmePerFacultyUserToggle, {
    toggleStates2$tlmePerFacultyUserT <- !toggleStates2$tlmePerFacultyUserT
  })
  observeEvent(input$tlmeTopPerFacultyToggle, {
    toggleStates2$tlmeTopPerFacultyT <- !toggleStates2$tlmeTopPerFacultyT
  })
  observeEvent(input$tlmePerStudentUserToggle, {
    toggleStates2$tlmePerStudentUserT <- !toggleStates2$tlmePerStudentUserT
  })
  observeEvent(input$tlmeTopPerStudentToggle, {
    toggleStates2$tlmeTopPerStudentT <- !toggleStates2$tlmeTopPerStudentT
  })
  observeEvent(input$tlmePerGradStudentUserToggle, {
    toggleStates2$tlmePerGradStudentUserT <- !toggleStates2$tlmePerGradStudentUserT
  })
  observeEvent(input$tlmeTopPerGradStudentToggle, {
    toggleStates2$tlmeTopPerGradStudentT <- !toggleStates2$tlmeTopPerGradStudentT
  })
  observeEvent(input$tlmePerUndergradStudentUserToggle, {
    toggleStates2$tlmePerUndergradStudentUserT <- !toggleStates2$tlmePerUndergradStudentUserT
  })
  observeEvent(input$tlmeTopPerUndergradStudentToggle, {
    toggleStates2$tlmeTopPerUndergradStudentT <- !toggleStates2$tlmeTopPerUndergradStudentT
  })
  observeEvent(input$tlmePerDoctoralUserToggle, {
    toggleStates2$tlmePerDoctoralUserT <- !toggleStates2$tlmePerDoctoralUserT
  })
  observeEvent(input$tlmeTopPerDoctoralToggle, {
    toggleStates2$tlmeTopPerDoctoralT <- !toggleStates2$tlmeTopPerDoctoralT
  })


  # Render tables conditionally
  output$tlmePerFacultyUserSelectedTable <- renderUI({
    if (toggleStates2$tlmePerFacultyUserT) {
      # table - tlmePerFacultyUserSelectedTable
      output$tlmePerFacultyUserSelectedTable <- renderTable({
        expMaterialsVis()[[16]]
      }, sanitize.text.function = function(x) x, rownames = FALSE, colnames = FALSE)
      tableOutput("tlmePerFacultyUserSelectedTable")
    }
  })

  output$tlmeTopPerFacultyTable <- renderUI({
    if (toggleStates2$tlmeTopPerFacultyT) {
      # table - tlmeTopPerFacultyTable
      output$tlmeTopPerFacultyTable <- renderTable({
        expMaterialsVis()[[11]]
      }, sanitize.text.function = function(x) x, rownames = FALSE, colnames = FALSE)
      tableOutput("tlmeTopPerFacultyTable")
    }
  })

  output$tlmePerStudentUserSelectedTable <- renderUI({
    if (toggleStates2$tlmePerStudentUserT) {
      # table - tlmePerStudentUserSelectedTable
      output$tlmePerStudentUserSelectedTable <- renderTable({
        expMaterialsVis()[[17]]
      }, sanitize.text.function = function(x) x, rownames = FALSE, colnames = FALSE)
      tableOutput("tlmePerStudentUserSelectedTable")
    }
  })

  output$tlmeTopPerStudentTable <- renderUI({
    if (toggleStates2$tlmeTopPerStudentT) {
      # table - tlmeTopPerStudentTable
      output$tlmeTopPerStudentTable <- renderTable({
        expMaterialsVis()[[12]]
      }, sanitize.text.function = function(x) x, rownames = FALSE, colnames = FALSE)
      tableOutput("tlmeTopPerStudentTable")
    }
  })

  output$tlmePerGradStudentUserSelectedTable <- renderUI({
    if (toggleStates2$tlmePerGradStudentUserT) {
      # table - tlmePerGradStudentUserSelectedTable
      output$tlmePerGradStudentUserSelectedTable <- renderTable({
        expMaterialsVis()[[18]]
      }, sanitize.text.function = function(x) x, rownames = FALSE, colnames = FALSE)
      tableOutput("tlmePerGradStudentUserSelectedTable")
    }
  })

  output$tlmeTopPerGradStudentTable <- renderUI({
    if (toggleStates2$tlmeTopPerGradStudentT) {
      # table - tlmeTopPerGradStudentTable
      output$tlmeTopPerGradStudentTable <- renderTable({
        expMaterialsVis()[[13]]
      }, sanitize.text.function = function(x) x, rownames = FALSE, colnames = FALSE)
      tableOutput("tlmeTopPerGradStudentTable")
    }
  })


  output$tlmePerUndergradStudentUserSelectedTable <- renderUI({
    if (toggleStates2$tlmePerUndergradStudentUserT) {
      # table - tlmePerUndergradStudentUserSelectedTable
      output$tlmePerUndergradStudentUserSelectedTable <- renderTable({
        expMaterialsVis()[[19]]
      }, sanitize.text.function = function(x) x, rownames = FALSE, colnames = FALSE)
      tableOutput("tlmePerUndergradStudentUserSelectedTable")
    }
  })

  output$tlmeTopPerUndergradStudentTable <- renderUI({
    if (toggleStates2$tlmeTopPerUndergradStudentT) {
      # table - tleTopPerUndergradStudentTable
      output$tlmeTopPerUndergradStudentTable <- renderTable({
        expMaterialsVis()[[14]]
      }, sanitize.text.function = function(x) x, rownames = FALSE, colnames = FALSE)
      tableOutput("tlmeTopPerUndergradStudentTable")
    }
  })

  output$tlmePerDoctoralUserSelectedTable <- renderUI({
    if (toggleStates2$tlmePerDoctoralUserT) {
      # table - tlmePerDoctoralUserSelectedTable
      output$tlmePerDoctoralUserSelectedTable <- renderTable({
        expMaterialsVis()[[20]]
      }, sanitize.text.function = function(x) x, rownames = FALSE, colnames = FALSE)
      tableOutput("tlmePerDoctoralUserSelectedTable")
    }
  })

  output$tlmeTopPerDoctoralTable <- renderUI({
    if (toggleStates2$tlmeTopPerDoctoralT) {
      # table - tleTopPerDoctoralTable
      output$tlmeTopPerDoctoralTable <- renderTable({
        expMaterialsVis()[[15]]
      }, sanitize.text.function = function(x) x, rownames = FALSE, colnames = FALSE)
      tableOutput("tlmeTopPerDoctoralTable")
    }
  })


  #
  #
  #
  # -- Professional Staff Salaries
  profStaffSalariesVis <- eventReactive(eventExpr = c(input$file1,
                                                      input$instituteInput,
                                                      input$yearsInput), {
                                                        visProfStaffSalaries(
                                                          dataARL = dataset(),
                                                          members = as.character(input$instituteInput),
                                                          years = as.vector(input$yearsInput, mode = "numeric"))
                                                      })

  # plot - proSalTopPerFaculty
  output$proSalTopPerFaculty <- renderPlot({
    profStaffSalariesVis()[[1]]
  })

  # plot - proSalTopPerStudent
  output$proSalTopPerStudent <- renderPlot({
    profStaffSalariesVis()[[2]]
  })

  # plot - proSalTopPerGradStudent
  output$proSalTopPerGradStudent <- renderPlot({
    profStaffSalariesVis()[[3]]
  })

  # plot - proSalTopPerUndergradStudent
  output$proSalTopPerUndergradStudent <- renderPlot({
    profStaffSalariesVis()[[4]]
  })

  # plot - proSalTopPerDoctoral
  output$proSalTopPerDoctoral <- renderPlot({
    profStaffSalariesVis()[[5]]
  })

  # plot - proSalFacultyUserSelected
  output$proSalFacultyUserSelected <- renderPlot({
    profStaffSalariesVis()[[6]]
  })

  # plot - proSalPerStudentUserSelected
  output$proSalPerStudentUserSelected <- renderPlot({
    profStaffSalariesVis()[[7]]
  })

  # plot - proSalPerGradStudentUserSelected
  output$proSalPerGradStudentUserSelected <- renderPlot({
    profStaffSalariesVis()[[8]]
  })

  # plot - proSalPerUndergradStudentUserSelected
  output$proSalPerUndergradStudentUserSelected <- renderPlot({
    profStaffSalariesVis()[[9]]
  })

  # plot - proSalPerDoctoralUserSelected
  output$proSalPerDoctoralUserSelected <- renderPlot({
    profStaffSalariesVis()[[10]]
  })


  # Initialize toggle states for tables
  toggleStates3 <- reactiveValues(
    proSalPerFacultyUserT = FALSE,
    proSalTopPerFacultyT = FALSE,
    proSalPerStudentUserT = FALSE,
    proSalTopPerStudentT = FALSE,
    proSalPerGradStudentUserT = FALSE,
    proSalTopPerGradStudentT = FALSE,
    proSalPerUndergradStudentUserT = FALSE,
    proSalTopPerUndergradStudentT = FALSE,
    proSalPerDoctoralUserT = FALSE,
    proSalTopPerDoctoralT = FALSE
  )

  # Observe events for toggle buttons
  observeEvent(input$proSalPerFacultyUserToggle, {
    toggleStates3$proSalPerFacultyUserT <- !toggleStates3$proSalPerFacultyUserT
  })
  observeEvent(input$proSalTopPerFacultyToggle, {
    toggleStates3$proSalTopPerFacultyT <- !toggleStates3$proSalTopPerFacultyT
  })
  observeEvent(input$proSalPerStudentUserToggle, {
    toggleStates3$proSalPerStudentUserT <- !toggleStates3$proSalPerStudentUserT
  })
  observeEvent(input$proSalTopPerStudentToggle, {
    toggleStates3$proSalTopPerStudentT <- !toggleStates3$proSalTopPerStudentT
  })
  observeEvent(input$proSalPerGradStudentUserToggle, {
    toggleStates3$proSalPerGradStudentUserT <- !toggleStates3$proSalPerGradStudentUserT
  })
  observeEvent(input$proSalTopPerGradStudentToggle, {
    toggleStates3$proSalTopPerGradStudentT <- !toggleStates3$proSalTopPerGradStudentT
  })
  observeEvent(input$proSalPerUndergradStudentUserToggle, {
    toggleStates3$proSalPerUndergradStudentUserT <- !toggleStates3$proSalPerUndergradStudentUserT
  })
  observeEvent(input$proSalTopPerUndergradStudentToggle, {
    toggleStates3$proSalTopPerUndergradStudentT <- !toggleStates3$proSalTopPerUndergradStudentT
  })
  observeEvent(input$proSalPerDoctoralUserToggle, {
    toggleStates3$proSalPerDoctoralUserT <- !toggleStates3$proSalPerDoctoralUserT
  })
  observeEvent(input$proSalTopPerDoctoralToggle, {
    toggleStates3$proSalTopPerDoctoralT <- !toggleStates3$proSalTopPerDoctoralT
  })


  # Render tables conditionally
  output$proSalPerFacultyUserSelectedTable <- renderUI({
    if (toggleStates3$proSalPerFacultyUserT) {
      # table - proSalPerFacultyUserSelectedTable
      output$proSalPerFacultyUserSelectedTable <- renderTable({
        profStaffSalariesVis()[[16]]
      }, sanitize.text.function = function(x) x, rownames = FALSE, colnames = FALSE)
      tableOutput("proSalPerFacultyUserSelectedTable")
    }
  })

  output$proSalTopPerFacultyTable <- renderUI({
    if (toggleStates3$proSalTopPerFacultyT) {
      # table - proSalTopPerFacultyTable
      output$proSalTopPerFacultyTable <- renderTable({
        profStaffSalariesVis()[[11]]
      }, sanitize.text.function = function(x) x, rownames = FALSE, colnames = FALSE)
      tableOutput("proSalTopPerFacultyTable")
    }
  })

  output$proSalPerStudentUserSelectedTable <- renderUI({
    if (toggleStates3$proSalPerStudentUserT) {
      # table - proSalPerStudentUserSelectedTable
      output$proSalPerStudentUserSelectedTable <- renderTable({
        profStaffSalariesVis()[[17]]
      }, sanitize.text.function = function(x) x, rownames = FALSE, colnames = FALSE)
      tableOutput("proSalPerStudentUserSelectedTable")
    }
  })

  output$proSalTopPerStudentTable <- renderUI({
    if (toggleStates3$proSalTopPerStudentT) {
      # table - proSalTopPerStudentTable
      output$proSalTopPerStudentTable <- renderTable({
        profStaffSalariesVis()[[12]]
      }, sanitize.text.function = function(x) x, rownames = FALSE, colnames = FALSE)
      tableOutput("proSalTopPerStudentTable")
    }
  })

  output$proSalPerGradStudentUserSelectedTable <- renderUI({
    if (toggleStates3$proSalPerGradStudentUserT) {
      # table - proSalPerGradStudentUserSelectedTable
      output$proSalPerGradStudentUserSelectedTable <- renderTable({
        profStaffSalariesVis()[[18]]
      }, sanitize.text.function = function(x) x, rownames = FALSE, colnames = FALSE)
      tableOutput("proSalPerGradStudentUserSelectedTable")
    }
  })

  output$proSalTopPerGradStudentTable <- renderUI({
    if (toggleStates3$proSalTopPerGradStudentT) {
      # table - proSalTopPerGradStudentTable
      output$proSalTopPerGradStudentTable <- renderTable({
        profStaffSalariesVis()[[13]]
      }, sanitize.text.function = function(x) x, rownames = FALSE, colnames = FALSE)
      tableOutput("proSalTopPerGradStudentTable")
    }
  })


  output$proSalPerUndergradStudentUserSelectedTable <- renderUI({
    if (toggleStates3$proSalPerUndergradStudentUserT) {
      # table - proSalPerUndergradStudentUserSelectedTable
      output$proSalPerUndergradStudentUserSelectedTable <- renderTable({
        profStaffSalariesVis()[[19]]
      }, sanitize.text.function = function(x) x, rownames = FALSE, colnames = FALSE)
      tableOutput("proSalPerUndergradStudentUserSelectedTable")
    }
  })

  output$proSalTopPerUndergradStudentTable <- renderUI({
    if (toggleStates3$proSalTopPerUndergradStudentT) {
      # table - tleTopPerUndergradStudentTable
      output$proSalTopPerUndergradStudentTable <- renderTable({
        profStaffSalariesVis()[[14]]
      }, sanitize.text.function = function(x) x, rownames = FALSE, colnames = FALSE)
      tableOutput("proSalTopPerUndergradStudentTable")
    }
  })

  output$proSalPerDoctoralUserSelectedTable <- renderUI({
    if (toggleStates3$proSalPerDoctoralUserT) {
      # table - proSalPerDoctoralUserSelectedTable
      output$proSalPerDoctoralUserSelectedTable <- renderTable({
        profStaffSalariesVis()[[20]]
      }, sanitize.text.function = function(x) x, rownames = FALSE, colnames = FALSE)
      tableOutput("proSalPerDoctoralUserSelectedTable")
    }
  })

  output$proSalTopPerDoctoralTable <- renderUI({
    if (toggleStates3$proSalTopPerDoctoralT) {
      # table - tleTopPerDoctoralTable
      output$proSalTopPerDoctoralTable <- renderTable({
        profStaffSalariesVis()[[15]]
      }, sanitize.text.function = function(x) x, rownames = FALSE, colnames = FALSE)
      tableOutput("proSalTopPerDoctoralTable")
    }
  })



  #
  #
  #
  # -- Professional Staff Counts
  profStaffCountsVis <- eventReactive(eventExpr = c(input$file1,
                                                    input$instituteInput,
                                                    input$yearsInput), {
                                                      visProfStaffCounts(
                                                        dataARL = dataset(),
                                                        members = as.character(input$instituteInput),
                                                        years = as.vector(input$yearsInput, mode = "numeric"))
                                                    })

  # plot - proFTETopPerFaculty
  output$proFTETopPerFaculty <- renderPlot({
    profStaffCountsVis()[[1]]
  })

  # plot - proFTETopPerStudent
  output$proFTETopPerStudent <- renderPlot({
    profStaffCountsVis()[[2]]
  })

  # plot - proFTETopPerGradStudent
  output$proFTETopPerGradStudent <- renderPlot({
    profStaffCountsVis()[[3]]
  })


  # plot - proFTETopPerUndergradStudent
  output$proFTETopPerUndergradStudent <- renderPlot({
    profStaffCountsVis()[[4]]
  })

  # plot - proFTETopPerDoctoral
  output$proFTETopPerDoctoral <- renderPlot({
    profStaffCountsVis()[[5]]
  })

  # plot - proPerFacultyUserSelected
  output$proFTEPerFacultyUserSelected <- renderPlot({
    profStaffCountsVis()[[6]]
  })

  # plot - proPerStudentUserSelected
  output$proFTEPerStudentUserSelected <- renderPlot({
    profStaffCountsVis()[[7]]
  })

  # plot - proPerGradStudentUserSelected
  output$proFTEPerGradStudentUserSelected <- renderPlot({
    profStaffCountsVis()[[8]]
  })

  # plot - proFTEPerUndergradStudentUserSelected
  output$proFTEPerUndergradStudentUserSelected <- renderPlot({
    profStaffCountsVis()[[9]]
  })

  # plot - proPerDoctoralUserSelected
  output$proFTEPerDoctoralUserSelected <- renderPlot({
    profStaffCountsVis()[[10]]
  })



  # Initialize toggle states for tables
  toggleStates4 <- reactiveValues(
    proFTEPerFacultyUserT = FALSE,
    proFTETopPerFacultyT = FALSE,
    proFTEPerStudentUserT = FALSE,
    proFTETopPerStudentT = FALSE,
    proFTEPerGradStudentUserT = FALSE,
    proFTETopPerGradStudentT = FALSE,
    proFTEPerUndergradStudentUserT = FALSE,
    proFTETopPerUndergradStudentT = FALSE,
    proFTEPerDoctoralUserT = FALSE,
    proFTETopPerDoctoralT = FALSE
  )

  # Observe events for toggle buttons
  observeEvent(input$proFTEPerFacultyUserToggle, {
    toggleStates4$proFTEPerFacultyUserT <- !toggleStates4$proFTEPerFacultyUserT
  })
  observeEvent(input$proFTETopPerFacultyToggle, {
    toggleStates4$proFTETopPerFacultyT <- !toggleStates4$proFTETopPerFacultyT
  })
  observeEvent(input$proFTEPerStudentUserToggle, {
    toggleStates4$proFTEPerStudentUserT <- !toggleStates4$proFTEPerStudentUserT
  })
  observeEvent(input$proFTETopPerStudentToggle, {
    toggleStates4$proFTETopPerStudentT <- !toggleStates4$proFTETopPerStudentT
  })
  observeEvent(input$proFTEPerGradStudentUserToggle, {
    toggleStates4$proFTEPerGradStudentUserT <- !toggleStates4$proFTEPerGradStudentUserT
  })
  observeEvent(input$proFTETopPerGradStudentToggle, {
    toggleStates4$proFTETopPerGradStudentT <- !toggleStates4$proFTETopPerGradStudentT
  })
  observeEvent(input$proFTEPerUndergradStudentUserToggle, {
    toggleStates4$proFTEPerUndergradStudentUserT <- !toggleStates4$proFTEPerUndergradStudentUserT
  })
  observeEvent(input$proFTETopPerUndergradStudentToggle, {
    toggleStates4$proFTETopPerUndergradStudentT <- !toggleStates4$proFTETopPerUndergradStudentT
  })
  observeEvent(input$proFTEPerDoctoralUserToggle, {
    toggleStates4$proFTEPerDoctoralUserT <- !toggleStates4$proFTEPerDoctoralUserT
  })
  observeEvent(input$proFTETopPerDoctoralToggle, {
    toggleStates4$proFTETopPerDoctoralT <- !toggleStates4$proFTETopPerDoctoralT
  })


  # Render tables conditionally
  output$proFTEPerFacultyUserSelectedTable <- renderUI({
    if (toggleStates4$proFTEPerFacultyUserT) {
      # table - proFTEPerFacultyUserSelectedTable
      output$proFTEPerFacultyUserSelectedTable <- renderTable({
        profStaffCountsVis()[[16]]
      }, sanitize.text.function = function(x) x, rownames = FALSE, colnames = FALSE)
      tableOutput("proFTEPerFacultyUserSelectedTable")
    }
  })

  output$proFTETopPerFacultyTable <- renderUI({
    if (toggleStates4$proFTETopPerFacultyT) {
      # table - proFTETopPerFacultyTable
      output$proFTETopPerFacultyTable <- renderTable({
        profStaffCountsVis()[[11]]
      }, sanitize.text.function = function(x) x, rownames = FALSE, colnames = FALSE)
      tableOutput("proFTETopPerFacultyTable")
    }
  })

  output$proFTEPerStudentUserSelectedTable <- renderUI({
    if (toggleStates4$proFTEPerStudentUserT) {
      # table - proFTEPerStudentUserSelectedTable
      output$proFTEPerStudentUserSelectedTable <- renderTable({
        profStaffCountsVis()[[17]]
      }, sanitize.text.function = function(x) x, rownames = FALSE, colnames = FALSE)
      tableOutput("proFTEPerStudentUserSelectedTable")
    }
  })

  output$proFTETopPerStudentTable <- renderUI({
    if (toggleStates4$proFTETopPerStudentT) {
      # table - proFTETopPerStudentTable
      output$proFTETopPerStudentTable <- renderTable({
        profStaffCountsVis()[[12]]
      }, sanitize.text.function = function(x) x, rownames = FALSE, colnames = FALSE)
      tableOutput("proFTETopPerStudentTable")
    }
  })

  output$proFTEPerGradStudentUserSelectedTable <- renderUI({
    if (toggleStates4$proFTEPerGradStudentUserT) {
      # table - proFTEPerGradStudentUserSelectedTable
      output$proFTEPerGradStudentUserSelectedTable <- renderTable({
        profStaffCountsVis()[[18]]
      }, sanitize.text.function = function(x) x, rownames = FALSE, colnames = FALSE)
      tableOutput("proFTEPerGradStudentUserSelectedTable")
    }
  })

  output$proFTETopPerGradStudentTable <- renderUI({
    if (toggleStates4$proFTETopPerGradStudentT) {
      # table - proFTETopPerGradStudentTable
      output$proFTETopPerGradStudentTable <- renderTable({
        profStaffCountsVis()[[13]]
      }, sanitize.text.function = function(x) x, rownames = FALSE, colnames = FALSE)
      tableOutput("proFTETopPerGradStudentTable")
    }
  })


  output$proFTEPerUndergradStudentUserSelectedTable <- renderUI({
    if (toggleStates4$proFTEPerUndergradStudentUserT) {
      # table - proFTEPerUndergradStudentUserSelectedTable
      output$proFTEPerUndergradStudentUserSelectedTable <- renderTable({
        profStaffCountsVis()[[19]]
      }, sanitize.text.function = function(x) x, rownames = FALSE, colnames = FALSE)
      tableOutput("proFTEPerUndergradStudentUserSelectedTable")
    }
  })

  output$proFTETopPerUndergradStudentTable <- renderUI({
    if (toggleStates4$proFTETopPerUndergradStudentT) {
      # table - tleTopPerUndergradStudentTable
      output$proFTETopPerUndergradStudentTable <- renderTable({
        profStaffCountsVis()[[14]]
      }, sanitize.text.function = function(x) x, rownames = FALSE, colnames = FALSE)
      tableOutput("proFTETopPerUndergradStudentTable")
    }
  })

  output$proFTEPerDoctoralUserSelectedTable <- renderUI({
    if (toggleStates4$proFTEPerDoctoralUserT) {
      # table - proFTEPerDoctoralUserSelectedTable
      output$proFTEPerDoctoralUserSelectedTable <- renderTable({
        profStaffCountsVis()[[20]]
      }, sanitize.text.function = function(x) x, rownames = FALSE, colnames = FALSE)
      tableOutput("proFTEPerDoctoralUserSelectedTable")
    }
  })

  output$proFTETopPerDoctoralTable <- renderUI({
    if (toggleStates4$proFTETopPerDoctoralT) {
      # table - tleTopPerDoctoralTable
      output$proFTETopPerDoctoralTable <- renderTable({
        profStaffCountsVis()[[15]]
      }, sanitize.text.function = function(x) x, rownames = FALSE, colnames = FALSE)
      tableOutput("proFTETopPerDoctoralTable")
    }
  })



  #
  #
  #
  # -- Support Staff Counts
  supStaffCountsVis <- eventReactive(eventExpr = c(input$file1,
                                                   input$instituteInput,
                                                   input$yearsInput), {
                                                     visSupStaffCounts(
                                                       dataARL = dataset(),
                                                       members = as.character(input$instituteInput),
                                                       years = as.vector(input$yearsInput, mode = "numeric"))
                                                   })

  # plot - supFTETopPerFaculty
  output$supFTETopPerFaculty <- renderPlot({
    supStaffCountsVis()[[1]]
  })

  # plot - supFTETopPerStudent
  output$supFTETopPerStudent <- renderPlot({
    supStaffCountsVis()[[2]]
  })

  # plot - supFTETopPerGradStudent
  output$supFTETopPerGradStudent <- renderPlot({
    supStaffCountsVis()[[3]]
  })

  # plot - supFTETopUndergradStudent
  output$supFTETopUndergradStudent <- renderPlot({
    supStaffCountsVis()[[4]]
  })

  # plot - supFTETopPerDoctoral
  output$supFTETopPerDoctoral <- renderPlot({
    supStaffCountsVis()[[5]]
  })

  # plot - supPerFacultyUserSelected
  output$supPerFacultyUserSelected <- renderPlot({
    supStaffCountsVis()[[6]]
  })

  # plot - supPerStudentUserSelected
  output$supPerStudentUserSelected <- renderPlot({
    supStaffCountsVis()[[7]]
  })

  # plot - supPerGradStudentUserSelected
  output$supPerGradStudentUserSelected <- renderPlot({
    supStaffCountsVis()[[8]]
  })

  # plot - supPerUndergradStudentUserSelected
  output$supPerUndergradStudentUserSelected <- renderPlot({
    supStaffCountsVis()[[9]]
  })

  # plot - supPerDoctoralUserSelected
  output$supPerDoctoralUserSelected <- renderPlot({
    supStaffCountsVis()[[10]]
  })



  # Initialize toggle states for tables
  toggleStates5 <- reactiveValues(
    supPerPerFacultyUserT = FALSE,
    supPerTopPerFacultyT = FALSE,
    supPerPerStudentUserT = FALSE,
    supPerTopPerStudentT = FALSE,
    supPerPerGradStudentUserT = FALSE,
    supPerTopPerGradStudentT = FALSE,
    supPerPerUndergradStudentUserT = FALSE,
    supPerTopPerUndergradStudentT = FALSE,
    supPerPerDoctoralUserT = FALSE,
    supPerTopPerDoctoralT = FALSE
  )

  # Observe events for toggle buttons
  observeEvent(input$supPerPerFacultyUserToggle, {
    toggleStates5$supPerPerFacultyUserT <- !toggleStates5$supPerPerFacultyUserT
  })
  observeEvent(input$supPerTopPerFacultyToggle, {
    toggleStates5$supPerTopPerFacultyT <- !toggleStates5$supPerTopPerFacultyT
  })
  observeEvent(input$supPerPerStudentUserToggle, {
    toggleStates5$supPerPerStudentUserT <- !toggleStates5$supPerPerStudentUserT
  })
  observeEvent(input$supPerTopPerStudentToggle, {
    toggleStates5$supPerTopPerStudentT <- !toggleStates5$supPerTopPerStudentT
  })
  observeEvent(input$supPerPerGradStudentUserToggle, {
    toggleStates5$supPerPerGradStudentUserT <- !toggleStates5$supPerPerGradStudentUserT
  })
  observeEvent(input$supPerTopPerGradStudentToggle, {
    toggleStates5$supPerTopPerGradStudentT <- !toggleStates5$supPerTopPerGradStudentT
  })
  observeEvent(input$supPerPerUndergradStudentUserToggle, {
    toggleStates5$supPerPerUndergradStudentUserT <- !toggleStates5$supPerPerUndergradStudentUserT
  })
  observeEvent(input$supPerTopPerUndergradStudentToggle, {
    toggleStates5$supPerTopPerUndergradStudentT <- !toggleStates5$supPerTopPerUndergradStudentT
  })
  observeEvent(input$supPerPerDoctoralUserToggle, {
    toggleStates5$supPerPerDoctoralUserT <- !toggleStates5$supPerPerDoctoralUserT
  })
  observeEvent(input$supPerTopPerDoctoralToggle, {
    toggleStates5$supPerTopPerDoctoralT <- !toggleStates5$supPerTopPerDoctoralT
  })


  # Render tables conditionally
  output$supPerPerFacultyUserSelectedTable <- renderUI({
    if (toggleStates5$supPerPerFacultyUserT) {
      # table - supPerPerFacultyUserSelectedTable
      output$supPerPerFacultyUserSelectedTable <- renderTable({
        supStaffCountsVis()[[16]]
      }, sanitize.text.function = function(x) x, rownames = FALSE, colnames = FALSE)
      tableOutput("supPerPerFacultyUserSelectedTable")
    }
  })

  output$supPerTopPerFacultyTable <- renderUI({
    if (toggleStates5$supPerTopPerFacultyT) {
      # table - supPerTopPerFacultyTable
      output$supPerTopPerFacultyTable <- renderTable({
        supStaffCountsVis()[[11]]
      }, sanitize.text.function = function(x) x, rownames = FALSE, colnames = FALSE)
      tableOutput("supPerTopPerFacultyTable")
    }
  })

  output$supPerPerStudentUserSelectedTable <- renderUI({
    if (toggleStates5$supPerPerStudentUserT) {
      # table - supPerPerStudentUserSelectedTable
      output$supPerPerStudentUserSelectedTable <- renderTable({
        supStaffCountsVis()[[17]]
      }, sanitize.text.function = function(x) x, rownames = FALSE, colnames = FALSE)
      tableOutput("supPerPerStudentUserSelectedTable")
    }
  })

  output$supPerTopPerStudentTable <- renderUI({
    if (toggleStates5$supPerTopPerStudentT) {
      # table - supPerTopPerStudentTable
      output$supPerTopPerStudentTable <- renderTable({
        supStaffCountsVis()[[12]]
      }, sanitize.text.function = function(x) x, rownames = FALSE, colnames = FALSE)
      tableOutput("supPerTopPerStudentTable")
    }
  })

  output$supPerPerGradStudentUserSelectedTable <- renderUI({
    if (toggleStates5$supPerPerGradStudentUserT) {
      # table - supPerPerGradStudentUserSelectedTable
      output$supPerPerGradStudentUserSelectedTable <- renderTable({
        supStaffCountsVis()[[18]]
      }, sanitize.text.function = function(x) x, rownames = FALSE, colnames = FALSE)
      tableOutput("supPerPerGradStudentUserSelectedTable")
    }
  })

  output$supPerTopPerGradStudentTable <- renderUI({
    if (toggleStates5$supPerTopPerGradStudentT) {
      # table - supPerTopPerGradStudentTable
      output$supPerTopPerGradStudentTable <- renderTable({
        supStaffCountsVis()[[13]]
      }, sanitize.text.function = function(x) x, rownames = FALSE, colnames = FALSE)
      tableOutput("supPerTopPerGradStudentTable")
    }
  })


  output$supPerPerUndergradStudentUserSelectedTable <- renderUI({
    if (toggleStates5$supPerPerUndergradStudentUserT) {
      # table - supPerPerUndergradStudentUserSelectedTable
      output$supPerPerUndergradStudentUserSelectedTable <- renderTable({
        supStaffCountsVis()[[19]]
      }, sanitize.text.function = function(x) x, rownames = FALSE, colnames = FALSE)
      tableOutput("supPerPerUndergradStudentUserSelectedTable")
    }
  })

  output$supPerTopPerUndergradStudentTable <- renderUI({
    if (toggleStates5$supPerTopPerUndergradStudentT) {
      # table - tleTopPerUndergradStudentTable
      output$supPerTopPerUndergradStudentTable <- renderTable({
        supStaffCountsVis()[[14]]
      }, sanitize.text.function = function(x) x, rownames = FALSE, colnames = FALSE)
      tableOutput("supPerTopPerUndergradStudentTable")
    }
  })

  output$supPerPerDoctoralUserSelectedTable <- renderUI({
    if (toggleStates5$supPerPerDoctoralUserT) {
      # table - supPerPerDoctoralUserSelectedTable
      output$supPerPerDoctoralUserSelectedTable <- renderTable({
        supStaffCountsVis()[[20]]
      }, sanitize.text.function = function(x) x, rownames = FALSE, colnames = FALSE)
      tableOutput("supPerPerDoctoralUserSelectedTable")
    }
  })

  output$supPerTopPerDoctoralTable <- renderUI({
    if (toggleStates5$supPerTopPerDoctoralT) {
      # table - tleTopPerDoctoralTable
      output$supPerTopPerDoctoralTable <- renderTable({
        supStaffCountsVis()[[15]]
      }, sanitize.text.function = function(x) x, rownames = FALSE, colnames = FALSE)
      tableOutput("supPerTopPerDoctoralTable")
    }
  })





  #
  #
  # -- Custom Ratio
  customRatioShiny <- eventReactive(eventExpr = c(input$file1,
                                                  input$instituteInput,
                                                  input$yearsInput,
                                                  input$numeratorChoice,
                                                  input$denominatorChoice), {
                                                    customRatioBuilder(
                                                      dataARL = dataset(),
                                                      numerator = as.character(input$numeratorChoice),
                                                      denominator = as.character(input$denominatorChoice),
                                                      members = as.character(input$instituteInput),
                                                      years = as.vector(input$yearsInput, mode = "numeric"))
                                                  })

  # plot - customRatioTop
  output$customRatioTop <- renderPlot({
    customRatioShiny()[[1]]
  })

  # plot - customRatioUser
  output$customRatioUser <- renderPlot({
    customRatioShiny()[[2]]
  })


  # Initialize toggle states for tables
  toggleStates6 <- reactiveValues(
    customRatioUserT = FALSE,
    customRatioTopT = FALSE
  )

  # Observe events for toggle buttons
  observeEvent(input$customRatioUserToggle, {
    toggleStates6$customRatioUserT <- !toggleStates6$customRatioUserT
  })
  observeEvent(input$customRatioTopToggle, {
    toggleStates6$customRatioTopT <- !toggleStates6$customRatioTopT
  })


  # Render tables conditionally
  output$customRatioUserTable <- renderUI({
    if (toggleStates6$customRatioUserT) {
      # table - customRatioUserTable
      output$customRatioUserTable <- renderTable({
        customRatioShiny()[[4]]
      }, sanitize.text.function = function(x) x, rownames = FALSE, colnames = FALSE)
      tableOutput("customRatioUserTable")
    }
  })

  output$customRatioTopTable <- renderUI({
    if (toggleStates6$customRatioTopT) {
      # table - customRatioTopTable
      output$customRatioTopTable <- renderTable({
        customRatioShiny()[[3]]
      }, sanitize.text.function = function(x) x, rownames = FALSE, colnames = FALSE)
      tableOutput("customRatioTopTable")
    }
  })



  #
  #
  # -- Index Table Generator
  indexTableGenVis <- eventReactive(eventExpr = c(input$file1,
                                                  input$instituteInput,
                                                  input$yearsInput), {
                                                    indexTableGenerator(
                                                      dataARL = dataset(),
                                                      members = as.character(input$instituteInput),
                                                      years = as.vector(input$yearsInput, mode = "numeric"))
                                                  })
  # plot - indexTableGenVis
  output$indexTableGen <- renderText({
    indexTableGenVis()
  })


  # URLs for downloading data
  url1 <- a("Example Dataset 2", href="https://raw.githubusercontent.com/anjalisilva/TestingPackage/master/inst/extdata/GeneCountsData2.csv")
  output$tab1 <- renderUI({
    tagList("Download:", url1)
  })


  observeEvent(input$data1, {
    # Show a modal when the button is pressed
    shinyalert(title = "Example Dataset 1",
               text = "You may download a simulated example dataset generated
               with the format required from link: https://exampledata.ca",
               type = "info")
  })





}


shinyApp(ui, server)
