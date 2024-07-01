library(shiny)
library(shinyalert)

# Define UI for random distribution app ----
ui <- fluidPage(

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
                                       with the highest ratio."),
                                   br(),
                                   br(),
                                   fluidRow(
                                     splitLayout(cellWidths = c("50%", "50%"), plotOutput("tlePerFacultyUser"), plotOutput("tleTopPerFaculty")),
                                     splitLayout(cellWidths = c("50%", "50%"), plotOutput("tlePerStudentUser"), plotOutput("tleTopPerStudent")),
                                     splitLayout(cellWidths = c("50%", "50%"), plotOutput("tlePerGradStudentUser"), plotOutput("tleTopPerGradStudent")),
                                     splitLayout(cellWidths = c("50%", "50%"), plotOutput("tlePerUndergradStudent"), plotOutput("tleTopPerUndergradStudent")),
                                     splitLayout(cellWidths = c("50%", "50%"), plotOutput("tlePerDoctoralUser"), plotOutput("tleTopPerDoctoral")),
                                   )),
                          tabPanel("Total Library Materials Expenditures",
                                   h3("Total Library Materials Expenditures (USD) Ratios", align = "center"),
                                   br(),
                                   h4("Total library materials expenditures in United States Dollars (USD) as ratios in comparison to various statistics
                                       reported in the annual survey of ARL. The chart title identifies the ratio being shown. Ratios for ARL member
                                       libraries selected by the user are shown on left. Plot on the right shows top 5 ARL member libraries
                                       with the highest ratio."),
                                   br(),
                                   br(),
                                   fluidRow(
                                     splitLayout(cellWidths = c("50%", "50%"), plotOutput("tlmePerFacultyUserSelected"), plotOutput("tlmeTopPerFaculty")),
                                     splitLayout(cellWidths = c("50%", "50%"), plotOutput("tlmePerStudentUserSelected"), plotOutput("tlmeTopPerStudent")),
                                     splitLayout(cellWidths = c("50%", "50%"), plotOutput("tlmePerGradStudentUserSelected"), plotOutput("tlmeTopPerGradStudent")),
                                     splitLayout(cellWidths = c("50%", "50%"), plotOutput("tlmePerUndergradStudentUserSelected"), plotOutput("tlmeTopPerUndergradStudent")),
                                     splitLayout(cellWidths = c("50%", "50%"), plotOutput("tlmeTopPerDoctoralUserSelected"), plotOutput("tlmeTopPerDoctoral")),
                                   )),
                          tabPanel("Professional Staff Salaries",
                                   h3("Professional Staff Salaries (USD) Ratios", align = "center"),
                                   br(),
                                   h4("Professional staff salaries in United States Dollars (USD) as ratios in comparison to various statistics
                                       reported in the annual survey of ARL. The chart title identifies the ratio being shown. Ratios for ARL member
                                       libraries selected by the user are shown on left. Plot on the right shows top 5 ARL member libraries
                                       with the highest ratio."),
                                   br(),
                                   br(),
                                   fluidRow(
                                     splitLayout(cellWidths = c("50%", "50%"), plotOutput("proSalFacultyUserSelected"), plotOutput("proSalTopPerFaculty")),
                                     splitLayout(cellWidths = c("50%", "50%"), plotOutput("proSalPerStudentUserSelected"), plotOutput("proSalTopPerStudent")),
                                     splitLayout(cellWidths = c("50%", "50%"), plotOutput("proSalPerGradStudentUserSelected"), plotOutput("proSalTopPerGradStudent")),
                                     splitLayout(cellWidths = c("50%", "50%"), plotOutput("proSalPerUndergradStudentUserSelected"), plotOutput("proSalTopPerUndergradStudent")),
                                     splitLayout(cellWidths = c("50%", "50%"), plotOutput("proSalPerDoctoralUserSelected"), plotOutput("proSalTopPerDoctoral")),
                                   )),
                          tabPanel("Professional Staff Counts",
                                   h3("Professional Library Staff Counts Full-time Equivalent (FTE) Ratios", align = "center"),
                                   br(),
                                   h4("Professional Library Staff Counts FTE as ratios in comparison to various statistics
                                       reported in the annual survey of ARL. The chart title identifies the ratio being shown. Ratios for ARL member
                                       libraries selected by the user are shown on left. Plot on the right shows top 5 ARL member libraries
                                       with the highest ratio. "),
                                   br(),
                                   br(),
                                   fluidRow(
                                     splitLayout(cellWidths = c("50%", "50%"), plotOutput("proFTEPerFacultyUserSelected"), plotOutput("proFTETopPerFaculty")),
                                     splitLayout(cellWidths = c("50%", "50%"), plotOutput("proFTEPerStudentUserSelected"), plotOutput("proFTETopPerStudent")),
                                     splitLayout(cellWidths = c("50%", "50%"), plotOutput("proFTEPerGradStudentUserSelected"), plotOutput("proFTETopPerGradStudent")),
                                     splitLayout(cellWidths = c("50%", "50%"), plotOutput("proFTEPerUndergradStudentUserSelected"), plotOutput("proFTETopPerUndergradStudent")),
                                     splitLayout(cellWidths = c("50%", "50%"), plotOutput("proFTEPerDoctoralUserSelected"), plotOutput("proFTETopPerDoctoral")),
                                   )),
                          tabPanel("Support Staff Counts",
                                   h3("Support Library Staff Counts Full-time Equivalent (FTE) Ratios", align = "center"),
                                   br(),
                                   h4("Support Library Staff Counts FTE as ratios in comparison to various statistics
                                       reported in the annual survey of ARL. The chart title identifies the ratio being shown. Ratios for ARL member
                                       libraries selected by the user are shown on left. Plot on the right shows top 5 ARL member libraries
                                       with the highest ratio."),
                                   br(),
                                   br(),
                                   fluidRow(
                                     splitLayout(cellWidths = c("50%", "50%"), plotOutput("supPerFacultyUserSelected"), plotOutput("supFTETopPerFaculty")),
                                     splitLayout(cellWidths = c("50%", "50%"), plotOutput("supPerStudentUserSelected"), plotOutput("supFTETopPerStudent")),
                                     splitLayout(cellWidths = c("50%", "50%"), plotOutput("supPerGradStudentUserSelected"), plotOutput("supFTETopPerGradStudent")),
                                     splitLayout(cellWidths = c("50%", "50%"), plotOutput("supPerUndergradStudentUserSelected"), plotOutput("supFTETopUndergradStudent")),
                                     splitLayout(cellWidths = c("50%", "50%"), plotOutput("supPerDoctoralUserSelected"), plotOutput("supFTETopPerDoctoral")),
                                   )),
                          tabPanel("Calculate Custom Ratio",
                                   h3("Calculate Custom Ratio For Selected Members", align = "center"),
                                   br(),
                                   h4("Select two distinct statistics from the annual survey of ARL to be computed into
                                       a ratio, across your selected ARL member libraries and years. The plot will be
                                       produced at the bottom. If no plot is produced, no data maybe avilable
                                      for your selected statistics."),
                                   br(),
                                   br(),
                                   splitLayout(cellWidths = c("50%", "50%"),   # Copy the line below to make a select box
                                               radioButtons("numeratorChoice", label = h3("Select Numerator"), choices = "",  selected = 1),
                                               radioButtons("denominatorChoice", label = h3("Select Denominator"), choices = ""),  selected = 1),
                                   br(),
                                   br(),
                                   fluidRow(
                                     splitLayout(cellWidths = c("50%", "50%"), plotOutput("customRatioUser"), plotOutput("customRatioTop"))),
                                   ),


              )
    )
  )
)

# Define server logic for random distribution app ----
server <- function(input, output, session) {

  # Reactive expression to generate the requested distribution ----
  # This is called whenever the inputs change. The output functions
  # defined below then use the value computed from this expression


  # Step I: Save Input csv As a Reactive
  csvInput <- reactive({
    if (is.null(input$file1)) {
      return(NULL)
    } else {
      readr::read_csv(file = input$file1$datapath)
    }
  })

  # Update Choices for Year for User
  observe({
    columns <- sort(unique(csvInput()$Year), decreasing = TRUE)
    updateCheckboxGroupInput(session = session,
                             inputId = "yearsInput",
                             label = NULL,
                             choices = columns,
                             selected = columns[1])
  })

  # Update Choices for Institution Name for User
  observe({
    columns2 <- sort(setdiff(unique(csvInput()$`Institution Name`), 'MEDIAN'))
    updateCheckboxGroupInput(session = session,
                       inputId = "instituteInput",
                       label = NULL,
                       choices = columns2, # remove median
                       selected = columns2[2])
  })

  # Update Create Own Ratio Choices for Numerator (top part)
  observe({
    columns3 <- unique(colnames(csvInput())[12:80])
    updateRadioButtons(session = session,
                       inputId = "numeratorChoice",
                       label = NULL,
                       choices = columns3, # remove median
                       selected = columns3[61])
  })

  # Update Create Own Ratio Choices for Denominator (bottom part)
  observe({
    columns4 <- unique(colnames(csvInput())[12:80])
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
      dataARL = csvInput(),
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


  # plot - tlePerUndergradStudent
  output$tlePerUndergradStudent <- renderPlot({
    expVisualization()[[9]]
  })


  # plot - tlePerDoctoralUser
  output$tlePerDoctoralUser <- renderPlot({
    expVisualization()[[10]]
  })



  # -- Total Library Materials Expenditures
  expMaterialsVis <- eventReactive(eventExpr = c(input$file1,
                                                 input$instituteInput,
                                                 input$yearsInput), {
    visTotalLibMaterialsExp(
      dataARL = csvInput(),
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


  # -- Professional Staff Salaries
  profStaffSalariesVis <- eventReactive(eventExpr = c(input$file1,
                                                      input$instituteInput,
                                                      input$yearsInput), {
    visProfStaffSalaries(
      dataARL = csvInput(),
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


  # -- Professional Staff Counts
  profStaffCountsVis <- eventReactive(eventExpr = c(input$file1,
                                                    input$instituteInput,
                                                    input$yearsInput), {
    visProfStaffCounts(
      dataARL = csvInput(),
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


  # -- Support Staff Counts
  supStaffCountsVis <- eventReactive(eventExpr = c(input$file1,
                                                   input$instituteInput,
                                                   input$yearsInput), {
    visSupStaffCounts(
      dataARL = csvInput(),
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


  # -- Custom Ratio
  customRatioShiny <- eventReactive(eventExpr = c(input$file1,
                                                  input$instituteInput,
                                                  input$yearsInput,
                                                  input$numeratorChoice,
                                                  input$denominatorChoice), {
    customRatioBuilder(
      dataARL = csvInput(),
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



  # -- Index Table Generator
  indexTableGenVis <- eventReactive(eventExpr = c(input$file1,
                                                    input$instituteInput,
                                                    input$yearsInput), {
                                                    indexTableGenerator(
                                                        dataARL = csvInput(),
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

# Create Shiny app ----
shinyApp(ui, server)
