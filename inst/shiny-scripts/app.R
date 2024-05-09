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

             tags$p("Read the first tab on the right side called 'Instructions' for instructions."),

                 # br() element to introduce extra vertical spacing ----
                 br(),
                 br(),
                 # input
                 shinyalert::useShinyalert(),  # Set up shinyalert
                 uiOutput("tab2"),
                 fileInput(inputId = "file1",
                           label = "1. Dataset:
                Upload a dataset below to analyze. File should be
                in comma-separated value (.csv) format with rows corresponding
                to years and columns to ARL indicators (variables). The first column must
                be 'Year', followed by other indicators in no particular order,
                e.g., 'Institution Name', 'Institution type', etc. as directly
                downloaded
                from ARL Data Portal.",
                           accept = c(".csv")),
                 checkboxGroupInput(inputId = "instituteInput",
                                    label = "2. ARL Member Institute: Select upto 5 choices. If more
                                    than 5 institutes are selected, then last 5 will be autoselected.
                                    After, select choices for years below."),
                 br(),
                 checkboxGroupInput(inputId = "yearsInput",
                                    label = "3. Years: Select upto 5 choices and press 'Analyze'. If
                         more than 5 choices are selected, most recent 5 years will be
                         autoselected."),

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
                                   h3("Welcome to libraryStatistics Shiny application (app).", align = "center"),
                                   h3("This app is part of the libraryStatistics R package.", align = "center"),
                                   br(),
                                   h4("What is the libraryStatistics Shiny app?"),
                                   h5("The libraryStatistics is an R package for analyzing and visualizing library statistics published
                                   from the annual survey of Association of Research Libraries (ARL). Ratios are generated using various
                                   statistics for comparison purposes. The Shiny app permit to visualize plots produced by the R package
                                   in an interactive manner."),
                                   br(),
                                   h4("How to use the libraryStatistics Shiny app?"),
                                   h5("First upload a dataset downloaded from
                                   ARL Data Portal. To download data from ARL Data Portal, it is recommended that all variables are
                                   selected, with columns being 'Variables' and data sorted by 'Institution Name' (default options).
                                   Once data is uploaded, the list of choices for 'Institute' and 'Years' based on uploaded dataset
                                   will appear here. Select up to 5 ARL member institutes and up to 5 years.
                                   Explore the results by navigating the tabs on the right side of app on the top. The left panel will
                                   remain intact, so if need user can alter their choices. Choices can be altered and
                                   results on the various tabs to the right will be updated as applicable."),
                                   h5(" The file
                                   should be in comma-separated value (.csv) format with rows corresponding to years and columns to
                                   ARL indicators (variables). The first column must be 'Year', followed by other indicators in no particular
                                   order, e.g., 'Institution Name', 'Institution type', etc. as directly downloaded from ARL Data Portal."),
                                   br(),
                                   h4("Not clear on what type of data to upload?"),
                                   h5("Uploaded data would come from ARL Data Portal directly with no data cleaning involved. The file
                                   should be in comma-separated value (.csv) format with rows corresponding to years and columns to
                                   ARL indicators (variables). The first column must be 'Year', followed by other indicators in no particular
                                   order, e.g., 'Institution Name', 'Institution type', etc. as directly downloaded from ARL Data Portal.
                                   Following file could be used as a demo dataset to understand the format."),
                                   actionButton(inputId = "data1",
                                                label = "Demo Dataset for Testing"),
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
                          tabPanel("Total Library Expenditures",
                                   h3("Total Library Expenditures (USD) Ratios", align = "center"),
                                   br(),
                                   h4("Total library expenditures in United States Dollars (USD) as ratios in comparison to various statistics
                                       reported in the annual survey of ARL. The subtitle identifies the ratio being used. Ratios for ARL member
                                       institutes selected by the user are shown on left. Plot on the right shows ARL member institutes
                                       with highest corresponding ratio. The ARL ranking is shown above each bar."),
                                   br(),
                                   br(),
                                   fluidRow(
                                     splitLayout(cellWidths = c("50%", "50%"), plotOutput("tleTopPerFacultyUser"), plotOutput("tleTopPerFaculty")),
                                     splitLayout(cellWidths = c("50%", "50%"), plotOutput("tleTopPerStudentUser"), plotOutput("tleTopPerStudent")),
                                     splitLayout(cellWidths = c("50%", "50%"), plotOutput("tleTopPerGradStudentUser"), plotOutput("tleTopPerGradStudent")),
                                     splitLayout(cellWidths = c("50%", "50%"), plotOutput("tleTopPerDoctoralUser"), plotOutput("tleTopPerDoctoral")),
                                   )),
                          tabPanel("Total Library Materials Expenditures",
                                   h3("Total Library Materials Expenditures (USD) Ratios", align = "center"),
                                   br(),
                                   h4("Total library materials expenditures in United States Dollars (USD) as ratios in comparison to various statistics
                                       reported in the annual survey of ARL. The subtitle identifies the ratio being used. Ratios for ARL member
                                       institutes selected by the user are shown on left. Plot on the right shows ARL member institutes
                                       with highest corresponding ratio. The ARL ranking is shown above each bar."),
                                   br(),
                                   br(),
                                   fluidRow(
                                     splitLayout(cellWidths = c("50%", "50%"), plotOutput("tlmePerFacultyUserSelected"), plotOutput("tlmeTopPerFaculty")),
                                     splitLayout(cellWidths = c("50%", "50%"), plotOutput("tlmePerStudentUserSelected"), plotOutput("tlmeTopPerStudent")),
                                     splitLayout(cellWidths = c("50%", "50%"), plotOutput("tlmePerGradStudentUserSelected"), plotOutput("tlmeTopPerGradStudent")),
                                     splitLayout(cellWidths = c("50%", "50%"), plotOutput("tlmeTopPerDoctoralUserSelected"), plotOutput("tlmeTopPerDoctoral")),
                                   )),
                          tabPanel("Professional Staff Salaries",
                                   h3("Professional Staff Salaries (USD) Ratios", align = "center"),
                                   br(),
                                   h4("Professional staff salaries in United States Dollars (USD) as ratios in comparison to various statistics
                                       reported in the annual survey of ARL. The subtitle identifies the ratio being used. Ratios for ARL member
                                       institutes selected by the user are shown on left. Plot on the right shows ARL member institutes
                                       with highest corresponding ratio. The ARL ranking is shown above each bar."),
                                   br(),
                                   br(),
                                   fluidRow(
                                     splitLayout(cellWidths = c("50%", "50%"), plotOutput("proSalFacultyUserSelected"), plotOutput("proSalTopPerFaculty")),
                                     splitLayout(cellWidths = c("50%", "50%"), plotOutput("proSalPerStudentUserSelected"), plotOutput("proSalTopPerStudent")),
                                     splitLayout(cellWidths = c("50%", "50%"), plotOutput("proSalPerGradStudentUserSelected"), plotOutput("proSalTopPerGradStudent")),
                                     splitLayout(cellWidths = c("50%", "50%"), plotOutput("proSalPerDoctoralUserSelected"), plotOutput("proSalTopPerDoctoral")),
                                   )),
                          tabPanel("Professional Staff Counts",
                                   h3("Professional Library Staff Counts Full-time Equivalent (FTE) Ratios", align = "center"),
                                   br(),
                                   h4("Professional Library Staff Counts FTE as ratios in comparison to various statistics
                                       reported in the annual survey of ARL. The subtitle identifies the ratio being used. Ratios for ARL member
                                       institutes selected by the user are shown on left. Plot on the right shows ARL member institutes
                                       with highest corresponding ratio. The ARL ranking is shown above each bar."),
                                   br(),
                                   br(),
                                   fluidRow(
                                     splitLayout(cellWidths = c("50%", "50%"), plotOutput("proPerFacultyUserSelected"), plotOutput("proFTETopPerFaculty")),
                                     splitLayout(cellWidths = c("50%", "50%"), plotOutput("proPerStudentUserSelected"), plotOutput("proFTETopPerStudent")),
                                     splitLayout(cellWidths = c("50%", "50%"), plotOutput("proPerGradStudentUserSelected"), plotOutput("proFTETopPerGradStudent")),
                                     splitLayout(cellWidths = c("50%", "50%"), plotOutput("proPerDoctoralUserSelected"), plotOutput("proFTETopPerDoctoral")),
                                   )),
                          tabPanel("Support Staff Counts",
                                   h3("Support Library Staff Counts Full-time Equivalent (FTE) Ratios", align = "center"),
                                   br(),
                                   h4("Support Library Staff Counts FTE as ratios in comparison to various statistics
                                       reported in the annual survey of ARL. The subtitle identifies the ratio being used. Ratios for ARL member
                                       institutes selected by the user are shown on left. Plot on the right shows ARL member institutes
                                       with highest corresponding ratio. The ARL ranking is shown above each bar."),
                                   br(),
                                   br(),
                                   fluidRow(
                                     splitLayout(cellWidths = c("50%", "50%"), plotOutput("supPerFacultyUserSelected"), plotOutput("supFTETopPerFaculty")),
                                     splitLayout(cellWidths = c("50%", "50%"), plotOutput("supPerStudentUserSelected"), plotOutput("supFTETopPerStudent")),
                                     splitLayout(cellWidths = c("50%", "50%"), plotOutput("supPerGradStudentUserSelected"), plotOutput("supFTETopPerGradStudent")),
                                     splitLayout(cellWidths = c("50%", "50%"), plotOutput("supPerDoctoralUserSelected"), plotOutput("supFTETopPerDoctoral")),
                                   )),
                          tabPanel("Calculate Custom Ratio",
                                   h3("Calculate Custom Ratio For Selected Members", align = "center"),
                                   br(),
                                   h4("Select two statistics from the annual survey of ARL to be computed into
                                       a ratio, across user selected ARL members and years. The plot will be
                                       produced at the bottom with ARL rankings shown above each bar. If no plot
                                       is produced, no data maybe avilable for selected statistics."),
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
    columns <- unique(csvInput()$Year)
    updateCheckboxGroupInput(session = session,
                             inputId = "yearsInput",
                             label = NULL,
                             choices = columns,
                             selected = columns[1])
  })

  # Update Choices for Institution Name for User
  observe({
    columns2 <- unique(csvInput()$`Institution Name`)
    updateCheckboxGroupInput(session = session,
                       inputId = "instituteInput",
                       label = NULL,
                       choices = columns2[-1], # remove median
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

  # plot - tleTopPerDoctoral
  output$tleTopPerDoctoral <- renderPlot({
    expVisualization()[[4]]
  })

  # plot - tleTopPerFacultyUser
  output$tleTopPerFacultyUser <- renderPlot({
    expVisualization()[[5]]
  })

  # plot - tleTopPerStudentUser
  output$tleTopPerStudentUser <- renderPlot({
    expVisualization()[[6]]
  })

  # plot - tleTopPerGradStudentUser
  output$tleTopPerGradStudentUser <- renderPlot({
    expVisualization()[[7]]
  })

  # plot - tleTopPerDoctoralUser
  output$tleTopPerDoctoralUser <- renderPlot({
    expVisualization()[[8]]
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

  # plot - tlmeTopPerDoctoral
  output$tlmeTopPerDoctoral <- renderPlot({
    expMaterialsVis()[[4]]
  })

  # plot - tlmePerFacultyUserSelected
  output$tlmePerFacultyUserSelected <- renderPlot({
    expMaterialsVis()[[5]]
  })

  # plot - tlmePerStudentUserSelected
  output$tlmePerStudentUserSelected <- renderPlot({
    expMaterialsVis()[[6]]
  })

  # plot - tlmePerGradStudentUserSelected
  output$tlmePerGradStudentUserSelected <- renderPlot({
    expMaterialsVis()[[7]]
  })

  # plot - tlmeTopPerDoctoralUserSelected
  output$tlmeTopPerDoctoralUserSelected <- renderPlot({
    expMaterialsVis()[[8]]
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

  # plot - proSalTopPerDoctoral
  output$proSalTopPerDoctoral <- renderPlot({
    profStaffSalariesVis()[[4]]
  })

  # plot - proSalFacultyUserSelected
  output$proSalFacultyUserSelected <- renderPlot({
    profStaffSalariesVis()[[5]]
  })

  # plot - proSalPerStudentUserSelected
  output$proSalPerStudentUserSelected <- renderPlot({
    profStaffSalariesVis()[[6]]
  })

  # plot - proSalPerGradStudentUserSelected
  output$proSalPerGradStudentUserSelected <- renderPlot({
    profStaffSalariesVis()[[7]]
  })

  # plot - proSalPerDoctoralUserSelected
  output$proSalPerDoctoralUserSelected <- renderPlot({
    profStaffSalariesVis()[[8]]
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

  # plot - proFTETopPerDoctoral
  output$proFTETopPerDoctoral <- renderPlot({
    profStaffCountsVis()[[4]]
  })

  # plot - proPerFacultyUserSelected
  output$proPerFacultyUserSelected <- renderPlot({
    profStaffCountsVis()[[5]]
  })

  # plot - proPerStudentUserSelected
  output$proPerStudentUserSelected <- renderPlot({
    profStaffCountsVis()[[6]]
  })

  # plot - proPerGradStudentUserSelected
  output$proPerGradStudentUserSelected <- renderPlot({
    profStaffCountsVis()[[7]]
  })

  # plot - proPerDoctoralUserSelected
  output$proPerDoctoralUserSelected <- renderPlot({
    profStaffCountsVis()[[8]]
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

  # plot - supFTETopPerDoctoral
  output$supFTETopPerDoctoral <- renderPlot({
    supStaffCountsVis()[[4]]
  })

  # plot - supPerFacultyUserSelected
  output$supPerFacultyUserSelected <- renderPlot({
    supStaffCountsVis()[[5]]
  })

  # plot - supPerStudentUserSelected
  output$supPerStudentUserSelected <- renderPlot({
    supStaffCountsVis()[[6]]
  })

  # plot - supPerGradStudentUserSelected
  output$supPerGradStudentUserSelected <- renderPlot({
    supStaffCountsVis()[[7]]
  })

  # plot - supPerDoctoralUserSelected
  output$supPerDoctoralUserSelected <- renderPlot({
    supStaffCountsVis()[[8]]
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
