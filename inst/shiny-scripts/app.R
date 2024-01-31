library(shiny)
library(shinyalert)

# Define UI for random distribution app ----
ui <- fluidPage(

  # App title ----
  titlePanel(tags$h1(tags$b("libraryStatistics:"),"Visualize Statistics by Association of Research Libraries Survey")),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      tags$p("Instructions: This is a Shiny Application (app) that is part of the libraryStatistics
             R package. Most of the functions available via the package are made
             available with Shiny App. The libraryStatistics is an R package for
             analyzing and visualizing library statistics published from the annual
             survey of Association of Research Libraries (ARL). First upload the dataset.
             The list of choices for 'Institute' and 'Years' based on uploaded dataset
             will appear. Select one institute and upto 5 years, and press 'Analyze'.
             Explore the results by navigating the tabs to the right."),

      # br() element to introduce extra vertical spacing ----
      br(),
      br(),
      br(),
      # input
      shinyalert::useShinyalert(),  # Set up shinyalert
      uiOutput("tab2"),
      actionButton(inputId = "data1",
                   label = "Demo Dataset for Testing"),
      fileInput(inputId = "file1",
                label = "Dataset: Upload a dataset below to analyze. File should be
                in comma-separated value (.csv) format with rows corresponding
                to years and columns to variables. The first column must
                be 'Year', followed by other variables in no particular order,
                e.g., 'Institution Name', 'Institution type', etc. as downloaded
                from ARL Data Portal.",
                accept = c(".csv")),
      selectInput(inputId = "instituteInput",
                  label = "Institute: select one library",
                  choices = ""),
      checkboxGroupInput(inputId = "yearsInput",
                         label = "Years: Select upto 5 choices and press 'Analyze'. If
                         more than 5 values provided, most recent 5 years will be
                         autoselected."),

      # br() element to introduce extra vertical spacing ----
      br(),

      # actionButton
      actionButton(inputId = "button2",
                   label = "Analyze"),

      # br() element to introduce extra vertical spacing -
      br(),

    ), # End of side pannel


    # Main panel for displaying outputs
    mainPanel(

      # Output: Tabet
      tabsetPanel(type = "tabs",
                  tabPanel("Summary",
                           h3("Summary of Dataset"),
                           br(),
                           fluidRow(
                             splitLayout(cellWidths = c("100%"), plotOutput("summaryRegionData")),
                             splitLayout(cellWidths = c("100%"), plotOutput("summaryInstTypeData")),
                           )),
                  tabPanel("Titles",
                           h3("A Comparison of Titles Held For Selected Institute"),
                           br(),
                           fluidRow(
                             splitLayout(cellWidths = c("100%"), plotOutput("titleUserInstitute")),
                             splitLayout(cellWidths = c("100%"), plotOutput("plotARLRankTop")),
                             splitLayout(cellWidths = c("100%"), plotOutput('InstCanadianPlot')),
                             splitLayout(cellWidths = c("100%"), plotOutput("academicPlot")),
                             splitLayout(cellWidths = c("100%"), plotOutput('instTypePlot')),
                           )),
                  tabPanel("Volumes",
                           h3("A Comparison of Volumes Held For Selected Institute"),
                           br(),
                           fluidRow(
                             splitLayout(cellWidths = c("100%"), plotOutput("volumeUserInstitute")),
                             splitLayout(cellWidths = c("100%"), plotOutput("volumeARLRankTop")),
                             splitLayout(cellWidths = c("100%"), plotOutput("volumeInstCanadian")),
                             splitLayout(cellWidths = c("100%"), plotOutput("volumeAcademic")),
                             splitLayout(cellWidths = c("100%"), plotOutput("volumeInstType")),
                           )),
                  tabPanel("Ebooks",
                           h3("A Comparison of eBooks For Selected Institute"),
                           br(),
                           fluidRow(
                             splitLayout(cellWidths = c("50%", "50%"), plotOutput("eBookUserInstitute"), plotOutput('eBookVolumeComp')),
                             splitLayout(cellWidths = c("100%"), plotOutput("eBookARLRankTop")),
                             splitLayout(cellWidths = c("100%"), plotOutput('eBookInstCanadian')),
                             splitLayout(cellWidths = c("100%"), plotOutput("eBookAcademicPlot")),
                             splitLayout(cellWidths = c("100%"), plotOutput('eBookInstType')),
                           )),
                  tabPanel("Library Expenditures",
                           h3("A Comparison of Total Library Expenditures For Selected Institute"),
                           br(),
                           fluidRow(
                             splitLayout(cellWidths = c("50%", "50%"), plotOutput("tleUserInstitute"), plotOutput('tleExpComp')),
                             splitLayout(cellWidths = c("100%"), plotOutput("tleARLRankTop")),
                             splitLayout(cellWidths = c("100%"), plotOutput("tleInstCanadian")),
                             splitLayout(cellWidths = c("100%"), plotOutput("tleAcademicPlot")),
                             splitLayout(cellWidths = c("100%"), plotOutput('tleInstType')),
                           )),
                  tabPanel("Salaries & Wages",
                           h3("A Comparison of Library Salaries & Wages For Selected Institute"),
                           br(),
                           fluidRow(
                             splitLayout(cellWidths = c("50%", "50%"), plotOutput("salariesUserInstitute"), plotOutput('salariesExpComp')),
                             splitLayout(cellWidths = c("100%"), plotOutput("salariesARLRankTop")),
                             splitLayout(cellWidths = c("100%"), plotOutput('salariesInstCanadian')),
                             splitLayout(cellWidths = c("100%"), plotOutput("salariesAcademicPlot")),
                             splitLayout(cellWidths = c("100%"), plotOutput('salariesInstType')),
                           )),
                  tabPanel("Staff Counts",
                           h3("A Comparison of Library Staff Counts For Selected Institute"),
                           br(),
                           fluidRow(
                             splitLayout(cellWidths = c("50%", "50%"), plotOutput("staffFTEUserInstitute"), plotOutput('staffFTEComp')),
                             splitLayout(cellWidths = c("100%"), plotOutput("staffFTEARLRankTop")),
                             splitLayout(cellWidths = c("100%"), plotOutput('staffFTEInstCanadian')),
                             splitLayout(cellWidths = c("100%"), plotOutput("staffFTEAcademicPlot")),
                             splitLayout(cellWidths = c("100%"), plotOutput('staffFTEInstType')),
                           ))


      )
    )
  )
)

# Define server logic for random distribution app ----
server <- function(input, output, session) {

  # Reactive expression to generate the requested distribution ----
  # This is called whenever the inputs change. The output functions
  # defined below then use the value computed from this expression


  # Step I: save input csv as a reactive
  csvInput <- reactive({
    if (is.null(input$file1)) {
      return(NULL)
    } else {
      readr::read_csv(file = input$file1$datapath)
    }
  })

  observe({
    columns <- unique(csvInput()$Year)
    updateCheckboxGroupInput(session,
                             "yearsInput",
                             label = NULL,
                             choices = columns,
                             selected = columns[1])
  })


  observe({
    columns2 <- unique(csvInput()$`Institution Name`)
    updateSelectInput(session,
                      "instituteInput",
                      label = NULL,
                      choices = columns2,
                      selected = columns2[2])
  })

  # -- Summary
  summaryVisualizing <- eventReactive(eventExpr = input$button2, {
    visSummaryAllData(dataARL = csvInput(),
                  institute = as.character(input$instituteInput),
                  years = as.vector(input$yearsInput, mode = "numeric"))
  })

  # plot - summaryRegionData
  output$summaryRegionData <- renderPlot({
    summaryVisualizing()[[1]]
  })

  # plot - summaryInstTypeData
  output$summaryInstTypeData <- renderPlot({
    summaryVisualizing()[[2]]
  })


  # -- Titles
  startvisualizing <- eventReactive(eventExpr = input$button2, {
    visTitlesData(dataARL = csvInput(),
                  institute = as.character(input$instituteInput),
                  years = as.vector(input$yearsInput, mode = "numeric"))
  })

  # startvisualizing <- reactive({
  #  if (is.null(input$file1)) {
  #    cat("\n Upload the data file\n")
  #  } else {
  #  visTitlesData(dataARL = csvInput(),
  #                institute = as.character(input$instituteInput),
  #                years = as.vector(input$yearsInput, mode = "numeric"))
  #  }
  # })

  # Only used for test purposes
  # output$textoutput <- renderText({
  #   as.vector(input$yearsInput, mode = "numeric")[1]
  # })

  # plot - titleUserInstitute
  output$titleUserInstitute <- renderPlot({
    startvisualizing()[[1]]
  })

  # plot - InstCanadianPlot
  output$InstCanadianPlot <- renderPlot({
    startvisualizing()[[2]]
  })

  # plot - instTypePlot
  output$instTypePlot <- renderPlot({
    startvisualizing()[[3]]
  })

  # plot - academicPlot
  output$academicPlot <- renderPlot({
    startvisualizing()[[4]]
  })


  # plot - plotARLRankTop
  output$plotARLRankTop <- renderPlot({
    startvisualizing()[[5]]
  })

  # -- Volumes
  # Using button
  startvisualizing2 <- eventReactive(eventExpr = input$button2, {
    visVolumeData(dataARL = csvInput(),
                  institute = as.character(input$instituteInput),
                  years = as.vector(input$yearsInput, mode = "numeric"))
  })

  # Without using the button
  #startvisualizing2 <- reactive({
  #  if (is.null(input$file1)) {
  #    cat("\n Upload the data file\n")
  #  } else {
  #    visVolumeData(dataARL = csvInput(),
  #                  institute = as.character(input$instituteInput),
  #                  years = as.vector(input$yearsInput, mode = "numeric"))
  #  }
  # })

  # plot - titleUserInstitute
  output$volumeUserInstitute <- renderPlot({
    startvisualizing2()[[1]]
  })

  # plot - InstCanadianPlot
  output$volumeInstCanadian <- renderPlot({
    startvisualizing2()[[2]]
  })

  # plot - instTypePlot
  output$volumeInstType <- renderPlot({
    startvisualizing2()[[3]]
  })

  # plot - academicPlot
  output$volumeAcademic <- renderPlot({
    startvisualizing2()[[4]]
  })


  # plot - plotARLRankTop
  output$volumeARLRankTop <- renderPlot({
    startvisualizing2()[[5]]
  })

  # -- eBooks
  startvisualizing3 <- eventReactive(eventExpr = input$button2, {
    viseBookData(dataARL = csvInput(),
                 institute = as.character(input$instituteInput),
                 years = as.vector(input$yearsInput, mode = "numeric"))
  })

  # plot - eBookUserInstitute
  output$eBookUserInstitute <- renderPlot({
    startvisualizing3()[[1]]
  })

  # plot - eBookVolumeComp
  output$eBookVolumeComp <- renderPlot({
    startvisualizing3()[[2]]
  })

  # plot - eBookInstCanadian
  output$eBookInstCanadian <- renderPlot({
    startvisualizing3()[[3]]
  })

  # plot - eBookInstType
  output$eBookInstType <- renderPlot({
    startvisualizing3()[[4]]
  })

  # plot - eBookAcademicPlot
  output$eBookAcademicPlot <- renderPlot({
    startvisualizing3()[[5]]
  })

  # plot - eBookARLRankTop
  output$eBookARLRankTop <- renderPlot({
    startvisualizing3()[[6]]
  })



  # -- Total Library Expenditures
  startvisualizing4 <- eventReactive(eventExpr = input$button2, {
    visTotalLibraryExp(
      dataARL = csvInput(),
      institute = as.character(input$instituteInput),
      years = as.vector(input$yearsInput, mode = "numeric"))
  })

  # plot - tleUserInstitute
  output$tleUserInstitute <- renderPlot({
    startvisualizing4()[[1]]
  })

  # plot - tleExpComp
  output$tleExpComp <- renderPlot({
    startvisualizing4()[[2]]
  })

  # plot - tleInstCanadian
  output$tleInstCanadian <- renderPlot({
    startvisualizing4()[[3]]
  })

  # plot - tleInstType
  output$tleInstType <- renderPlot({
    startvisualizing4()[[4]]
  })

  # plot - tleAcademicPlot
  output$tleAcademicPlot <- renderPlot({
    startvisualizing4()[[5]]
  })

  # plot - tleARLRankTop
  output$tleARLRankTop <- renderPlot({
    startvisualizing4()[[6]]
  })


  # -- Salaraies
  startvisualizing5 <- eventReactive(eventExpr = input$button2, {
    visLibrarySalaries(
      dataARL = csvInput(),
      institute = as.character(input$instituteInput),
      years = as.vector(input$yearsInput, mode = "numeric"))
  })

  # plot - salariesUserInstitute
  output$salariesUserInstitute <- renderPlot({
    startvisualizing5()[[1]]
  })

  # plot - salariesExpComp
  output$salariesExpComp <- renderPlot({
    startvisualizing5()[[2]]
  })

  # plot - salariesInstCanadian
  output$salariesInstCanadian <- renderPlot({
    startvisualizing5()[[3]]
  })

  # plot - salariesInstType
  output$salariesInstType <- renderPlot({
    startvisualizing5()[[4]]
  })

  # plot - salariesAcademicPlot
  output$salariesAcademicPlot <- renderPlot({
    startvisualizing5()[[5]]
  })

  # plot - salariesARLRankTop
  output$salariesARLRankTop <- renderPlot({
    startvisualizing5()[[6]]
  })


  # URLs for downloading data
  url1 <- a("Example Dataset 2", href="https://raw.githubusercontent.com/anjalisilva/TestingPackage/master/inst/extdata/GeneCountsData2.csv")
  output$tab1 <- renderUI({
    tagList("Download:", url1)
  })



  # -- Staff Counts (FTE)
  startvisualizing6 <- eventReactive(eventExpr = input$button2, {
    visStaffCounts(
      dataARL = csvInput(),
      institute = as.character(input$instituteInput),
      years = as.vector(input$yearsInput, mode = "numeric"))
  })

  # plot - staffFTEUserInstitute
  output$staffFTEUserInstitute <- renderPlot({
    startvisualizing6()[[1]]
  })

  # plot - staffFTEInstCanadian
  output$staffFTEInstCanadian <- renderPlot({
    startvisualizing6()[[2]]
  })

  # plot - staffFTEInstType
  output$staffFTEInstType <- renderPlot({
    startvisualizing6()[[3]]
  })

  # plot - staffFTEAcademicPlot
  output$staffFTEAcademicPlot <- renderPlot({
    startvisualizing6()[[4]]
  })

  # plot - staffFTEARLRankTop
  output$staffFTEARLRankTop <- renderPlot({
    startvisualizing6()[[5]]
  })

  # plot - staffFTEComp
  output$staffFTEComp <- renderPlot({
    startvisualizing6()[[6]]
  })


  observeEvent(input$data1, {
    # Show a modal when the button is pressed
    shinyalert(title = "Example Dataset 1",
               text = "This is a simulated dataset generated from mixtures of multivariate Poisson log-normal
               distributions with G = 2 components. It has a size of n = 1000 observations along rows and d = 6
               samples along columns. Data was generated January, 2022. To save the file, click on link, then click 'Download' from the top right side.
               Citation: Silva, A., S. J. Rothstein, P. D. McNicholas, and S. Subedi (2019). A multivariate Poisson-log normal
               mixture model for clustering transcriptome sequencing data. BMC Bioinformatics. 2019;20(1):394. URL https://pubmed.ncbi.nlm.nih.gov/31311497/",
               type = "info")
  })


}

# Create Shiny app ----
shinyApp(ui, server)
