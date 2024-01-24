library(shiny)
library(shinyalert)

# Define UI for random distribution app ----
ui <- fluidPage(

  # App title ----
  titlePanel(tags$h1(tags$b("libraryStatistics:"),"Visualize Association of Research Libraries (ARL) Survey Stats")),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      tags$p("Description: This is a Shiny App that is part of the libraryStatistics
             R package. Most of the functions available via the package are made
             available with Shiny App. The libraryStatistics is an R package for
             analyzing and visualizing library statistics published from the annual
             survey of Association of Research Libraries."),

      # br() element to introduce extra vertical spacing ----
      br(),
      br(),

      # input
      tags$p("Instructions: Below, upload a dataset, and enter values
              to perform analysis. Default values are shown.
              Then press 'Analyze'. Navigate through the different tabs
              to the right to explore the results."),

      # br() element to introduce extra vertical spacing ----
      br(),
      br(),
      br(),
      # input
      shinyalert::useShinyalert(),  # Set up shinyalert
      uiOutput("tab2"),
      actionButton(inputId = "data1",
                   label = "Demo Dataset Details"),
      fileInput(inputId = "file1",
                label = "Dataset: Upload a dataset below to analyze. File should be
                in comma-separated value (.csv) format with rows corresponding
                to years and columns to variables. The first column must
                be 'Year', followed by other variables in no particular order,
                e.g., 'Institution Name', 'Institution type', etc.",
                accept = c(".csv")),
      textInput(inputId = "institute",
                label = "Institute: A character vector specifying the institute
                of interest, as identified in the dataset. E.g., 'TORONTO' for
                University of Toronto Libraries.",
                value = "TORONTO"),
      checkboxGroupInput(inputId = "yearsInput",
                label = "Years: A numeric vector specifying up to 5 calendar years
                for which data should be plotted, e.g., c(2015, 2016, 2017, 2018, 2019).
                If no value is provided (i.e., NA), then most recent five years
                available in the dataset will be automatically selected. If more than
                5 values provided, last 5 values will be selected. Default is NA."),

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
                  tabPanel("Titles",
                           h3("Comparison of Titles with Median and Institute of Selction"),
                           br(),
                           fluidRow(
                             splitLayout(cellWidths = c("50%", "50%"), plotOutput("titleUserInstitute"), plotOutput('InstCanadianPlot')),
                             splitLayout(cellWidths = c("50%", "50%"), plotOutput("instTypePlot"), plotOutput('academicPlot')),
                             splitLayout(cellWidths = c("50%", "50%"), plotOutput("plotARLRankTop"), plotOutput('')),
                           )),
                  tabPanel("Expenditure",
                           h3("Instructions: Enter values and click 'Run' at the bottom left side."),
                           h3("Alluvial Plot Showing Observation Memberships by Information Criteria for Input Dataset:"),
                           h5("Note, below the x-axis values are in the order of BIC, ICL, AIC, AIC3.
                              Colors are assigned based on cluster membership of model selected via BIC."),
                           br(),
                           fluidRow(
                             splitLayout(cellWidths = c("100%"), plotOutput("alluvialPlot")),
                             h5("Note, below the x-axis values are in the order of ICL, BIC, AIC, AIC3.
                              Colors are assigned based on cluster membership of model selected via ICL."),
                             splitLayout(cellWidths = c("100%"), plotOutput("alluvialPlot2")),
                             h5("Note, below the x-axis values are in the order of AIC3, ICL, BIC, AIC
                              Colors are assigned based on cluster membership of model selected via AIC3."),
                             splitLayout(cellWidths = c("100%"), plotOutput("alluvialPlot3")),
                             h5("Note, below the x-axis values are in the order of AIC, AIC3, ICL, BIC
                              Colors are assigned based on cluster membership of model selected via AIC."),
                             splitLayout(cellWidths = c("100%"), plotOutput("alluvialPlot4")),
                           )),
                  tabPanel("Barplot",
                           h3("Instructions: Enter values and click 'Run' at the bottom left side."),
                           h3("Barplot of Posterior Probabilities with Cluster Memberships:"),
                           h5("Note, the plots are in the order of models selected by: BIC (top, left), ICL (top, right) and AIC (bottom, left), AIC3 (bottom, right)."),
                           br(),
                           fluidRow(
                             splitLayout(cellWidths = c("50%", "50%"), plotOutput("barPlotBIC"), plotOutput('barPlotICL')),
                             splitLayout(cellWidths = c("50%", "50%"), plotOutput("barPlotAIC3"), plotOutput('barPlotAIC'))
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
    updateCheckboxGroupInput(session, "yearsInput",
                             label = NULL,
                             choices = columns,
                             selected = NULL)
  })

  startvisualizing <- eventReactive(eventExpr = input$button2, {
    visTitlesData(dataARL = csvInput(),
                        institute = as.character(input$institute),
                        years = input$yearsInput)
  })


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


  # URLs for downloading data
  url1 <- a("Example Dataset 2", href="https://raw.githubusercontent.com/anjalisilva/TestingPackage/master/inst/extdata/GeneCountsData2.csv")
  output$tab1 <- renderUI({
    tagList("Download:", url1)
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
