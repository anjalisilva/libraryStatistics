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

             tags$p("Instructions: This is the Shiny application that is part of the libraryStatistics
             R package. The libraryStatistics is an R package for
             analyzing and visualizing library statistics published from the annual
             survey of Association of Research Libraries (ARL). First upload a dataset
             downloaded from ARL Data Portal. To download data from ARL Data Portal, it
             is recommended that all variables are selected, with columns being 'Variables'
             and data sorted by 'Institution Name' (default options). Once data is uploaded, the list of
             choices for 'Institute' and 'Years' based on uploaded dataset
             will appear here. Select one institute and upto 5 years, and press 'Analyze'.
             Explore the results by navigating the tabs to the right. If choices are
             later altered, press 'Analyze' again to update results."),

                 # br() element to introduce extra vertical spacing ----
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
                to years and columns to ARL indicators (variables). The first column must
                be 'Year', followed by other indicators in no particular order,
                e.g., 'Institution Name', 'Institution type', etc. as directly
                downloaded
                from ARL Data Portal.",
                           accept = c(".csv")),
                 checkboxGroupInput(inputId = "instituteInput",
                                    label = "ARL Member: Select upto 5 choices"),
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
    mainPanel(width = 9,

              # Output: Tabet
              tabsetPanel(type = "tabs",
                          tabPanel("Library Expenditures",
                                   h3("Total Library Expenditures as Overall Maximums vs User Selected Institutes", align = "center"),
                                   br(),
                                   fluidRow(
                                     splitLayout(cellWidths = c("50%", "50%"), plotOutput("tleTopPerFaculty"), plotOutput(" ")),

                                   )),
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
    updateCheckboxGroupInput(session,
                             "instituteInput",
                             label = NULL,
                             choices = columns2,
                             selected = columns2[2])
  })

  # -- Total Library Expenditures
  expVisualization <- eventReactive(eventExpr = input$button2, {
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
