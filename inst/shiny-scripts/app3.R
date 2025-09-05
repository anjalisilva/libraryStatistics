# Sample for trying tabs

library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Custom Background with Tabs"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
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
      tabItem(tabName = "dashboard",
              h2("Dashboard Content"),
              p("This is the dashboard tab.")
      ),

      tabItem(tabName = "charts",
              h2("Charts Section"),
              plotOutput("plot")
      ),

      tabItem(tabName = "tables",
              h2("Tables Section"),
              tableOutput("table")
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
