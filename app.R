library(shiny)
library(shinydashboard)
library(ggplot2)


# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Wine Dashboard"),
  titlePanel("More Widgets"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Red Wine", tabName = "RedWine", icon = icon("wine-glass")),
      menuItem("Rose Wine", tabName = "RoseWine", icon = icon("wine-bottle")),
      menuItem("Sparkling Wine", tabName = "SparklingWine", icon = icon("fas fa-wine-glass-alt")),
      menuItem("White Wine", tabName = "WhiteWine", icon = icon("wine-glass")),
      img(src = "PP_monogram.jpg", class = "sidebar-img")
    )
  ),
  dashboardBody(
    tags$head(
      includeCSS("www/custom.css")
    ),
    tabItems(
      tabItem(tabName = "RedWine"
              #fluidRow(
               # box(plotOutput("plot1"), width = 6),
                #box(tableOutput("table1"), width = 6)
              #)
      ),
      tabItem(tabName = "RoseWine",
              h2("Another tab content")
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
}

# Run the application
shinyApp(ui = ui, server = server)
