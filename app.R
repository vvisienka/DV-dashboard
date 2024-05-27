library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Wine Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Basic Informations", tabName = "BasicInfo", icon = icon("info")),
      menuItem("Details Informations", tabName = "DetaiInfo", icon = icon("book")),
      menuItem("World Map", tabName = "World", icon = icon("globe")),
      menuItem("Ratings", tabName = "Ratings", icon = icon("star"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "BasicInfo",
              fluidRow(
                column(width = 2,
                       selectInput("chooseDataset", "Choose Wine Type",
                                   choices = c("Red", "Rose", "Sparkling", "White"),
                                   selected = "Red")
                ),
                column(width = 12,
                       valueBoxOutput("rowCountBox")
                ),
                column(width = 8,
                       plotlyOutput("regionPlot")
                )
              )
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # choosing dataset basing on the input
  current_data <- reactive({
    switch(input$chooseDataset,
           "Red" = read.csv("datasets/Red.csv"),
           "Rose" = read.csv("datasets/Rose.csv"),
           "Sparkling" = read.csv("datasets/Sparkling.csv"),
           "White" = read.csv("datasets/White.csv")
    )
  })
  
  # making value box
  output$rowCountBox <- renderValueBox({
    valueBox(
      nrow(current_data()),
      "Number of different wines",
      icon = icon("wine-glass"),
      color = "red"
    )
  })
  
  # making barplot for top 5 frequent regions
  output$regionPlot <- renderPlotly({
    
    data_aggregated <- current_data() %>%
      count(Region, Country) 
    
    # nie dziala :(
    data_aggregated$n <- as.numeric(data_aggregated$n)
    
    data_aggregated <- data_aggregated %>%
      arrange(desc(n))
    # to pomiedzy :(
    
    data_aggregated <- data_aggregated %>%
      slice(1:5)
    
    plot_ly(data_aggregated, 
            x = ~n, 
            y = ~Region, 
            type = 'bar', 
            hovertemplate = "Amount of wine:<br> %{x}",
            name = ~Country,  
            orientation = 'h',  
            marker = list(color = c("#F51313","#BD0E0E","#9E0C0C","#7A0909","#4F0606")),  # Dodanie kolorów w zależności od kraju
            showlegend = TRUE) %>%  
      layout(title = "Top 5 Regions by Frequency",
             xaxis = list(title = "Frequency"), 
             yaxis = list(title = "Region")) 
    
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
