library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(DT)

ui <- dashboardPage(skin="red",
  dashboardHeader(title = "Wine Dashboard"),
  dashboardSidebar(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    sidebarMenu(
      br(),
      selectInput("chooseDataset", "Choose Wine Type",
                             choices = c("All", "Red", "Rose", "Sparkling", "White"),
                             selected = "All"),
      br(),
      menuItem("Basic Informations", tabName = "BasicInfo", icon = icon("info")),
      menuItem("Details Informations", tabName = "DetaiInfo", icon = icon("book")),
      menuItem("Prices in Countries", tabName = "MoreInformations", icon = icon("book-open")),
      menuItem("Ratings", tabName = "Ratings", icon = icon("star")),
      menuItem("World Map", tabName = "World", icon = icon("globe"))
    ),
    tags$img(src = "PP_logotyp_ANG_RGB.png", height = 40, width = 220)
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tabItems(
      tabItem(tabName = "BasicInfo", 
              fluidRow(
                box(
                  width = 12,
                  valueBoxOutput("rowCountBox"),
                  valueBoxOutput("rowPriceBox"),
                  footer = uiOutput("wineImage", style = "width: 100%; height: 100%;")
                )
              ),
              fluidRow(
                box(
                  width = 12,
                  plotlyOutput("regionPlot"),
                  style = "width: 100%; overflow-x: scroll;"
                 
                )
              )
      ),
      tabItem(tabName = "DetaiInfo",
              fluidPage(
              fluidRow(
                column(
                  width = 8,
                  offset = 2,
                  align = "center",
                  div(
                    style = "color: #ffffff; background-color: #ad3c3c; height: 85px; border: 6px double white",
                    h3(style = "font-size: 30px; text-align:center;", "Interactive Table for Wines")
                  )
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  DTOutput("table"),
                  style = "margin-top: 25px"
                  
                )
              )
              )

      ),
      tabItem(tabName = "MoreInformations",
              fluidPage(
                box(
                  title = "Choose Year and Country to get the most expensive wine ",
                  selectInput(inputId = "year",
                              label = "Year",
                              choices = NULL
                  ),
                  selectInput(inputId = "country",
                              label = "Country",
                              choices = NULL
                  ),
                  tags$img(src = "expensiveWine.jpg", height = 300, width = "100%"),
                  verbatimTextOutput("expensiveWineInfo")
                  
                ),
                box(
                  title =" TU BEDZIE WYKRES zaleznosci ceny od roku / moze i  kraju"
                )
              ))
    )
  )
)

server <- function(input, output, session) { # Dodaj argument `session`
  
  # choosing dataset basing on the input
  current_data <- reactive({
    switch(input$chooseDataset,
           "All" = read.csv("datasets/all.csv"),
           "Red" = read.csv("datasets/Red.csv"),
           "Rose" = read.csv("datasets/Rose.csv"),
           "Sparkling" = read.csv("datasets/Sparkling.csv"),
           "White" = read.csv("datasets/White.csv")
           
    )
  })
  
  output$wineImage <- renderUI({
    image_path <- switch(input$chooseDataset,
                         "All" = "all.jpg",
                         "Red" = "red_wine.jpg",
                         "Rose" = "rose_wine.jpg",
                         "Sparkling" = "sparkling_wine.jpg",
                         "White" = "white_wine.jpg")
    
    tags$img(src = image_path, height = 450, width = "100%")
  })
  
  # making value box 1
  output$rowCountBox <- renderValueBox({
    valueBox(
      nrow(current_data()),
      "Number of different wines",
      icon = icon("wine-glass"),
      color="red"
    )
  })
  
  output$rowPriceBox <- renderValueBox({
    avg_price <- mean(current_data()$Price, na.rm = TRUE)
    avg_price_rounded <- round(avg_price, 2)
    
    valueBox(
      avg_price_rounded,
      "Average Price",
      icon = icon("dollar-sign"),
      color = "red"
    )
  })
  
  # making value box 2
  output$rowCountBox2 <- renderValueBox({
    valueBox(
      nrow(current_data()),
      "Other Data",
      icon = icon("info"),
      color = "blue"
    )
  })
  
  # making barplot for top 5 frequent regions
  output$regionPlot <- renderPlotly({
    
    data_aggregated <- current_data() %>%
      count(Region, Country) 
    
    data_aggregated$n <- as.numeric(data_aggregated$n)
    
    data_aggregated <- data_aggregated %>%
      arrange(desc(n)) %>%
      slice(1:5)
    
    country_colors <- c("#F51313", "#BD0E0E", "#9E0C0C", "#7A0909", "#4F0606")
    
    
    plot_ly(data_aggregated, 
            x = ~n, 
            y = ~Region, 
            type = 'bar', 
            hovertemplate = "Amount of wine:<br> %{x}",
            color = ~Country,  # Assigning color based on Country
            colors = country_colors,  # Using custom color scale
            showlegend = TRUE,
            width = 800,
            height = 400) %>%
      layout(title = "Top 5 Regions by Frequency",
             xaxis = list(title = "Frequency"), 
             yaxis = list(title = "Region"),
             margin = list(t = 100)) 
    
    
  })
  
  # The prettyTable function
  prettyTable <- function(table_df, round_columns_func = is.numeric, round_digits = 2, width = NULL) {
    DT::datatable(
      table_df,
      style = "bootstrap",
      filter = "top",
      rownames = FALSE,
      options = list(
        dom = 'Bfrtip',
        scrollX = TRUE,
        autoWidth = TRUE,
        scrollY = NULL,
        width = width
      )
    ) %>%
      formatRound(unlist(lapply(table_df, round_columns_func)), round_digits)
  }
  
  # Display the first table
  output$table <- renderDT({
    prettyTable(current_data())
  })
  
  observe({
    data <- current_data()
    updateSelectInput(session, "year", label = "Year", choices = unique(data$Year))
    updateSelectInput(session, "country", label = "Country", choices = unique(data$Country))
  })
  
  output$expensiveWineInfo <- renderText({
    req(input$year, input$country)
    
    filtered_data <- current_data() %>%
      filter(Year == input$year, Country == input$country)
    
    expensive_wine <- filtered_data[which.max(filtered_data$Price), ]
    
    expensive_wine_info <- paste("Name: ", expensive_wine$Name, "\n",
                                 "Price: ", expensive_wine$Price, "\n",
                                 "Country: ", expensive_wine$Country)
    
    HTML(expensive_wine_info)
  })
  
  
  
  
}

# Run the application
shinyApp(ui = ui, server = server)
