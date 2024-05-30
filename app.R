library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(DT)
library(shinyWidgets)

ui <- dashboardPage(skin="red",
  dashboardHeader(title = "Wine Dashboard"),
  dashboardSidebar(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    sidebarMenu(
      br(),
      tags$div(class='select',selectInput("chooseDataset", "Choose Wine Type",
                             choices = c("All", "Red", "Rose", "Sparkling", "White"),
                             selected = "All")),
      br(),
      menuItem("Basic Informations", tabName = "BasicInfo", icon = icon("info")),
      menuItem("Details Informations", tabName = "DetaiInfo", icon = icon("book")),
      menuItem("Prices in Countries", tabName = "MoreInformations", icon = icon("book-open")),
      menuItem("Ratings", tabName = "Ratings", icon = icon("star")),
      menuItem("World Map", tabName = "World", icon = icon("globe"))
    ),
    tags$img(src = "PP_logotyp_ANG_WHITE.png", height = 40, width = 220)
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
                  valueBoxOutput("rowAvgPriceBox"),
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
              
                box(
                  width = 12,
                  offset = 6,
                  align = "center",
                  div(
                    style = "color: #ffffff; background-color: #ad3c3c; height: 85px; border: 6px double white",
                    h3(style = "font-size: 30px; text-align:center;", "Interactive Table for Wines")
                  )
                
              ),
              fluidRow(
                box(
                  width = 12,
                  DTOutput("table"),
                  style = "margin-top: 25px"
                  
                )
              )
              )

      ),
      tabItem(tabName = "MoreInformations",
              fluidPage(
                chooseSliderSkin("Flat"),
                
                box(
                  width = 3,
                  title = "The most expensive wine in choosen Year and Country",
                  tags$img(src = "expensiveWine.jpg", height = "100%", width = "100%"),
                  
                ),
                box(
                  width = 3,
                  verbatimTextOutput("expensiveWineInfo")
                ),
                box(
                  width = 6,
                  title = "choosing the year for wine to get the mean price and the most expensive",
                  sliderInput("sliderYear", "Year:",sep="",
                              min = 1961, max = 2020, value = c(1990,2000), step = 1)
                ),
                box(
                  width = 12,
                  plotlyOutput("winePlot")
                )
              ))
    )
  )
)

years <- as.character(2000:2022)


server <- function(input, output, session) {
  help_data = NULL
  help_data_2 = NULL
  value_Years <- reactiveValues(selected_years = NULL)
  value_Country <- reactiveValues(selected_country = NULL)
  
  
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
  
  # making value box 2
  output$rowPriceBox <- renderValueBox({
    avg_price <- mean(current_data()$Price, na.rm = TRUE)
    avg_price_rounded <- round(avg_price, 2)
    
    valueBox(
      avg_price_rounded,
      "Average Price",
      icon = icon("dollar-sign"),
      color = "red",
    )
  })
  
  # making value box 3
  output$rowAvgPriceBox <- renderValueBox({
    avg_rating <- mean(current_data()$Rating, na.rm = TRUE)
    avg_rating <- round(avg_rating, 2)
    
    valueBox(
      avg_rating,
      "Average Rating",
      icon = icon("star"),
      color = "orange"
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
            color = ~Country,  
            colors = country_colors, 
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
  

  # plot from prices in countires
  output$winePlot <- renderPlotly({
    
    selected_years <- seq(input$sliderYear[1], input$sliderYear[2])
    
    help_data <- current_data()
    
    help_data <- help_data %>%
      filter(Year != "N.V.") %>%
      mutate(Year = as.numeric(Year)) %>%
      filter(Year %in% selected_years)
    
    help_data <- help_data %>%
      group_by(Year, Country) %>%
      summarise(avg_price = mean(Price, na.rm = TRUE))
    
    
    plot_ly(source = "plot_wine") %>%
      add_trace(data = help_data,
                x = ~ Year,
                y = ~ avg_price,
                color = ~Country,
                type = "bar") %>%
      layout(title = "Avergae price in choosen Year in different Countries",
             yaxis = list(title = "Average price"),
             xaxis = list(title = "Years")) %>%
      event_register("plotly_click")
  })
  
  observeEvent(event_data(event = "plotly_click", source = "plot_wine"),
     {
       clicked = event_data(event = "plotly_click",source = "plot_wine")
       if (!is.null(clicked)) {
         value_Years$selected_Years = clicked$x
       }
     })
  
  
  output$expensiveWineInfo <- renderText({
    validate(need(value_Years$selected_Years, message = "Click choosen point on the plot"))
    
    help_data_2 <- current_data()
    
    help_data_2 <- help_data_2 %>%
      filter(Year != "N.V.") %>%
      mutate(Year = as.numeric(Year)) %>%
      filter(Year == value_Years$selected_Years)
    
    expensive_wine <- help_data_2[which.max(help_data_2$Price), ]
    
    expensive_wine_info <- paste("Name: ", expensive_wine$Name, "\n",
                                 "Price: ", expensive_wine$Price, "\n",
                                 "Country: ", expensive_wine$Country)
    
    HTML(expensive_wine_info)
  })
  
  
  
  
}

# Run the application
shinyApp(ui = ui, server = server)
