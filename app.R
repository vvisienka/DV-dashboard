library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(DT)
library(shinyWidgets)
library(rnaturalearth)
library(sf)
library(ggplot2)


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
      menuItem("About", tabName = "About", icon = icon("question")),
      menuItem("Wine Overview", tabName = "BasicInfo", icon = icon("info")),
      menuItem("Details Information", tabName = "DetaiInfo", icon = icon("book")),
      menuItem("Prices in Countries", tabName = "MoreInformations", icon = icon("dollar-sign")),
      menuItem("Ratings", tabName = "RatingPrice", icon = icon("star")),
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
                  footer = uiOutput("wineImage", style = "width: 70%; height: 90%; margin: 0 auto;")
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
                fluidRow(
                  box(
                    width = 6,
                    
                    h3(style = "font-size: 30px; text-align:center; margin-bottom: 25px", "The most expensive Wine"),
                    style = "border: 2px solid #ad3c3c;", 
                    sliderInput("sliderYear", "Year:",sep="",
                                min = 1961, max = 2020, value = c(1990,2012), step = 1),
                    box(
                      height = 100,
                      width = 12,
                      title = "Name",
                      textOutput("expensiveWineName")
                    ),
                    
                    box(
                      title = "Price",
                      height = 90,
                      textOutput("expensiveWinePrice")
                    ),
                    
                    box(
                      height = 90,
                      title = "Country",
                      textOutput("expensiveWineCountry")
                    )
                    
                  ),
                  box(
                    width = 6,
                    height = 434,
                    style = "border: 2px solid #ad3c3c;", 
                    tags$img(src = "expensiveWine2.jpg", height = "408px", width = "375px", style = "display: block; margin: 0 auto;")
                    
                  )),
                box(
                  width = 14,
                  style = "border: 2px solid #990c0c;", 
                  plotlyOutput("winePlot")
                )
              )),
      tabItem(tabName="RatingPrice",
              fluidPage(
                box(
                  width = 12,
                  offset = 6,
                  align = "center",
                  div(
                    style = "color: #ffffff; background-color: #ad3c3c; height: 85px; border: 6px double white",
                    h3(style = "font-size: 30px; text-align:center;", "Rating's Influence on the Price")
                  )),
                fluidRow(
                
                  box(
                    width = 6,
                    height = 610,
                    plotlyOutput("ratingPlot")
                  ),
                  box(
                    width = 6,
                    height = 610,
                    style = "overflow-y: auto;",                     
                    DTOutput("topWinesTable")
                  )
                )
              )
      ),
      tabItem(tabName = "World",
              fluidPage(
                box(
                  width = 12,
                  plotlyOutput("worldMap", height = "520px")
                ),
                box(
                  width = 12,
                  plotlyOutput("regionPlot"),
                  style = "width: 100%; overflow-x: scroll;"
                )
              )
      ),
      tabItem(tabName="About",
              fluidPage(
                div(
                  style = "color: #ffffff; background-color: #ad3c3c; width: 80%; height: 485px; border: 6px double white; margin: 0 auto;",
                  h3(style = "font-size: 30px; text-align:center;", "Hey there!"),
                  h4(style = "font-size: 20px; text-align:center;", "Ready to uncork the ultimate wine adventure?"),
                  h4(style = "font-size: 20px; text-align:center;", "Our Wine Dashboard's got all the cool stuff you need to become a grape guru! 🍇🎉 "),
                  br(),br(),
                  h4(style = "font-size: 16px; text-align:center;", "1. Quick wine facts – no boring lectures, check the features of the data. 🍷"),
                  h4(style = "font-size: 16px; text-align:center;", "2. Deep dive into the deets – for serious wine buffs only! 🤓"),
                  h4(style = "font-size: 16px; text-align:center;", "3. Check out global wine prices – find the best deals worldwide. 💸"),
                  h4(style = "font-size: 16px; text-align:center;", "4. Explore how ratings affect prices – wine math made easy! 📊"),
                  h4(style = "font-size: 16px; text-align:center;", "5. See which wine regions are buzzing with activity. 🌍"),
                  br(), br(),
                  h4(style = "font-size: 20px; text-align:center;", "So, what are you waiting for? Let's pop the cork and pour out some fun!"),
                  h4(style = "font-size: 20px; text-align:center;", "Cheers to good times and great wine! 🍷✨")
              )
              ))
      
    )
  )
)

years <- as.character(2000:2022)


server <- function(input, output, session) {
  help_data = NULL
  help_data_2 = NULL
  value <- reactiveValues(selected_years = NULL,selected_country = NULL)
  
  
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
      paste(avg_price_rounded,"$"),
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
      slice(1:5) %>%
      arrange(n)
    
    data_aggregated$Region <- factor(data_aggregated$Region, levels = data_aggregated$Region)
    
    country_colors <- c("#F51313", "#BD0E0E", "#9E0C0C", "#7A0909", "#4F0606")
    
    plot_ly(data_aggregated, 
            x = ~n, 
            y = ~Region, 
            type = 'bar', 
            hovertemplate = "Number of wines:<br> %{x}",
            color = ~Country,  
            colors = country_colors, 
            showlegend = TRUE,
            width = 800,
            height = 400) %>%
      layout(
        title = list(text = "Top 5 Regions: Most Frequent Wine Producers", x = 0.5), 
        xaxis = list(title = "Frequency"), 
        yaxis = list(title = list(text = "Region", standoff = 30)),
        margin = list(l = 250, r = 20)
      )
    
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
  
  # plot from prices in countires
  output$winePlot <- renderPlotly({
    
    selected_years <- seq(input$sliderYear[1], input$sliderYear[2])
    
    help_data <- current_data()
    
    help_data <- help_data %>%
      filter(Year != "N.V.") %>%
      mutate(Year = as.numeric(Year)) %>%
      filter(Year %in% selected_years)
    
    help_data11 <- current_data()
    
    help_data11 <- help_data %>%
      group_by(Year) %>%
      summarise(avg_price = round(mean(Price, na.rm = TRUE),2))
    
    
    help_data <- help_data %>%
      group_by(Year, Country) %>%
      summarise(avg_price = round(mean(Price, na.rm = TRUE),2))
    
    
    plot_ly(source = "plot_wine") %>%
      add_trace(data = help_data,
                x = ~ Year,
                y = ~ avg_price,
                color = ~ Country,
                colors = c("#67000d","#a50f15","#cb181d","#ef3b2c","#fb6a4a","#ec7014","#fc9272","#fcbba1","#fe9929","#fdd0a2","#fee0d2",
                           "#deebf7","#c6dbef","#a6bddb","#9ecae1","#6baed6","#4292c6","#2171b5","#08519c","#253494","#08306b","#081d58",
                           "#004529","#006837","#238443","#41ab5d","#78c679","#addd8e","#d9f0a3","#f7fcb9"),
                type = "scatter",
                mode = "markers",
                marker = list(size = 10),
                hovertemplate = ~paste("Rok: ", Year, "\n Average price: ", avg_price, "$"),
                customdata = ~Country
      ) %>%
      add_trace(data = help_data11,
                x = ~ Year,
                y = ~ avg_price,
                type = "scatter",
                mode = "lines",
                line = list(color = "#A30446"),
                name = "Average Price for Year"
      ) %>%
      layout(title = "Average price in different Countries through Years",
             yaxis = list(title = "Average price ($)"),
             xaxis = list(title = "Years",
                          tickmode = "linear",
                          dtick = 1,
                          tickangle = 45)) %>%
      event_register("plotly_click")
    
  })
  
  observeEvent(event_data(event = "plotly_click", source = "plot_wine"), {
    clicked <- event_data(event = "plotly_click", source = "plot_wine")
    if (!is.null(clicked)) {
      value$selected_country <- clicked$customdata
      value$selected_years <- clicked$x
    }
  })
  
  
  output$expensiveWineName <- renderPrint({
    validate(need(value$selected_years, message = "Click point on the plot to get the most expensive wine from chosen Year and Country "))
    
    help_data_2 <- current_data()
    
    help_data_2 <- help_data_2 %>%
      filter(Year != "N.V.") %>%
      mutate(Year = as.numeric(Year)) %>%
      filter(Year == value$selected_years, Country == value$selected_country)
    
    expensive_wine <- help_data_2[which.max(help_data_2$Price), ]
    
    expensive_wine_info <- paste(expensive_wine$Name)
    HTML(expensive_wine_info)
  })
  
  output$expensiveWinePrice <- renderPrint({
    validate(need(value$selected_years, message =""))
    
    help_data_2 <- current_data()
    
    help_data_2 <- help_data_2 %>%
      filter(Year != "N.V.") %>%
      mutate(Year = as.numeric(Year)) %>%
      filter(Year == value$selected_years, Country == value$selected_country)
    
    expensive_wine <- help_data_2[which.max(help_data_2$Price), ]
    
    expensive_wine_info <- paste(expensive_wine$Price, "$")
    HTML(expensive_wine_info)
  })
  
  output$expensiveWineCountry <- renderPrint({
    validate(need(value$selected_years, message =""))
    
    expensive_wine_info <- paste(value$selected_country)
    HTML(expensive_wine_info)
  })
  
  


  output$ratingPlot <- renderPlotly({
    rating_data <- current_data() %>%
      mutate(Rating_Group = ifelse(Rating < 3.5, "<3.5", as.character(Rating))) %>%
      group_by(Rating_Group) %>%
      summarize(median_price = median(Price, na.rm = TRUE))
    
    plot_ly(rating_data, 
            x = ~Rating_Group,
            y = ~median_price, 
            type = 'bar',
            hovertemplate = "Median Price: $%{y} <br> Rating: %{x}",
            name = ' ',
            height = 500,
            marker = list(color = '#ad3c3c')) %>%
      layout(
        title = "Median Price Distribution by Rating",
        xaxis = list(title = "Rating"),
        yaxis = list(title = "Median Price ($)"),
        margin = list(t = 100)
  )})
  
  
  output$topWinesTable <- renderDT({
    filtered_data <- current_data() %>%
      mutate(rating_price_ratio = round(Rating / Price, digits=3)) %>%
      select(Name, rating_price_ratio) %>%
      arrange(desc(rating_price_ratio)) %>%
      rename(`Rating/Price Ratio` = rating_price_ratio)
    
    datatable(filtered_data)
  })
  
  output$worldMap <- renderPlotly({
    # Read world map data
    world_map <- ne_countries(scale = "medium", returnclass = "sf") %>%
      filter(admin != "Antarctica") %>%
      arrange(admin)
    
    target_crs <- "+proj=moll"
    world_moll <- world_map %>% st_transform(crs = target_crs)
    
    # Aggregating the data by country
    world_data <- current_data() %>%
      group_by(Country) %>%
      summarise(
        wine_count = n(),
        avg_price = round(mean(Price, na.rm = TRUE), 2),
        avg_rating = round(mean(Rating, na.rm = TRUE), 2)
      ) %>%
      ungroup() %>%
      select(wine_count, avg_price, avg_rating, Country)
    
    world_data <- world_data %>%
      mutate(info = paste(
        "<br>",
        "Country: ", Country, "<br>",
        "Number of Wines: ", wine_count, "<br>",
        "Average Price: $", avg_price, "<br>",
        "Average Rating: ", avg_rating
      ))
    
    # Merge map data with aggregated wine data
    base_map <- world_moll %>%
      left_join(world_data, by = c("admin" = "Country")) %>%
      ggplot(aes(fill = wine_count, label = info)) +
      geom_sf() +
      scale_fill_gradient(low = "#cc8585", high = '#6b0808') +
      labs(fill = "Number of Wines", title = "World Map of Wines") +
      theme(plot.title = element_text(hjust = 0.5))
    
    ggplotly(base_map, tooltip = "label")
  })
  
  
}
# Run the application
shinyApp(ui = ui, server = server)
