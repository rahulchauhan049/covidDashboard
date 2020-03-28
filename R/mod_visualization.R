#' visualization UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_visualization_ui <- function(id){
  choices <- read.csv("data/country_name.csv")
  ns <- NS(id)
  tagList(
    f7Row(
      f7Col(
        f7Card(
          h2(class = "center", span("Top 10 Countries with confirmed cases"), style = paste0("color:", "#fff", ";")),
          plotly::plotlyOutput(ns("top_countries"))
        )
      )
    ),
    f7Row(
      f7Col(
        f7Card(
          h2(class = "center", span("Daily Trend Of Countries"), style = paste0("color:", "#fff", ";")),
          f7Select(
            ns("country_name"), "Select Country Name", choices = choices$unique.d.Country.Region.,
            selected = "India"
          ),
          plotly::plotlyOutput(ns("country_plot"))
        )
      )
    )
  )
}
    
#' visualization Server Function
#'
#' @noRd 
mod_visualization_server <- function(input, output, session, confirmed_data, death_data, recovered_data){
  ns <- session$ns
 
  output$top_countries <- plotly::renderPlotly({
    country_confirmed_count <- extract_country_data(confirmed_data(), "confirmed")
    country_deaths_count <- extract_country_data(death_data(), "deaths")
    country_recovered_count <- extract_country_data(recovered_data(), "recovered")
    
    daily_country <- merge(country_confirmed_count, country_deaths_count)
    daily_country <- merge(daily_country, country_recovered_count)
    daily_country <- daily_country[with(daily_country, order(confirmed, decreasing  = TRUE)), ]
    
    df <- daily_country[1:10,]
    rownames(df) <- 1:nrow(df)
    df <- droplevels(df)
    
    plot_ly(df, x = df$country, y = df$confirmed, name = "Confirmed" , type = 'bar') %>% 
      add_trace(y = df$deaths, name = 'Deaths') %>% 
      add_trace(y = df$recovered, name = 'Recovered') %>% 
      layout(paper_bgcolor = 'transparent',
             plot_bgcolor = "transparent",
             xaxis = list(
               color = "#ffffff",
               showline=TRUE,
               zeroline = TRUE,
               showline = TRUE,
               showticklabels = TRUE,
               showgrid = FALSE
             ),
             yaxis = list(
               color = "#ffffff",
               title = 'Count',
               showticklabels = TRUE,
               showline = TRUE,
               showgrid = TRUE,
               gridcolor = toRGB("gray50")
             ),
             legend = list(
               x = 0,
               y = 1,
               orientation = 'h',
               font = list(
                 color = "#ffffff"
               )
             ),
             hovermode  = 'x',
             spikedistance = 300,
             hoverdistance = 10
      )
  })
  
  # observe({
  #   choices = unique(confirmed_data()$Country.Region)
  #   
  #   # Can also set the label and select items
  #   updateSelectInput(session, "country_name", 
  #                     "Select Country Name",
  #                     choices = choices,
  #                     selected = "India"
  #   )
  # })
  
  output$country_plot <- plotly::renderPlotly({
    daily_confirmed <- extract_country_daily_trend(confirmed_data(), input$country_name, "confirmed")
    daily_deaths <- extract_country_daily_trend(death_data(), input$country_name, "deaths")
    daily_recovered <- extract_country_daily_trend(recovered_data(), input$country_name, "recovered")
    
    daily <- merge(daily_confirmed, daily_deaths)
    daily <- merge(daily, daily_recovered)
    
    plot_ly(daily, x = ~date, y = ~confirmed, name = "Confirmed" , type = 'scatter', mode = 'lines+markers') %>% 
      add_trace(y = ~deaths, name = 'Deaths', mode = 'lines+markers') %>% 
      add_trace(y = ~recovered, name = 'Recovered', mode = 'lines+markers') %>% 
      layout(paper_bgcolor = 'transparent',
             plot_bgcolor = "transparent",
             xaxis = list(
               showspikes = TRUE,
               spikemode  = 'across',
               spikesnap = 'cursor',
               spikedash = "solid",
               spikecolor = '#ffffff',
               spikethickness = 1,
               showline=TRUE,
               color = "#ffffff",
               zeroline = TRUE,
               showline = TRUE,
               showticklabels = TRUE,
               showgrid = FALSE
             ),
             yaxis = list(
               zeroline = TRUE,
               showline = TRUE,
               title = 'Count',
               color = '#ffffff',
               showticklabels = TRUE,
               showgrid = TRUE,
               gridcolor = toRGB("gray50")
             ),
             legend = list(
               x = 0,
               y = 1,
               orientation = 'h',
               font = list(
                 color = "#ffffff"
               )
             ),
             showlegend = TRUE,
             hovermode  = 'x',
             spikedistance = 300,
             hoverdistance = 10
      )
  })
  
  extract_country_data <- function(input, type = "confirmed"){
    input <- input[-1]
    input <- as.data.frame(dplyr::group_by(input, Country.Region) %>% dplyr::summarise_all(sum))
    country <- data.frame("country" = input$Country.Region)
    input <- as.data.frame(input[,ncol(input)])
    dataset <- data.frame("country" = country, "count"=input)
    colnames(dataset) <- c("country", type)
    return(dataset)
  }
  
  extract_country_daily_trend <- function(input, country = "India", type = "confirmed"){
    input <- input[-1]
    input <- as.data.frame(dplyr::group_by(input, Country.Region) %>% dplyr::summarise_all(sum))
    data <- subset(input, Country.Region == country)
    data<- data[-1:-3]
    data <- t(data)
    time <- rownames(data)
    month <- as.numeric(substr(time[length(time)], 2, 2))
    date <- as.numeric(substr(time[length(time)], 4, 5))
    dateEnd <- paste0("2020/",month,"/",date)
    dateSeq <- data.frame("date"=seq(as.Date("2020/1/22"), as.Date(dateEnd), "days"))
    dataset <- data.frame("date"=dateSeq, data)
    rownames(dataset) <- 1:nrow(dataset)
    colnames(dataset) <- c("date", type)
    return(dataset)
  }
}
    
## To be copied in the UI
# mod_visualization_ui("visualization_ui_1")
    
## To be copied in the server
# callModule(mod_visualization_server, "visualization_ui_1")
 
