#' world_map UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' 
mod_world_map_ui <- function(id){
  ns <- NS(id)
  tagList(
    f7Row(
      f7Col(
        f7Card(
          h2(
            class = "center",
            span(countup::countupOutput(ns("cases")), style = paste0("color:", "#fff", ";"), class = "count"),
            span("Total Cases", class = "count-small")
          )
        )
      ),
      f7Col(
        f7Card(
          h2(
            class = "center",
            span(countup::countupOutput(ns("deaths")), style = paste0("color:", "#fff", ";"), class = "count"),
            span("Total Deaths", class = "count-small")
          )
        )
      ),
      f7Col(
        f7Card(
          h2(
            class = "center",
            span(countup::countupOutput(ns("recovered")), style = paste0("color:", "#fff", ";"), class = "count"),
            span("Total Recovered", class = "count-small")
          )
        )
      ),
      f7Col(
        f7Card(
          h2(
            class = "center",
            span(countup::countupOutput(ns("country")), style = paste0("color:", "#fff", ";"), class = "count"),
            span("Countries Affected", class = "count-small")
          )
        )
      )
    ),
    f7Row(
      f7Col(
        f7Card(
          highcharter::highchartOutput(ns("map_world"))
        )
      )
    ),
    f7Row(
      f7Col(
        f7Card(
          plotly::plotlyOutput(ns("time_series"))
        )
      )
    )
  )
}
    
#' world_map Server Function
#'
#' @noRd 
mod_world_map_server <- function(input, output, session, confirmed_global, death_global, recovered_global){
  ns <- session$ns
  
  output$cases <- renderCountup({
    country_confirmed_count <- extract_country_data(confirmed_global(), "confirmed")
    df <- country_confirmed_count
    sum(df$confirmed) %>%
      countup()
  })
  
  output$deaths <- renderCountup({
    country_confirmed_count <- extract_country_data(death_global(), "deaths")
    df <- country_confirmed_count
    sum(df$deaths)%>%
      countup()
  })
  
  output$recovered <- renderCountup({
    country_confirmed_count <- extract_country_data(recovered_global(), "recovered")
    df <- country_confirmed_count
    sum(df$recovered)%>%
      countup()
  })
  
  output$country <- renderCountup({
    country_confirmed_count <- extract_country_data(confirmed_global(), "confirmed")
    df <- country_confirmed_count
    a <- (unique(df$country))
    length(a)%>%
      countup()
  })
  
  # output$table <- renderFormattable({
  #   df <- country_data()
  #   df$country[df$country  == "US"] = "United States of America"
  #   df$confirmed <- df$stats$confirmed
  #   df$deaths <- df$stats$deaths
  #   df$recovered <- df$stats$recovered
  #   
  #   df <- df[c("country", "confirmed", "deaths", "recovered")]
  #   df <- dplyr::group_by(df, country) %>% dplyr::summarise_all(sum)
  #   formattable(
  #     df,
  #     list(`Indicator Name` = formatter(
  #       "span", style = ~ style(color = "grey",font.weight = "bold")),
  #       "confirmed" = color_tile("#ff7f7f", "#ee7f7f")
  #     )
  #   )
  # })
  
  output$map_world <- highcharter::renderHighchart({
    country_confirmed_count <- extract_country_data(confirmed_global(), "confirmed")
    country_deaths_count <- extract_country_data(death_global(), "deaths")
    country_recovered_count <- extract_country_data(recovered_global(), "recovered")
    
    daily_country <- merge(country_confirmed_count, country_deaths_count)
    daily_country <- merge(daily_country, country_recovered_count)
    df <- daily_country
    df$country <- as.character(df$country)
    df$country[df$country  == "US"] = "United States of America"


    data(worldgeojson, package = "highcharter")
    highchart() %>%
      hc_add_series_map(worldgeojson, df, value = 'confirmed', joinBy = c('name','country'))  %>% 
      #hc_colors(c("darkorange", "darkgray")) %>% 
      hc_colorAxis(stops = color_stops()) %>% 
      hc_title(text = "People with confirmed Cornonavirus cases") %>%
      hc_tooltip(crosshairs = TRUE, backgroundColor = "gray",
                 headerFormat = "",
                 pointFormat = "{point.country} <br>
             Confirmed: {point.confirmed} <br>
             Deaths: {point.deaths} <br>
             Recovered: {point.recovered}",
                 shared = TRUE, borderWidth = 5)
  })
  
  output$time_series <- plotly::renderPlotly({
    daily_confirmed <- extract_daily_trend(confirmed_global(), "confirmed")
    daily_deaths <- extract_daily_trend(death_global(), "death")
    daily <- merge(daily_confirmed, daily_deaths)
    
    plot_ly(daily, x = ~date, y = ~confirmed, name = "Confirmed" , type = 'scatter', mode = 'lines+markers') %>% 
      add_trace(y = ~death, name = 'Deaths', mode = 'lines+markers') %>% 
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
  
  extract_daily_trend <- function(input, type = "confirmed"){
    input <- input[-1]
    input <- as.data.frame(dplyr::group_by(input, Country.Region) %>% dplyr::summarise_all(sum))
    country <- data.frame("country" = input$Country.Region)
    input<- input[-1:-3]
    input <- as.data.frame(colSums(input))
    
    time <- rownames(input)
    month <- as.numeric(substr(time[length(time)], 2, 2))
    date <- as.numeric(substr(time[length(time)], 4, 5))
    dateEnd <- paste0("2020/",month,"/",date)
    dateSeq <- data.frame("date"=seq(as.Date("2020/1/22"), as.Date(dateEnd), "days"))
    dataset <- data.frame("date"=dateSeq, type=input)
    rownames(dataset) <- 1:nrow(dataset)
    colnames(dataset) <- c("date", type)
    return(dataset)
  }
  
  extract_country_data <- function(input, type = "confirmed"){
    input <- input[-1]
    input <- as.data.frame(dplyr::group_by(input, Country.Region) %>% dplyr::summarise_all(sum))
    country <- data.frame("country" = input$Country.Region)
    input <- as.data.frame(input[,ncol(input)])
    dataset <- data.frame("country" = country, "count"=input)
    colnames(dataset) <- c("country", type)
    return(dataset)
  }
}
    
## To be copied in the UI
# mod_world_map_ui("world_map_ui_1")
    
## To be copied in the server
# callModule(mod_world_map_server, "world_map_ui_1")