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
    ),
    f7Row(
      f7Col(
        f7Card(
          h2(class = "center", span("Daily Unique Trend"), style = paste0("color:", "#fff", ";")),
          plotly::plotlyOutput(ns("country_plot_unique"))
        )
      )
    ),
    f7Row(
      f7Col(
        f7Card(
          h2(class = "center", span("Top Affected Countries Trend (Logarithmic)"), style = paste0("color:", "#fff", ";")),
          plotly::plotlyOutput(ns("country_plot_log"))
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
    daily_recovered <- extract_daily_trend(recovered_global(), "recovered")
    daily <- merge(daily_confirmed, daily_deaths)
    daily <- merge(daily, daily_recovered)
    
    plot_ly(daily, x = ~date, y = ~confirmed, name = "Confirmed" , type = 'scatter', mode = 'lines+markers') %>% 
      add_trace(y = ~death, name = 'Deaths', mode = 'lines+markers') %>% 
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
  
  output$country_plot_unique <- plotly::renderPlotly({
    confirmed_daily <- extract_daily_trend(confirmed_global(), "confirmed")  %>%
      mutate(unique_confirmed = confirmed - lag(confirmed, default = confirmed[1]))
    
    death_daily <- extract_daily_trend(death_global(), "death") %>%
      mutate(unique_death = death - lag(death, default = death[1]))
    
    recovered_daily <- extract_daily_trend(recovered_global(), "recovered") %>%
      mutate(unique_recovered = recovered - lag(recovered, default = recovered[1]))
    
    
    daily <- merge(confirmed_daily, death_daily)
    daily <- merge(daily, recovered_daily)
    
    plot_ly(daily, x = daily$date, y = daily$unique_death, name = "Daily Death" , type = 'bar') %>% 
      add_trace(y = daily$unique_recovered, name = 'Daily Recovered') %>%
      add_trace(y = daily$unique_confirmed, name = 'Daily confiremd') %>%
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
             hoverdistance = 10,
             barmode = "stack"
      )
  })
  
  output$country_plot_log <- plotly::renderPlotly({
    country_confirmed_count <- extract_country_data(confirmed_global(), "confirmed")
    country_confirmed_count <- country_confirmed_count[order(country_confirmed_count$confirmed, decreasing = TRUE),]
    first <- data.frame()
    second <- data.frame()
    third <- data.frame()
    fourth <- data.frame()
    fifth <- data.frame()
    sixth <- data.frame()
    countries <- country_confirmed_count[1:6,1]
    # countries <- levels(droplevels(countries))
    
    for(i in 1:length(countries)){
      if(i==1){
        first <- extract_country_daily_trend(confirmed_global(), country = countries[i], "confirmed")%>%
          mutate(unique_confirmed = confirmed - lag(confirmed, default = confirmed[1]))
        smooth_first = loess(first$unique_confirmed~first$confirmed, span=0.95)
      } else if(i==2){
        second <- extract_country_daily_trend(confirmed_global(), country = countries[i], "confirmed")%>%
          mutate(unique_confirmed = confirmed - lag(confirmed, default = confirmed[1]))
        smooth_second = loess(second$unique_confirmed~second$confirmed, span=0.95)
      } else if(i==3){
        third <- extract_country_daily_trend(confirmed_global(), country = countries[i], "confirmed")%>%
          mutate(unique_confirmed = confirmed - lag(confirmed, default = confirmed[1]))
        smooth_third = loess(third$unique_confirmed~third$confirmed, span=0.95)
      } else if(i==4){
        fourth <- extract_country_daily_trend(confirmed_global(), country = countries[i], "confirmed")%>%
          mutate(unique_confirmed = confirmed - lag(confirmed, default = confirmed[1]))
        smooth_fourth = loess(fourth$unique_confirmed~fourth$confirmed, span=0.95)
      } else if(i==5){
        fifth <- extract_country_daily_trend(confirmed_global(), country = countries[i], "confirmed")%>%
          mutate(unique_confirmed = confirmed - lag(confirmed, default = confirmed[1]))
        smooth_fifth = loess(fifth$unique_confirmed~fifth$confirmed, span=0.95)
      } else if(i==6){
        sixth <- extract_country_daily_trend(confirmed_global(), country = countries[i], "confirmed")%>%
          mutate(unique_confirmed = confirmed - lag(confirmed, default = confirmed[1]))
        smooth_sixth = loess(sixth$unique_confirmed~sixth$confirmed, span=0.95)
      }
    }
    
    line.fmt = list(dash="solid",width = 3.0, color=NULL)
    
    
    plot_ly(x = first$confirmed, y =  predict(smooth_first), type="scatter", mode="lines", line = line.fmt, name=countries[1]) %>%
      add_lines(second$confirmed, predict(smooth_second), line=line.fmt, mode = "lines", name = countries[2]) %>%
      add_lines(third$confirmed, predict(smooth_third), line=line.fmt, mode = "lines", name = countries[3]) %>%
      add_lines(fourth$confirmed, predict(smooth_fourth), line=line.fmt, mode = "lines", name = countries[4]) %>%
      add_lines(fifth$confirmed, predict(smooth_fifth), line=line.fmt, mode = "lines", name = countries[5]) %>%
      add_lines(sixth$confirmed, predict(smooth_sixth), line=line.fmt, mode = "lines", name = countries[6]) %>%
      layout(paper_bgcolor = 'transparent',
             plot_bgcolor = "transparent",
             xaxis = list(
               type = "log",
               showspikes = TRUE,
               spikemode  = 'across',
               spikesnap = 'cursor',
               spikedash = "solid",
               spikecolor = '#ffffff',
               spikethickness = 0,
               showline=TRUE,
               color = "#ffffff",
               zeroline = TRUE,
               showline = TRUE,
               showticklabels = TRUE,
               showgrid = FALSE
             ),
             yaxis = list(
               type = "log",
               zeroline = TRUE,
               showline = TRUE,
               title = 'Count',
               color = '#ffffff',
               showticklabels = TRUE,
               showgrid = TRUE,
               gridcolor = toRGB("gray30")
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
  
  
  
  
  
  extract_daily_trend <- function(input, type = "confirmed"){
    input <- input[-1]
    input <- as.data.frame(dplyr::group_by(input, Country.Region) %>% dplyr::summarise_all(sum))
    country <- data.frame("country" = input$Country.Region)
    input<- input[-1:-3]
    input <- as.data.frame(colSums(input))
    dateSeq <- data.frame("date"=seq(as.Date("2020/1/22"), as.Date(format(Sys.Date()-1,"%Y/%m/%d")), "days"))
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
    
extract_country_daily_trend <- function(input, country = "India", type = "confirmed"){
  input <- input[-1]
  input <- as.data.frame(dplyr::group_by(input, Country.Region) %>% dplyr::summarise_all(sum))
  data <- subset(input, Country.Region == country)
  data<- data[-1:-3]
  data <- t(data)
  dateSeq <- data.frame("date"=seq(as.Date("2020/1/22"), as.Date(format(Sys.Date()-1,"%Y/%m/%d")), "days"))
  dataset <- data.frame("date"=dateSeq, data)
  rownames(dataset) <- 1:nrow(dataset)
  colnames(dataset) <- c("date", type)
  return(dataset)
}
## To be copied in the UI
# mod_world_map_ui("world_map_ui_1")
    
## To be copied in the server
# callModule(mod_world_map_server, "world_map_ui_1")