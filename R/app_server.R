#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny jsonlite
#' @noRd
app_server <- function( input, output, session ) {
  # List the first level callModules here
  api_url <- "https://covid19-server.chrismichael.now.sh/api/v1/"
  api_key <- read.delim("api_key.txt")
  data_store <-
    shiny::reactiveValues(
      countries_where_virus_has_spread = data.frame(),
      # fatality_rate_by_age = data.frame(),
      # fatality_rate_by_sex = data.frame(),
      country_wize_data = data.frame(),
      time_series_covid19_deaths_global = data.frame(),
      time_series_covid19_confirmed_global = data.frame(),
      time_series_covid19_recovered_global = data.frame(),
      news = data.frame()
    )
  

  data_store$countries_where_virus_has_spread <- reactive({
    data <- jsonlite::fromJSON(paste0(api_url,"CountriesWhereCoronavirusHasSpread"))
    return(data$table)
  })

  # data_store$fatality_rate_by_age <- reactive({
  #   data <- jsonlite::fromJSON(paste0(api_url,"FatalityRateByAge"))
  #   return(data$table)
  # })
  # 
  # data_store$fatality_rate_by_sex <- reactive({
  #   data <- jsonlite::fromJSON(paste0(api_url,"FatalityRateBySex"))
  #   return(data$table)
  # })
  # 


  data_store$time_series_covid19_deaths_global <- reactive({
    data <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
    return(data)
  })

  data_store$time_series_covid19_confirmed_global <- reactive({
    data <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
    return(data)
  })
  
  data_store$time_series_covid19_recovered_global <- reactive({
    data <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")
    return(data)
  })
  
  data_store$news <- reactive({
    data <- jsonlite::fromJSON(paste0("https://newsapi.org/v2/top-headlines?country=in&apiKey=",api_key$x,"&q=coronavirus"))
    return(data$articles)
  })

  # -------------------- Load tab by tab for more responsiveness
  data_summary_init <- world_data_init <- visualization_init <- news_init <- TRUE
  w <- waiter::Waiter$new(html = loader, color = "#000")
  observeEvent(input$tabs, {
    if(all(input$tabs == "Data Summary", data_summary_init)){
      w$show()
      data_summary_init <- FALSE
      callModule(
        mod_summary_server,
        "summary_ui_1",
        data_store$countries_where_virus_has_spread
        # data_store$fatality_rate_by_age,
        # data_store$fatality_rate_by_sex
      )
      w$hide()
    }
    else if(all(input$tabs == "World Data", world_data_init)){
      w$show()
      world_data_init <- FALSE
      callModule(
        mod_world_map_server,
        "world_map_ui_1",
        data_store$time_series_covid19_confirmed_global,
        data_store$time_series_covid19_deaths_global,
        data_store$time_series_covid19_recovered_global
      )
      w$hide()
    } else if(all(input$tabs == "Visualization", visualization_init)){
      w$show()
      visualization_init <- FALSE
      
      callModule(
        mod_visualization_server,
        "visualization_ui_1",
        data_store$time_series_covid19_confirmed_global,
        data_store$time_series_covid19_deaths_global,
        data_store$time_series_covid19_recovered_global
      )
      w$hide()
    } else if(all(input$tabs == "News", news_init)){
      w$show()
      news_init <- FALSE
      
      callModule(mod_news_server, "news_ui_1", data_store$news)
      w$hide()
    }
  })
  
  callModule(mod_knowledge_server, "knowledge_ui_1")
}
