#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny httr jsonlite
#' @noRd
app_server <- function( input, output, session ) {
  # List the first level callModules here
  api_url <- "https://covid19-server.chrismichael.now.sh/api/v1/"
  data_store <-
    shiny::reactiveValues(
      countries_where_virus_has_spread = data.frame(),
      fatality_rate_by_age = data.frame(),
      fatality_rate_by_sex = data.frame(),
      daily_deaths = data.frame(),
      all_reports = data.frame()
    )
  
  data_store$countries_where_virus_has_spread <- reactive({
    data <- jsonlite::fromJSON(paste0(api_url,"CountriesWhereCoronavirusHasSpread"))
    return(data$table)
  })
  
  data_store$fatality_rate_by_age <- reactive({
    data <- jsonlite::fromJSON(paste0(api_url,"FatalityRateByAge"))
    return(data$table)
  })
  
  data_store$fatality_rate_by_sex <- reactive({
    data <- jsonlite::fromJSON(paste0(api_url,"FatalityRateBySex"))
    return(data$table)
  })
  
  callModule(
    mod_summary_server,
    "summary_ui_1", 
    data_store$countries_where_virus_has_spread,
    data_store$fatality_rate_by_age,
    data_store$fatality_rate_by_sex
    )

}
