#' summary UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_summary_ui <- function(id){
  ns <- NS(id)
  tagList(
    h2("World Stats of Covid-19", class = "heading-out", class = "center"),
    h4("Data by John Hopkins", class = "heading-out", class = "center"),
    h5("Made By Rahul", class = "heading-out", class = "center"),
    
    f7Row(
      f7Col(
        f7Card(
          h2(
            class = "center",
            span(countup::countupOutput(ns("NewConfirmed")), style = paste0("color:", "#fff", ";"), class = "count"),
            span(br(), "NewConfirmed cases", class = "count-small")
          )
        )
      ),
      f7Col(
        f7Card(
          h2(
            class = "center",
            span(countup::countupOutput(ns("TotalConfirmed")), style = paste0("color:", "#fff", ";"), class = "count"),
            span(br(), "TotalConfirmed Cases", class = "count-small")
          )
        )
      )
    ),
    h3("Deaths Summary", class = "center"),
    f7Row(
      f7Col(
        f7Card(
          h3("NewDeaths Count", class = "center"),
          h2(
            class = "center",
            span(countup::countupOutput(ns("NewDeaths")), style = paste0("color:", "#fff", ";"), class = "count")
          )
        )
      ),
      f7Col(
        f7Card(
          h3("TotalDeaths Count", class = "center"),
          h2(
            class = "center",
            span(countup::countupOutput(ns("TotalDeaths")), style = paste0("color:", "#fff", ";"), class = "count")          )
        )
      )
    ),
   
    h3("Recovery Summary", class = "center"),
    f7Row(
      f7Col(
        f7Card(
          h3("NewRecovered Count", class = "center"),
          h2(
            class = "center",
            span(countup::countupOutput(ns("NewRecovered")), style = paste0("color:", "#fff", ";"), class = "count")
          )
        )
      ),
      f7Col(
        f7Card(
          h3("TotalRecovered Count", class = "center"),
          h2(
            class = "center",
            span(countup::countupOutput(ns("TotalRecovered")), style = paste0("color:", "#fff", ";"), class = "count")
          )
        )
      )
    )
  )
}
    
#' summary Server Function
#'
#' @noRd 
mod_summary_server <- function(input, output, session, summary_data){
  ns <- session$ns
  output$NewConfirmed <- renderCountup({
    df <- summary_data()
    sum(as.numeric(gsub(",", "", as.character(df$NewConfirmed)))) %>%
    countup()
  })
  
  output$TotalConfirmed <- renderCountup({
    df <- summary_data()
    sum(as.numeric(gsub(",", "", as.character(df$TotalConfirmed)))) %>%
      countup()
  })
  
  output$NewDeaths <- renderCountup({
    df <- summary_data()
    df <- as.numeric(gsub(",", "", as.character(df$NewDeaths)))
    df %>%
      countup()
  })
  

  output$TotalDeaths <- renderCountup({
    df <- summary_data()
    df <- as.numeric(gsub(",", "", as.character(df$TotalDeaths)))
    df %>%
      countup()
  })
  
  
  output$NewRecovered <- renderCountup({
    df <- summary_data()
    df <- as.numeric(gsub(",", "", as.character(df$NewRecovered)))
    df %>%
      countup()
  })
  

  
  output$TotalRecovered <- renderCountup({
    df <- summary_data()
    df <- as.numeric(gsub(",", "", as.character(df$TotalRecovered)))
    df %>%
      countup()
  })
  

  
}