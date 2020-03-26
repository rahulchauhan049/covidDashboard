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
    f7Row(
      f7Col(
        f7Card(
          h2(
            class = "center",
            span(verbatimTextOutput(ns("cnt")), style = paste0("color:", "#fff", ";"), class = "count"),
            span("Cases", class = "count-small")
          )
        )
      ),
      f7Col(
        f7Card(
          h2(
            class = "center",
            span(verbatimTextOutput(ns("death")), style = paste0("color:", "#fff", ";"), class = "count"),
            span("Deaths", class = "count-small")
          )
        )
      )
    ),
    f7Row(
      f7Col(
        span("Country with maximum cases", class = "heading-out"),
        f7Card(
          h2(
            class = "center",
            span(verbatimTextOutput(ns("max_case")), verbatimTextOutput(ns("max_case_country")), style = paste0("color:", "#fff", ";"), class = "count")
          )
        )
      ),
      f7Col(
        span("Country with Maximum Deaths", class = "heading-out"),
        f7Card(
          h2(
            class = "center",
            span(verbatimTextOutput(ns("max_deaths")), verbatimTextOutput(ns("max_deaths_country")), style = paste0("color:", "#fff", ";"), class = "count")          )
        )
      )
    ),
    f7Row(
      f7Col(
        f7Card(
          h2(
            class = "center",
            span(verbatimTextOutput(ns("death_age_below_thirty")), style = paste0("color:", "#fff", ";"), class = "count"),
            span("Death Rate in Age 0 to 30", class = "count-small")
          )
        )
      ),
      f7Col(
        f7Card(
          h2(
            class = "center",
            span(verbatimTextOutput(ns("thirtyone_to_sixty")), style = paste0("color:", "#fff", ";"), class = "count"),
            span("Death Rate in Age 31 to 60", class = "count-small")
          )
        )
      ),
      f7Col(
        f7Card(
          h2(
            class = "center",
            span(verbatimTextOutput(ns("sixty_plus")), style = paste0("color:", "#fff", ";"), class = "count"),
            span("Death Rate in Age 60+", class = "count-small")
          )
        )
      ),
    ),
    f7Row(
      f7Col(
        span("Region with Maximum Cases", class = "heading-out"),
        f7Card(
          h2(
            class = "center",
            span(verbatimTextOutput(ns("max_cases_region_count")), verbatimTextOutput(ns("max_cases_region_name")), style = paste0("color:", "#fff", ";"), class = "count")
          )
        )
      ),
      f7Col(
        span("Region with Maximum Deaths", class = "heading-out"),
        f7Card(
          h2(
            class = "center",
            span(verbatimTextOutput(ns("max_deaths_region_count")), verbatimTextOutput(ns("max_deaths_region_name")), style = paste0("color:", "#fff", ";"), class = "count")
          )
        )
      )
    ),
    f7Row(
      f7Col(
        f7Card(
          h2(
            class = "center",
            span(verbatimTextOutput(ns("male")), style = paste0("color:", "#fff", ";"), class = "count"),
            span("% of Infected Male Died", class = "count-small")
          )
        )
      ),
      f7Col(
        f7Card(
          h2(
            class = "center",
            span(verbatimTextOutput(ns("female")), style = paste0("color:", "#fff", ";"), class = "count"),
            span("% of Infected Femal Died", class = "count-small")
          )
        )
      )
    )
  )
}
    
#' summary Server Function
#'
#' @noRd 
mod_summary_server <- function(input, output, session, countries_data, fatility_data, sex_data){
  ns <- session$ns
  output$cnt <- renderText({
    df <- countries_data()
    sum(as.numeric(gsub(",", "", as.character(df$Cases))))
  })
  
  output$death <- renderText({
    df <- countries_data()
    sum(as.numeric(gsub(",", "", as.character(df$Deaths))))
  })
  
  output$max_case <- renderText({
    df <- countries_data()
    df$Cases <- as.numeric(gsub(",", "", as.character(df$Cases)))
    df <- df[which.max(df$Cases),]
    df$Cases
  })
  
  output$max_case_country <- renderText({
    df <- countries_data()
    df$Cases <- as.numeric(gsub(",", "", as.character(df$Cases)))
    df <- df[which.max(df$Cases),]
    df$Country
  })
  
  output$max_deaths <- renderText({
    df <- countries_data()
    df$Deaths <- as.numeric(gsub(",", "", as.character(df$Deaths)))
    df <- df[which.max(df$Deaths),]
    df$Deaths
  })
  
  output$max_deaths_country <- renderText({
    df <- countries_data()
    df$Deaths <- as.numeric(gsub(",", "", as.character(df$Deaths)))
    df <- df[which.max(df$Deaths),]
    df$Country
  })
  
  output$max_cases_region_count <- renderText({
    df <- countries_data()
    df$Cases <- as.numeric(gsub(",", "", as.character(df$Cases)))
    df <- df[c("Cases", "Region")]
    df <- dplyr::group_by(df, Region) %>% dplyr::summarise_all(sum)
    df <- df[which.max(df$Cases),]
    df$Cases
  })
  
  output$max_cases_region_name <- renderText({
    df <- countries_data()
    df$Cases <- as.numeric(gsub(",", "", as.character(df$Cases)))
    df <- df[c("Cases", "Region")]
    df <- dplyr::group_by(df, Region) %>% dplyr::summarise_all(sum)
    df <- df[which.max(df$Cases),]
    df$Region
  })
  
  output$max_deaths_region_count <- renderText({
    df <- countries_data()
    df$Deaths <- as.numeric(gsub(",", "", as.character(df$Deaths)))
    df <- df[c("Deaths", "Region")]
    df <- dplyr::group_by(df, Region) %>% dplyr::summarise_all(sum)
    df <- df[which.max(df$Deaths),]
    df$Deaths
  })
  
  output$max_deaths_region_name <- renderText({
    df <- countries_data()
    df$Deaths <- as.numeric(gsub(",", "", as.character(df$Deaths)))
    df <- df[c("Deaths", "Region")]
    df <- dplyr::group_by(df, Region) %>% dplyr::summarise_all(sum)
    df <- df[which.max(df$Deaths),]
    df$Region
  })
  
  output$death_age_below_thirty <- renderText({
    df <- fatility_data()
    df <- na.omit(as.numeric(gsub("%", "", as.character(df$DeathRateAllCases))))
    paste0(sum(df[7:9], na.rm = TRUE),"%")
  })
  
  output$thirtyone_to_sixty <- renderText({
    df <- fatility_data()
    df <- na.omit(as.numeric(gsub("%", "", as.character(df$DeathRateAllCases))))
    paste0(sum(df[4:6], na.rm = TRUE),"%")
  })
  
  output$sixty_plus <- renderText({
    df <- fatility_data()
    df <- na.omit(as.numeric(gsub("%", "", as.character(df$DeathRateAllCases))))
    paste0(sum(df[1:3], na.rm = TRUE),"%")
  })
  
  output$male <- renderText({
    df <- sex_data()
    df$DeathRateConfirmedCases[1]
  })
  
  output$female <- renderText({
    df <- sex_data()
    df$DeathRateConfirmedCases[2]
  })
  
  
}
    
## To be copied in the UI
# mod_summary_ui("summary_ui_1")
    
## To be copied in the server
# callModule(mod_summary_server, "summary_ui_1")
 
