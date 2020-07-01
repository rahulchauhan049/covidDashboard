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
            span(countup::countupOutput(ns("cnt")), style = paste0("color:", "#fff", ";"), class = "count"),
            span(br(), "Cases", class = "count-small")
          )
        )
      ),
      f7Col(
        f7Card(
          h2(
            class = "center",
            span(countup::countupOutput(ns("death")), style = paste0("color:", "#fff", ";"), class = "count"),
            span(br(), "Deaths", class = "count-small")
          )
        )
      )
    ),
    h3("Countries in Worst Situations", class = "center"),
    f7Row(
      f7Col(
        f7Card(
          h3("Country with maximum cases", class = "center"),
          h2(
            class = "center",
            span(countup::countupOutput(ns("max_case")), verbatimTextOutput(ns("max_case_country")), style = paste0("color:", "#fff", ";"), class = "count")
          )
        )
      ),
      f7Col(
        f7Card(
          h3("Country with Maximum Deaths", class = "center"),
          h2(
            class = "center",
            span(countup::countupOutput(ns("max_deaths")), verbatimTextOutput(ns("max_deaths_country")), style = paste0("color:", "#fff", ";"), class = "count")          )
        )
      )
    ),
      # h3("Death Rate in Different Age Group", class = "center"),
    # f7Row(
    #   f7Col(
    #     f7Card(
    #       h2(
    #         class = "center",
    #         span(verbatimTextOutput(ns("death_age_below_thirty")), style = paste0("color:", "#fff", ";"), class = "count"),
    #         span("Death Rate in Age 0 to 30", class = "count-small")
    #       )
    #     )
    #   ),
    #   f7Col(
    #     f7Card(
    #       h2(
    #         class = "center",
    #         span(verbatimTextOutput(ns("thirtyone_to_sixty")), style = paste0("color:", "#fff", ";"), class = "count"),
    #         span("Death Rate in Age 31 to 60", class = "count-small")
    #       )
    #     )
    #   ),
    #   f7Col(
    #     f7Card(
    #       h2(
    #         class = "center",
    #         span(verbatimTextOutput(ns("sixty_plus")), style = paste0("color:", "#fff", ";"), class = "count"),
    #         span("Death Rate in Age 60+", class = "count-small")
    #       )
    #     )
    #   ),
    # ),
    h3("Most Affected Continents", class = "center"),
    f7Row(
      f7Col(
        f7Card(
          h3("Region with Maximum Cases", class = "center"),
          h2(
            class = "center",
            span(countup::countupOutput(ns("max_cases_region_count")), verbatimTextOutput(ns("max_cases_region_name")), style = paste0("color:", "#fff", ";"), class = "count")
          )
        )
      ),
      f7Col(
        f7Card(
          h3("Region with Maximum Deaths", class = "center"),
          h2(
            class = "center",
            span(countup::countupOutput(ns("max_deaths_region_count")), verbatimTextOutput(ns("max_deaths_region_name")), style = paste0("color:", "#fff", ";"), class = "count")
          )
        )
      )
    ),
    # h3("Total Affected/Died Percentage", class = "center"),
    # f7Row(
    #   f7Col(
    #     f7Card(
    #       h2(
    #         class = "center",
    #         span(verbatimTextOutput(ns("male")), style = paste0("color:", "#fff", ";"), class = "count"),
    #         span("% of Infected Male Died", class = "count-small")
    #       )
    #     )
    #   ),
    #   f7Col(
    #     f7Card(
    #       h2(
    #         class = "center",
    #         span(verbatimTextOutput(ns("female")), style = paste0("color:", "#fff", ";"), class = "count"),
    #         span("% of Infected Femal Died", class = "count-small")
    #       )
    #     )
    #   )
    # )
  )
}
    
#' summary Server Function
#'
#' @noRd 
mod_summary_server <- function(input, output, session, countries_data){
  ns <- session$ns
  output$cnt <- renderCountup({
    df <- countries_data()
    sum(as.numeric(gsub(",", "", as.character(df$Cases)))) %>%
    countup()
  })
  
  output$death <- renderCountup({
    df <- countries_data()
    sum(as.numeric(gsub(",", "", as.character(df$Deaths)))) %>%
      countup()
  })
  
  output$max_case <- renderCountup({
    df <- countries_data()
    df$Cases <- as.numeric(gsub(",", "", as.character(df$Cases)))
    df <- df[which.max(df$Cases),]
    df$Cases %>%
      countup()
  })
  
  output$max_case_country <- renderText({
    df <- countries_data()
    df$Cases <- as.numeric(gsub(",", "", as.character(df$Cases)))
    df <- df[which.max(df$Cases),]
    df$Country
  })
  
  output$max_deaths <- renderCountup({
    df <- countries_data()
    df$Deaths <- as.numeric(gsub(",", "", as.character(df$Deaths)))
    df <- df[which.max(df$Deaths),]
    df$Deaths %>%
      countup()
  })
  
  output$max_deaths_country <- renderText({
    df <- countries_data()
    df$Deaths <- as.numeric(gsub(",", "", as.character(df$Deaths)))
    df <- df[which.max(df$Deaths),]
    df$Country
  })
  
  output$max_cases_region_count <- renderCountup({
    df <- countries_data()
    df$Cases <- as.numeric(gsub(",", "", as.character(df$Cases)))
    df <- df[c("Cases", "Region")]
    df <- dplyr::group_by(df, Region) %>% dplyr::summarise_all(sum)
    df <- df[which.max(df$Cases),]
    df$Cases %>%
      countup()
  })
  
  output$max_cases_region_name <- renderText({
    df <- countries_data()
    df$Cases <- as.numeric(gsub(",", "", as.character(df$Cases)))
    df <- df[c("Cases", "Region")]
    df <- dplyr::group_by(df, Region) %>% dplyr::summarise_all(sum)
    df <- df[which.max(df$Cases),]
    df$Region
  })
  
  output$max_deaths_region_count <- renderCountup({
    df <- countries_data()
    df$Deaths <- as.numeric(gsub(",", "", as.character(df$Deaths)))
    df <- df[c("Deaths", "Region")]
    df <- dplyr::group_by(df, Region) %>% dplyr::summarise_all(sum)
    df <- df[which.max(df$Deaths),]
    df$Deaths %>%
      countup()
  })
  
  output$max_deaths_region_name <- renderText({
    df <- countries_data()
    df$Deaths <- as.numeric(gsub(",", "", as.character(df$Deaths)))
    df <- df[c("Deaths", "Region")]
    df <- dplyr::group_by(df, Region) %>% dplyr::summarise_all(sum)
    df <- df[which.max(df$Deaths),]
    df$Region
  })
  
  # output$death_age_below_thirty <- renderText({
  #   df <- fatility_data()
  #   df <- na.omit(as.numeric(gsub("%", "", as.character(df$DeathRateAllCases))))
  #   paste0(sum(df[7:9], na.rm = TRUE),"%")
  # })
  # 
  # output$thirtyone_to_sixty <- renderText({
  #   df <- fatility_data()
  #   df <- na.omit(as.numeric(gsub("%", "", as.character(df$DeathRateAllCases))))
  #   paste0(sum(df[4:6], na.rm = TRUE),"%")
  # })
  # 
  # output$sixty_plus <- renderText({
  #   df <- fatility_data()
  #   df <- na.omit(as.numeric(gsub("%", "", as.character(df$DeathRateAllCases))))
  #   paste0(sum(df[1:3], na.rm = TRUE),"%")
  # })
  
  # output$male <- renderText({
  #   df <- sex_data()
  #   df$DeathRateConfirmedCases[1]
  # })
  # 
  # output$female <- renderText({
  #   df <- sex_data()
  #   df$DeathRateConfirmedCases[2]
  # })
  # 
  
}
    
## To be copied in the UI
# mod_summary_ui("summary_ui_1")
    
## To be copied in the server
# callModule(mod_summary_server, "summary_ui_1")\