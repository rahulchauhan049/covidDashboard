#' news UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_news_ui <- function(id){
  ns <- NS(id)
  tagList(
    f7Block(
      h2("News", class = "center"),
      p(tags$small("Latest articles on the coronavirus."), class = "center"),
      inset = TRUE
    ),
    f7Block(uiOutput(ns("articles")))
  )
}
    
#' news Server Function
#'
#' @noRd 
mod_news_server <- function(input, output, session, df){
  ns <- session$ns
  output$articles <- renderUI({
    df <- df()
    df["source"] <- df$source$name
    df$author[is.na(df$author)] = "Author is Unkown"
    if(is.null(df))
      return(span("No newsapi token"))
    
    nws <- data.frame()
    for(i in 1:nrow(df)){
      if(i %% 2 == 0){
        row <- df[i,]
        nws <- dplyr::bind_rows(nws, row)
      }
    }
    
    items <- nws %>% 
      dplyr::distinct() %>% 
      purrr::transpose() %>% 
      purrr::map(function(article){
        f7ListItem(
          title = article$title,
          subtitle = substr(article$author, 1, 25),
          tagList(
            article$description,
          ),
          media = tags$img(
            src = article$urlToImage
          ),
          footer = tags$a(
            class = "link external article-link",
            target = "_blank",
            article$source,
            href = article$url
          )
        )
      })
    
    f7List(mode = "media", items)
  })
}
    
## To be copied in the UI
# mod_news_ui("news_ui_1")
    
## To be copied in the server
# callModule(mod_news_server, "news_ui_1")
 
