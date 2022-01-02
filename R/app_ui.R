#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny shinyMobile
#' @import plotly leaflet waiter
#' @import dplyr highcharter countup base64enc
#' @noRd
app_ui <- function(request) {
  version <- paste0("v", packageVersion("covidDashboard"))
  
  tagList(
    use_waiter(),
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    f7Page(
      title = "covidDashboard",
      dark = TRUE,
      options = list(theme = c("md"), dark = TRUE),
      # init = f7Init(
      #   skin = "md",
      #   theme = "dark"
      # ),
      preloader = FALSE,
      loading_duration = 6,
      f7TabLayout(
        navbar = f7Navbar(
          title = "Coronavirus Tracker",
          hairline = FALSE,
          shadow = TRUE,
          left_panel = TRUE,
          right_panel = FALSE
        ),
        panels = tagList(
          f7Panel(
            title = "About", 
            side = "left", 
            theme = "dark",
            effect = "cover",
            p("Open-Source Visualization Dashboard for Covid-19. This dashboard Made by Rahul Chauhan shows multiple story of covid-19 Data. This contain interactive Plots to understand the data."),
            f7Link(label = "Author", href = "https://github.com/rahulchauhan049/"),
            f7Link(label = "API", href = "https://github.com/ChrisMichaelPerezSantiago/covid19"),
            f7Link(label = "Johns Hopkins Data", href = "https://github.com/CSSEGISandData/COVID-19"),
            f7Link(label = "Code", href = "https://github.com/rahulchauhan049/covidDashboard"),
            tags$pre(tags$code(version))
          )
        ),
        f7Tabs(
          animated = TRUE,
          id = 'tabs',
          f7Tab(
            tabName = "Data Summary",
            icon = f7Icon("waveform_path", old = FALSE),
            active = TRUE,
            swipeable = TRUE,
            mod_summary_ui("summary_ui_1")
          ),
          f7Tab(
            tabName = "World Data",
            icon = f7Icon("map", old = FALSE),
            active = FALSE,
            swipeable = TRUE,
            mod_world_map_ui("world_map_ui_1")
          ),
          f7Tab(
            tabName = "Visualization",
            icon = f7Icon("graph_circle", old = FALSE),
            active = FALSE,
            swipeable = TRUE,
            mod_visualization_ui("visualization_ui_1")
          ),
          f7Tab(
            tabName = "Knowledge",
            icon = f7Icon("info", old = FALSE),
            active = FALSE,
            swipeable = TRUE,
            mod_knowledge_ui("knowledge_ui_1")
          ),
          f7Tab(
            tabName = "News",
            icon = f7Icon("text_bubble_fill", old = FALSE),
            active = FALSE,
            swipeable = TRUE,
            mod_news_ui("news_ui_1")
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    tags$link(rel="stylesheet", type="text/css", href="www/custom.css"),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'covidDashboard'
    ),
    HTML('
      <!-- Global site tag (gtag.js) - Google Analytics -->
        <script async src="https://www.googletagmanager.com/gtag/js?id=UA-162143218-1"></script>
        <script>
        window.dataLayer = window.dataLayer || [];
      function gtag(){dataLayer.push(arguments);}
      gtag("js", new Date());
      
      gtag("config", "UA-162143218-1");
      </script>'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

loader <- tagList(
  waiter::spin_pixel(),
  br(),
  h2("Loading data")
)

