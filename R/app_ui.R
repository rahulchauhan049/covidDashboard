#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny shinyMobile
#' @import plotly leaflet waiter
#' @import dplyr highcharter countup
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
      dark_mode = TRUE,
      init = f7Init(
        skin = "md", 
        theme = "dark"
      ),
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
            f7Link(label = "Author", src = "https://github.com/rahulchauhan049/", external = TRUE),
            f7Link(label = "API", src = "https://github.com/ChrisMichaelPerezSantiago/covid19", external = TRUE),
            f7Link(label = "Code", src = "https://github.com/JohnCoene/coronavirus", external = TRUE),
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

