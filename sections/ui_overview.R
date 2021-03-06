body_overview <- dashboardBody(
  tags$head(
    tags$style(type = "text/css", "#overview_map {height: 48vh !important;}"),
    tags$style(type = 'text/css', ".slider-animate-button { font-size: 20pt !important; }"),
    tags$style(type = 'text/css', ".slider-animate-container { text-align: left !important; }"),
    tags$style(type = "text/css", "@media (max-width: 991px) { .details { display: flex; flex-direction: column; } }"),
    tags$style(type = "text/css", "@media (max-width: 991px) { .details .map { order: 1; width: 100%; } }"),
    tags$style(type = "text/css", "@media (max-width: 991px) { .details .summary { order: 3; width: 100%; } }"),
    tags$style(type = "text/css", "@media (max-width: 991px) { .details .slider { order: 2; width: 100%; } }")
  ),
  fluidRow(
    fluidRow(
      uiOutput("box_keyFigures")
    ),
    fluidRow(
      class = "details",
      column(
        box(
          width = 12,
          leafletOutput("overview_map")
        ),
        class = "map",
        width = 8,
        style = 'padding:0px;'
      ),
      column(
        uiOutput("summaryTables"),
        class = "summary",
        width = 4,
        style = 'padding:0px;'
      ),
      column(
        # print(data_evolution$date),
        sliderInput(
          "timeSlider",
          label      = "Select Date",
          min        = data_evolution$date[1],
          max        = max(current_date,na.rm = TRUE),
          value      = max(current_date,na.rm = TRUE),
          width      = "100%",
          timeFormat = "%d.%m.%Y",
          animate    = animationOptions(loop = TRUE)
        ),
        class = "slider",
        width = 12,
        style = 'padding-left:15px; padding-right:15px;'
      )
    )
  )
)

page_overview <- dashboardPage(
  title   = "Resúmen",
  header  = dashboardHeader(disable = TRUE),
  sidebar = dashboardSidebar(disable = TRUE),
  body    = body_overview
)