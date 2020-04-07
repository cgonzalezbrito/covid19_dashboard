server <- function(input, output) {
  sourceDirectory("sections", recursive = TRUE)

  showNotification("ORIGINAL:  https://github.com/chschoenenberger/covid19_dashboard DATA: https://github.com/pablora19/COVID19_EC.",
    duration = 30, type = "error")

  # Trigger once an hour
  dataLoadingTrigger <- reactiveTimer(3600000)

  observeEvent(dataLoadingTrigger, {
    updateData()
  })

  observe({
    data <- data_atDate(input$timeSlider)
  })
}