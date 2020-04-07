output$summaryTables <- renderUI({
  tabBox(
    tabPanel("Provincia",
      div(
        dataTableOutput("summaryDT_country"),
        style = "margin-top: -10px")
    ),
    tabPanel("Cantón",
      div(
        dataTableOutput("summaryDT_state"),
        style = "margin-top: -10px"
      )
    ),
    width = 12
  )
})

output$summaryDT_country <- renderDataTable(getSummaryDT(data_atDate(input$timeSlider), "nombre_provincia", selectable = TRUE))
proxy_summaryDT_country  <- dataTableProxy("summaryDT_country")
output$summaryDT_state   <- renderDataTable(getSummaryDT(data_atDate(input$timeSlider), "nombre_canton", selectable = TRUE))
proxy_summaryDT_state    <- dataTableProxy("summaryDT_state")

observeEvent(input$timeSlider, {
  data <- data_atDate(input$timeSlider)
  replaceData(proxy_summaryDT_country, summariseData(data, "nombre_provincia"), rownames = FALSE)
  replaceData(proxy_summaryDT_state, summariseData(data, "nombre_canton"), rownames = FALSE)
}, ignoreInit = TRUE, ignoreNULL = TRUE)

observeEvent(input$summaryDT_country_row_last_clicked, {
  selectedRow     <- input$summaryDT_country_row_last_clicked
  selectedCountry <- summariseData(data_atDate(input$timeSlider), "nombre_provincia")[selectedRow, "nombre_provincia"]
  location        <- data_evolution %>%
    distinct(nombre_provincia, Lat, Long) %>%
    filter(nombre_provincia == selectedCountry) %>%
    summarise(
      Lat  = mean(Lat),
      Long = mean(Long)
    )
  leafletProxy("overview_map") %>%
    setView(lng = location$Long, lat = location$Lat, zoom = 9)
})

observeEvent(input$summaryDT_state_row_last_clicked, {
  selectedRow     <- input$summaryDT_state_row_last_clicked
  selectedCountry <- summariseData(data_atDate(input$timeSlider), "nombre_canton")[selectedRow, "nombre_canton"]
  location <- data_evolution %>%
    distinct(nombre_canton, Lat, Long) %>%
    filter(nombre_canton == selectedCountry) %>%
    summarise(
      Lat  = mean(Lat),
      Long = mean(Long)
    )
  leafletProxy("overview_map") %>%
    setView(lng = location$Long, lat = location$Lat, zoom = 9)
})

summariseData <- function(df, groupBy) {
  freq_deceased = count(df, deceased)
  freq_recovered = count(df, recovered)
  df %>%
    group_by(!!sym(groupBy)) %>%
    summarise(
      "Confirmados" = sum(confirmed, na.rm = T),
      "Recuperados" = sum(freq_recovered[1,1], na.rm = T),
      "Decesos"     = sum(freq_deceased[1,1], na.rm = T),
      "Activos"     = sum(confirmed, na.rm = T)
    ) %>%
    as.data.frame()
}

getSummaryDT <- function(data, groupBy, selectable = FALSE) {
  datatable(
    na.omit(summariseData(data, groupBy)),
    rownames  = FALSE,
    options   = list(
      order          = list(1, "desc"),
      scrollX        = TRUE,
      scrollY        = "37vh",
      scrollCollapse = T,
      dom            = 'ft',
      paging         = FALSE
    ),
    selection = ifelse(selectable, "single", "none")
  )
}