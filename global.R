# ---- Loading libraries ----
library("shiny")
library("shinydashboard")
library("tidyverse")
library("leaflet")
library("plotly")
library("DT")
library("fs")
library("wbstats")

source("utils.R", local = T)

downloadGithubData <- function() {
  download.file(
    url      = "https://github.com/pablora19/COVID19_EC/archive/master.zip",
    destfile = "data/covid19_data.zip"
  )
  
  data_path <- "COVID19_EC-master/"
  unzip(
    zipfile   = "data/covid19_data.zip",
    files     = paste0(data_path, c("covid_ec.csv", "age range - gender fem_mal .csv", "age range - alive_deads.csv","deaths by province from 26_03_2020.csv")),
    exdir     = "data",
    junkpaths = T
  )

  download.file(
    url      = "https://github.com/cgonzalezbrito/EcuadorLatLong/archive/master.zip",
    destfile = "data/EcLatLong.zip"
  )
  
  data_path_LAT_LONG <- "EcuadorLatLong-master/"
  unzip(
    zipfile   = "data/EcLatLong.zip",
    files     = paste0(data_path_LAT_LONG, c("EcLatLong.csv")),
    exdir     = "data",
    junkpaths = T
  )  
}


updateData <- function() {
  if (!dir_exists("data")) {
    dir.create('data')
    downloadGithubData()
  } else if ((!file.exists("data/covid19_data.zip")) || (as.double(Sys.time() - file_info("data/covid19_data.zip")$change_time, units = "hours") > 0.5)) {
    downloadGithubData()
  }
}

# Update with start of app
updateData()

# # TODO: Still throws a warning but works for now
data_confirmed <- read_csv("data/covid_ec.csv")
location <- read_csv("data/EcLatLong.csv")
# deceased_confirmed <- read_csv("data/deaths by province from 26_03_2020.csv")

data_confirmed <- merge(x=data_confirmed, y= location, by= "nombre_canton", all = TRUE)


# # Get latest data
current_date <- as.Date(data_confirmed[["fecha"]], format = "%d/%m/%y")
changed_date <- file_info("data/covid19_data.zip")$change_time

print(current_date)

# # Get evolution data
data_confirmed_sub <- data_confirmed %>%
  group_by(infografia, fecha, nombre_provincia, nombre_canton, Lat, Long) %>%
  summarise("confirmed" = sum(casos_confirmados, na.rm = T))

data_recovered_sub <- data_confirmed %>%
  group_by(infografia, fecha, nombre_provincia, nombre_canton, Lat, Long) %>%
  summarise("recovered" = sum(`Alta hospitalaria`, na.rm = T))

data_deceased_sub <- data_confirmed %>%
  group_by(infografia, fecha, nombre_provincia, Lat, Long) %>%
  summarise("deceased" = sum(fallecidos, na.rm = T))

data_evolution <- data_confirmed_sub %>%
  full_join(data_deceased_sub) %>%
  full_join(data_recovered_sub) %>%
  ungroup() %>%
  mutate(date = as.Date(fecha, "%d/%m/%y")) %>%
  arrange(date) %>%
  group_by(infografia, fecha, nombre_provincia, nombre_canton, Lat, Long) %>%
  mutate(
    active = confirmed #- recovered/86 - deceased/86
  ) %>%
  pivot_longer(names_to = "var", cols = c(confirmed, recovered, deceased, active)) %>%
  ungroup()

# # Calculating new cases
data_evolution <- data_evolution %>%
  group_by(infografia, nombre_provincia, nombre_canton, Lat, Long) %>%
  mutate(value_new = value - lag(value, 4, default = 0)) %>%
  ungroup()

# # # ---- Download population data ----
# population_sub <- data_confirmed %>%
#   group_by(nombre_provincia, nombre_canton) %>%
#   summarise("population" = sum(poblacion_canton, na.rm = T))

rm(data_confirmed, data_confirmed_sub, data_deceased_sub, data_recovered_sub)



# population                                                            <- wb(country = "countries_only", indicator = "SP.POP.TOTL", startdate = 2018, enddate = 2020) %>%
#   select(country, value) %>%
#   rename(population = value)
# countryNamesPop                                                       <- c("Brunei Darussalam", "Congo, Dem. Rep.", "Congo, Rep.", "Czech Republic",
#   "Egypt, Arab Rep.", "Iran, Islamic Rep.", "Korea, Rep.", "St. Lucia", "West Bank and Gaza", "Russian Federation",
#   "Slovak Republic", "United States", "St. Vincent and the Grenadines", "Venezuela, RB")
# countryNamesDat                                                       <- c("Brunei", "Congo (Kinshasa)", "Congo (Brazzaville)", "Czechia", "Egypt", "Iran", "Korea, South",
#   "Saint Lucia", "occupied Palestinian territory", "Russia", "Slovakia", "US", "Saint Vincent and the Grenadines", "Venezuela")
# population[which(population$country %in% countryNamesPop), "country"] <- countryNamesDat


# data_evolution <- data_evolution %>%
#   left_join(population, by = c("Country/Region" = "country"))
# rm(population, countryNamesPop, countryNamesDat, noDataCountries)


data_atDate <- function(inputDate) {
  data_evolution[which(data_evolution$date == inputDate),] %>%
    distinct() %>%
    pivot_wider(id_cols = c("infografia","nombre_provincia", "nombre_canton", "fecha","Lat","Long"), names_from = var, values_from = value) %>%
    filter(confirmed > 0 |
             recovered > 0 |
             deceased > 0 |
             active > 0)
}

data_latest <- data_atDate(data_evolution$date[3516])

top5_countries <- data_evolution %>%
  filter(var == "active", date == current_date) %>%
  group_by(nombre_provincia) %>%
  summarise(value = sum(value, na.rm = T)) %>%
  arrange(desc(value)) %>%
  top_n(5) %>%
  select(nombre_provincia) %>%
  pull()
