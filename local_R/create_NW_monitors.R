
library(PWFSLSmoke)
library(MazamaLocationUtils)

mazama_initialize()
setLocationDataDir("./data")

wa <- initializeLocationTable()

# ----- Washington

monitor <- monitor_loadLatest() %>% monitor_subset(stateCodes = "WA")
lons <- monitor$meta$longitude
lats <- monitor$meta$latitude

initializeLocationTable() %>%
  addLocation(lons, lats, radius = 500) %>%
  saveLocationTable("wa_monitors_500")

# ----- Oregon

monitor <- monitor_loadLatest() %>% monitor_subset(stateCodes = "OR")
lons <- monitor$meta$longitude
lats <- monitor$meta$latitude

initializeLocationTable() %>%
  addLocation(lons, lats, radius = 500) %>%
  saveLocationTable("or_monitors_500")

# ----- Idaho

monitor <- monitor_loadLatest() %>% monitor_subset(stateCodes = "ID")
lons <- monitor$meta$longitude
lats <- monitor$meta$latitude

initializeLocationTable() %>%
  addLocation(lons, lats, radius = 500) %>%
  saveLocationTable("id_monitors_500")


