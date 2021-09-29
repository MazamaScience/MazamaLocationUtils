
library(PWFSLSmoke)
library(MazamaLocationUtils)

mazama_initialize()
setLocationDataDir("./data")

wa <- table_initialize()

# ----- Washington

monitor <- monitor_loadLatest() %>% monitor_subset(stateCodes = "WA")
lons <- monitor$meta$longitude
lats <- monitor$meta$latitude

table_initialize() %>%
  table_addLocation(lons, lats, distanceThreshold = 500) %>%
  table_save("wa_monitors_500")

# ----- Oregon

monitor <- monitor_loadLatest() %>% monitor_subset(stateCodes = "OR")
lons <- monitor$meta$longitude
lats <- monitor$meta$latitude

table_initialize() %>%
  table_addLocation(lons, lats, distanceThreshold = 500) %>%
  table_save("or_monitors_500")

# ----- Idaho

monitor <- monitor_loadLatest() %>% monitor_subset(stateCodes = "ID")
lons <- monitor$meta$longitude
lats <- monitor$meta$latitude

table_initialize() %>%
  table_addLocation(lons, lats, distanceThreshold = 500) %>%
  table_save("id_monitors_500")


