
library(PWFSLSmoke)
library(MazamaLocationUtils)

mazama_initialize()

jon <- initializeLocationTable()

# ----- Add Washington

monitor <- monitor_loadLatest() %>% monitor_subset(stateCodes = "WA")
lons <- monitor$meta$longitude
lats <- monitor$meta$latitude

jon <- jon %>% addLocations(lons, lats, radius = 500)

saveLocationTable(jon, "jon")

# ----- Add Oregon

monitor <- monitor_loadLatest() %>% monitor_subset(stateCodes = c("WA","OR"))
lons <- monitor$meta$longitude
lats <- monitor$meta$latitude

jon <- jon %>% addLocations(lons, lats, radius = 500)

saveLocationTable(jon, "jon")

# ----- Add Pac NW random sample

monitor <- monitor_loadLatest() %>% monitor_subset(stateCodes = c("WA","OR","ID"))
indices <- sort(sample(seq_along(monitor$meta$longitude), 10))
lons <- monitor$meta$longitude[indices]
lats <- monitor$meta$latitude[indices]

jon <- jon %>% addLocations(lons, lats, radius = 500)

saveLocationTable(jon, "jon")

# ----- Add Idaho

monitor <- monitor_loadLatest() %>% monitor_subset(stateCodes = c("WA","OR","ID"))
lons <- monitor$meta$longitude
lats <- monitor$meta$latitude

jon <- jon %>% addLocations(lons, lats, radius = 500)

saveLocationTable(jon, "jon")

