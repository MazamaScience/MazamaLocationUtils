library(MazamaLocationUtils)

load(url("http://data-monitoring_v2-c1.airfire.org/monitoring-v2/known-locations/wrcc_PM2.5_sites_1000.rda"))

table_leaflet(wrcc_PM2.5_sites_1000)

# Pinecrest, CA has 10 locations that are too close
longitude = -120.00753
latitude = 38.187973


bop <- 
  wrcc_PM2.5_sites_1000 %>%
  table_findAdjacentDistances(
    distanceThreshold = 500
  )

bop <- 
  wrcc_PM2.5_sites_1000 %>%
  table_findAdjacentLocations(
    distanceThreshold = 500
  )


# Fire Cache Way at Missoula airport has LOTS of monitors
longitude = -114.09615
latitude = 46.9269

bop <-
  wrcc_PM2.5_sites_1000 %>%
  table_filterByDistance(
    longitude, 
    latitude, 
    1000
  )



