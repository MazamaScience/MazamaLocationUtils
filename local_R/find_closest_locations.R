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
  table_getNearestDistance(longitude, latitude, 1000)





################################################################################
################################################################################
################################################################################





load(url("http://data-monitoring_v2-c1.airfire.org/monitoring-v2/known-locations/airsis_PM2.5_sites_1000.rda"))
load(url("http://data-monitoring_v2-c1.airfire.org/monitoring-v2/known-locations/airnow_PM2.5_sites.rda"))

# Meeks Bay

kl <-
  airsis_PM2.5_sites_1000 %>%
  dplyr::filter(
    .data$longitude > -120.13 &
      .data$longitude < -120.12 &
      .data$latitude > 39.03 &
      .data$latitude < 39.04
  )

# 8 records very close together

library(AirMonitor)

airsis_2021 <- airsis_loadAnnual(2021)
airsis_2020 <- airsis_loadAnnual(2020)
airsis_2019 <- airsis_loadAnnual(2019)
airsis_2018 <- airsis_loadAnnual(2018)
airsis_2017 <- airsis_loadAnnual(2017)
airsis_2016 <- airsis_loadAnnual(2016)
airsis_2015 <- airsis_loadAnnual(2015)

# 2020 -- ARB 1017 and this is the only one. airsis_2020 shows only a single location


wrcc_2021 <- wrcc_loadAnnual(2021)
wrcc_2020 <- wrcc_loadAnnual(2020)
wrcc_2019 <- wrcc_loadAnnual(2019)
wrcc_2018 <- wrcc_loadAnnual(2018)
wrcc_2017 <- wrcc_loadAnnual(2017)
wrcc_2016 <- wrcc_loadAnnual(2016)
wrcc_2015 <- wrcc_loadAnnual(201)




