% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{id_monitors_500}
\alias{id_monitors_500}
\title{Idaho monitor locations dataset}
\format{
A tibble with 34 rows and 13 columns of data.
}
\usage{
id_monitors_500
}
\description{
The \code{id_monitor_500} dataset provides a set of known
locations associated with Idaho state air quality monitors.
This dataset was generatedon 2019-10-21 by running:

\preformatted{
library(PWFSLSmoke)
library(MazamaLocationUtils)

mazama_initialize()
setLocationDataDir("./data")

monitor <- monitor_loadLatest() %>% monitor_subset(stateCodes = "ID")
lons <- monitor$meta$longitude
lats <- monitor$meta$latitude

table_initialize() %>%
  table_addLocation(lons, lats, radius = 500) %>%
  table_save("id_monitors_500")
}
}
\seealso{
\link{or_monitors_500}

\link{wa_monitors_500}
}
\keyword{datasets}
