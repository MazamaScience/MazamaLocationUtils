% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/table_getNearestDistance.R
\name{table_getNearestDistance}
\alias{table_getNearestDistance}
\title{Return distances to nearest known locations}
\usage{
table_getNearestDistance(
  locationTbl = NULL,
  longitude = NULL,
  latitude = NULL,
  radius = NULL
)
}
\arguments{
\item{locationTbl}{Tibble of known locations, Default: NULL}

\item{longitude}{Vector of longitudes in decimal degrees E, Default: NULL}

\item{latitude}{Vector of latitudes in decimal degrees N, Default: NULL}

\item{radius}{Radius in meters, Default: NULL}
}
\value{
Vector of distances from known locations.
}
\description{
Returns a distances from known locations in \code{locationTbl},
one for each incoming location. If no known location is found within
\code{radius} meters for a particular
incoming location, that distance in the vector will be \code{NA}.
}
\examples{
library(MazamaLocationUtils)

locationTbl <- get(data("wa_monitors_500"))

# Wenatchee
lon <- -120.325278
lat <- 47.423333

# Too small a radius will not find a match
table_getNearestDistance(locationTbl, lon, lat, radius = 50)

# Expanding the radius will find one
table_getNearestDistance(locationTbl, lon, lat, radius = 5000)
}
