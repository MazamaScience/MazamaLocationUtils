% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/location_createID.R
\name{location_createID}
\alias{location_createID}
\title{Create one or more unique locationIDs}
\usage{
location_createID(
  longitude = NULL,
  latitude = NULL,
  algorithm = c("geohash", "digest"),
  precision = 10
)
}
\arguments{
\item{longitude}{Vector of longitudes in decimal degrees E.}

\item{latitude}{Vector of latitudes in decimal degrees N.}

\item{algorithm}{Algorithm to use -- either \code{"geohash"} or \code{"digest"}.}

\item{precision}{\code{precision} argument used when encoding with \code{"geohash"}.}
}
\value{
Vector of character locationIDs.
}
\description{
A unique locationID is created for each incoming
\code{longitude} and \code{latitude}.

See \code{MazamaCoreUtils::\link[MazamaCoreUtils:createLocationID]{createLocationID}} for details.

At \code{precision = 10}, this results in a maximum error of 0.6 meters which
is more than precise enough for environmental monitoring studies making use
of this package.

An excellent way to become familiar with geohash is through the
\href{https://geohash.softeng.co}{GeoHash Explorer}.
}
\note{
The \code{"digest"} algorithm is deprecated but provided for backwards
compatibility with databases that were built using locationIDs generated
with this algorithm.
}
\examples{
library(MazamaLocationUtils)

# Wenatchee
lon <- -120.325278
lat <- 47.423333
locationID <- location_createID(lon, lat)
print(locationID)

location_createID(lon, lat, algorithm = "geohash")
location_createID(lon, lat, algorithm = "geohash", precision = 7)

}
\references{
\url{https://en.wikipedia.org/wiki/Decimal_degrees}

\url{https://www.johndcook.com/blog/2017/01/10/probability-of-secure-hash-collisions/}
}
