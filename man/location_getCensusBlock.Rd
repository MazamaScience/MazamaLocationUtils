% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/location_getCensusBlock.R
\name{location_getCensusBlock}
\alias{location_getCensusBlock}
\title{Get census block data from the FCC API}
\usage{
location_getCensusBlock(
  longitude = NULL,
  latitude = NULL,
  censusYear = 2010,
  verbose = TRUE
)
}
\arguments{
\item{longitude}{Single longitude in decimal degrees E.}

\item{latitude}{Single latitude in decimal degrees N.}

\item{censusYear}{Year the census was taken.}

\item{verbose}{Logical controlling the generation of progress messages.}
}
\value{
List of census block/county/state data.
}
\description{
The FCC Block API is used get census block, county, and state FIPS associated with
the \code{longitude} and \code{latitude}. The following list of data
is returned:
\itemize{
\item{\code{stateCode}}
\item{\code{countyName}}
\item{\code{censusBlock}}
}
The data from this function should be considered to be the gold standard for state and county.
i.e. this information could and should be used to override information we get elsewhere.
}
\examples{
\donttest{
library(MazamaLocationUtils)

# Fail gracefully if any resources are not available
try({

  # Wenatchee
  lon <- -120.325278
  lat <- 47.423333

  censusList <- location_getCensusBlock(lon, lat)
  str(censusList)
  
}, silent = FALSE)
}
}
\references{
\url{https://geo.fcc.gov/api/census/}
}
