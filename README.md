
# MazamaLocationUtils

```
A suite of utility functions for discovering and managaing metadata associated
with sets of spatially unique "known locations".
```

## Background

This package is intended to be used in support of data management activities
associated with fixed locations in space. The motivating fields include both
air and water quality monitoring where fixed sensors report at regular time 
intervals.

When working with environmental monitoring time series, one of the first things
you have to do is create unique identifiers for each individual time series. In 
an ideal world, each environmental time series would have both a 
`locationID` and a `sensorID` that uniquely identify the spatial location and 
specific instrument making measurements. A unique `timeseriesID` could
be produced as `locationID_sensorID`. Metadata associated with each
time series would contain basic information needed for downstream analysis
including at least:

`timeseriesID, locationID, sensorID, longitude, latitude, ...`

* Multiple sensors placed at a location could be be grouped by `locationID`.
* An extended timeservers for a mobile sensor would group by `sensorID`.
* Maps would be created using `longitude, latitude`.
* Time series would be accessed from a secondary `data` table with `timeseriesID`.

Unfortunately, we are rarely supplied with a truly unique and truly spatial 
`locationID`. Instead we often use `sensorID` or an associated non-spatial
identifier as a standin for `locationID`.

Complications we have seen include:

* GPS-reported longitude and latitude can have _jitter_ in the fourth or fifth 
decimal place making it challenging to use them to create a unique `locationID`.
* Sensors are sometimes _repositioned_ in what the scientist considers the "same 
location".
* Data for a single sensor goes through different processing pipelines using
different identifiers and is later brought together as two separate timeseries.
* The radius of what constitutes a "single location" depends on the 
instrumentation and scientific question being asked.
* Deriving location-based metadata from spatial datasets is computationally 
intensive unless saved and identified with a unique `locationID`.
* Automated searches for spatial metadata occasionally produce incorrect results
because of the non-infinite resolution of spatial datasets.

## Desktop solution

A solution to all these problems is possible if we create the following 
functionality here written as pseudo-code in R:

```
# Function to retrieve "known location" metadata
getLocation <- function(
  longitude = NULL,
  latitude = NULL,
  collectionName = NULL,
  radius = NULL
) {

  # ----- Open up pre-existing collectionName table
  
  # ----- Find known locations in this table
  
    # Use sp pacakge to create a circle at lon,lat with radius
    # Use point-in-polygon search to find the location that falls within
    
  # ----- Handle duplicate or missing appropriately
  
  # ----- Return record from location table
  
}
```

However, in order to create the pre-existing `collectionName` table we also
need:

```
# Function to add a record to a "known location" table
addLocation <- function(
  longitude = NULL,
  latitude = NULL
  collectionName = NULL
) {

  # ----- Open up pre-existing collectionName table
  
  # ----- Create CPU intensive / Internet obtained spatial metadata
  
  # ----- Update the table 
  
  # ----- Return newly created record
  
}
```

This second function may take a very long time to run depending on how many
spatial queries we need to perform. But it only needs to be done once per
known location.

Some care needs to be taken when initially populating the table to make sure
that the "best" longitude and latitude information is used.

## Immediate Advantages

Working in this manner will solve the problems initially mentioned but also 
provides further useful functionality.

* Administrators can correct entries in the `collectionName` table.  (_e.g._ 
locations in river bends that even high resolution spatial datasets mis-assign)
* Additional, non-automatable metadata can be added to `collectionName`. (_e.g._
commonly used location names within a community of practice)
* Different field campaigns can have separate `collectionName` tables.
* `.csv` or `.rda` versions of well populated tables can be downloaded from a
URL and used locally, giving scientists working with known locations instant
access to spatial data that otherwise requires special skills, large datasets 
and lots of compute cycles.

----

This project is supported by Mazama Science.

