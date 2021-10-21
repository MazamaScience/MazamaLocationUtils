# Finding locations that are too close after using table_initializeExisting()

library(MazamaSpatialUtils)
setSpatialDataDir("~/Data/Spatial")

library(MazamaLocationUtils)
mazama_initialize()

library(RAWSmet)

# Get an existing set of RAWS location metadata
meta <- fw13_createMeta()

# > dim(meta)
# [1] 1791    8

# Create a set of known locations (with a small radius)
known_locations <- 
  MazamaLocationUtils::table_initializeExisting(
    meta,
    distanceThreshold = 1
  )

# > dim(known_locations)
# [1] 1791   15

# Some extra columns have been added

# Now we need to figure out whether any locations are closer than e.g. 100m

# Use geodist to calculate all distances

a <- 
  known_locations %>%
  dplyr::rename(x = longitude, y = latitude) %>% 
  geodist::geodist()

# > class(a)
# [1] "matrix"
# > dim(a)
# [1] 1791 1791
# > typeof(a)
# [1] "double"

# Matrix has all distances between locations

# Let's create a logical matrix of non-zero (self-self) distances < 100m

b <- (a != 0) & (a < 100)

# > class(b)
# [1] "matrix"
# > dim(b)
# [1] 1791 1791
# > typeof(b)
# [1] "logical"

# TRUE counts as 1 and FALSE as zero. Use this to find rows with neighbors that
# are too close.

c <- which(rowSums(b) > 0)

# > c
# [1]  120  179  325  326  738  739  750  751  770  771 1597 1598

d <- which(b > 0, arr.ind = TRUE)
#        row  col
#  [1,]  179  120
#  [2,]  120  179
#  [3,]  326  325
#  [4,]  325  326
#  [5,]  739  738
#  [6,]  738  739
#  [7,]  751  750
#  [8,]  750  751
#  [9,]  771  770
# [10,]  770  771
# [11,] 1598 1597
# [12,] 1597 1598

everyOther <- d[seq(1, nrow(d), 2), ]

lines <- c()
for(i in seq(nrow(everyOther))) {
  nwsID1 <- known_locations[everyOther[i,1],]$nwsID
  nwsID2 <- known_locations[everyOther[i,2],]$nwsID
  dist <- a[everyOther[i,1], everyOther[i,2]]
  newLine <- sprintf(
    "nwsID: %s and nwsID %s. Distance: %s m",
    nwsID1,
    nwsID2,
    round(dist, 2)
  )
  
  lines <- append(lines, newLine)
}

# TODO:  Use this information to extract distances from `a`` and individual records
# TODO:  from 'known_locations' to create a nice warning message explaining
# TODO:  what is going on.


# ----- Create individual strings ----------------------------------------------

firstLine <- sprintf(
  "%d locations have neighbors that are < %d m away.",
  round(tooCloseCount),
  radius
)

# Create a list or vector of individual records and how far away they are from
# another record.

# Assemble the big, multi-line warning message using sprintf() or paste()


