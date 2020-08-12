# Finding locations that are too close after using table_initializeExisting()

library(MazamaSpatialUtils)
setSpatialDataDir("~/Data/Spatial")

library(MazamaLocationUtils)
mazama_initialize()

library(RAWSmet)

# Get an existing set of RAWS location metadata
meta <- fw13_createMetadata()

# > dim(meta)
# [1] 1791    8

# Create a set of known locations (without specifying a radius!)
known_locations <- MazamaLocationUtils::table_initializeExisting(meta)

# > dim(known_locations)
# [1] 1791   15

# Some extra columns have been added

# Now we need to figure out whether any locations are closer than e.g. 100m

# Use geodist to calculate all distances

a <- geodist::geodist(known_locations)

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


