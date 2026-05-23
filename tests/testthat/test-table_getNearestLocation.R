test_that("table_getNearestLocation() works", {
  
  locationTbl <- get(data("wa_monitors_500"))

  emptyRecord <- locationTbl[NA_integer_, ]  # one row, all NA, same schema as locationTbl
  wenatcheeRecord <- locationTbl %>% dplyr::filter(city == "Wenatchee")
  
  # Wenatchee
  lon <- -120.325278
  lat <- 47.423333
  
  testClose <- table_getNearestLocation(locationTbl, lon, lat, distanceThreshold = 100)
  testFar <- table_getNearestLocation(locationTbl, lon, lat, distanceThreshold = 10000)
  expect_equal(testClose, emptyRecord)
  expect_equal(testFar, wenatcheeRecord)
  
})

