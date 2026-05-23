test_that("table_initialize() has proper columns", {
  skip_on_cran()
  skip_on_ci()
  
  initializeMazamaSpatialUtils()
  locationTbl <- table_initialize()
  expect_equal(names(locationTbl), MazamaLocationUtils::coreMetadataNames)
})

