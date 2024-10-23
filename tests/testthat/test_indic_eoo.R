#### COMPONENT indic: Calculate Indicators
#### MODULE: Calculate Area Metrics
context("eoo")

### Set parameters
# binary raster
r <- terra::rast(system.file("extdata/Bassaricyon_neblina.tif",
                             package = "wallace"))
# occurrences
occs <- read.csv(system.file("extdata/Bassaricyon_neblina.csv",
                             package = "wallace"))
# wcea
wkt <- getWKT("wcea")


### Run function
eoo_r <- indic_eoo(r, occs = NULL, lon = NULL, lat = NULL, wkt, logger = NULL)
eoo_o <- indic_eoo(r = NULL, occs, lon = "longitude", lat = "latitude", wkt, logger = NULL)

################ Tests ####################################
### test if the error messages appear when they are supposed to
test_that("error checks", {
  # when using occs, but lon and lat are not set
  expect_error(indic_eoo(r = NULL, occs, lon = NULL, lat = NULL, wkt, logger = NULL),
               "No longitude and/or latitude name provided.")
  # when using occs, lon and lat are set but do not match
  expect_error(indic_eoo(r = NULL, occs, lon = "lon", lat = "lat", wkt, logger = NULL),
               "Longitude and/or latitude names not found.")
  # r and occs both null
  expect_error(indic_eoo(r = NULL, occs = NULL, lon = NULL, lat = NULL, wkt, logger = NULL),
               "Provide occurrences or raster.")
  })

### test if the warning messages appear when they are supposed to
# No warning messages

### test output features
test_that("output checks", {
  # the output is a list
  expect_is(eoo_r, "list")
  expect_is(eoo_o, "list")
  # the list has two elements
  expect_equal(length(eoo_r), 2)
  expect_equal(length(eoo_o), 2)
  # the elements are:
  ## numeric
  expect_is(eoo_r$area, "numeric")
  expect_is(eoo_o$area, "numeric")
  ## spatialpolygon
  expect_is(eoo_r$eooPoly, "SpatialPolygons")
  expect_is(eoo_o$eooPoly, "SpatialPolygons")
})

### test function steps
