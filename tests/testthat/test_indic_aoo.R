#### COMPONENT indic: Calculate Indicators
#### MODULE: Calculate Area Metrics
context("aoo")

### Set parameters
# binary raster
r <- terra::rast(system.file("extdata/Bassaricyon_neblina.tif",
                             package = "wallace"))
# occurrences
occs <- read.csv(system.file("extdata/Bassaricyon_neblina.csv",
                             package = "wallace"))

### Run function
# aoo from raster
aoo_r <- indic_aoo(r,
                   occs = NULL,
                   lon = NULL,
                   lat = NULL,
                   wktFrom = getWKT("wgs84"),
                   wktTo = getWKT("wcea"),
                   logger = NULL)
# aoo from occs
aoo_occs <- indic_aoo(r <- NULL,
                      occs,
                      lon = "longitude",
                      lat = "latitude",
                      wktFrom = getWKT("wgs84"),
                      wktTo = getWKT("wcea"),
                      logger = NULL)


################ tests ####################################
### test if the error messages appear when they are supposed to
test_that("error checks", {
  # lat and lon are not set
  expect_error(indic_aoo(r <- NULL,
                         occs,
                         #lon = "longitude",
                         #lat = "latitude",
                         wktFrom = getWKT("wgs84"),
                         wktTo = getWKT("wcea"),
                         logger = NULL),
               paste0('No longitude and/or latitude name provided.'))

  # lat and lon are set, but not correctly
  expect_error(indic_aoo(r <- NULL,
                         occs,
                         lon = "lon",
                         lat = "lat",
                         wktFrom = getWKT("wgs84"),
                         wktTo = getWKT("wcea"),
                         logger = NULL),
               paste0('Longitude and/or latitude names not found.'))

  # both r and occs are null
  expect_error(indic_aoo(r <- NULL,
                         occs <- NULL,
                         lon = "longitude",
                         lat = "latitude",
                         wktFrom = getWKT("wgs84"),
                         wktTo = getWKT("wcea"),
                         logger = NULL),
               paste0('Provide occurrences or raster.'))
  })

### test if the warning messages appear when they are supposed to
# test_that("warnings checks", {
#   # user's input does not have coordinates
#   expect_warning(
#     indic_aoo(),
#     paste0(""))
# })


### test output features
test_that("output checks", {
  # output is a list
  expect_is(aoo_r, "list")
  expect_is(aoo_occs, "list")
  # the list has two elements
  expect_equal(length(aoo_r), 2)
  expect_equal(length(aoo_occs),2)
  # the elements "area" and "AOOraster"
  expect_equal(c("area", "AOOraster"), names(aoo_r))
  expect_equal(c("area", "AOOraster"), names(aoo_occs))
  # element within the output list are:
  # numeric
  expect_is(aoo_r$area, "numeric")
  expect_is(aoo_occs$area, "numeric")
  # a RasterLayer
  expect_is(aoo_r$AOOraster, "RasterLayer")
  expect_is(aoo_occs$AOOraster, "RasterLayer")
})

### test function steps


