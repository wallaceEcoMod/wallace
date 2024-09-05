#### COMPONENT indic: Calculate Indicators
#### MODULE: Calculate Ratio Overlap
context("raster")

### Set parameters
#rasPath
rasPath <- system.file("extdata/Bassaricyon_neblina.tif",
                             package = "wallace")
#rasName
rasName <- "Bassaricyon_neblina.tif"

#overlapArea
rangeMap <- raster::raster(nrows=108, ncols=108, xmn=-79, xmx=-73)
raster::values(rangeMap)<- runif(n = (108*108))
##convert to sf object
rangeMap <- terra::rast(rangeMap)
rangeMap[rangeMap == 0] <- NA
rangeMap <- terra::as.polygons(rangeMap)
rangeMap <- sf::st_as_sf(rangeMap)

### Run function
r <- indic_raster(rasPath,
                  rasName,
                  overlapArea = rangeMap,
                  logger = NULL,
                  spN = NULL)

### for error checks
#overlapArea ERROR
rangeFAIL <- raster::raster(nrows=108, ncols=108, xmn=-50, xmx=50)
raster::values(rangeFAIL)<- runif(n = (108*108))
rangeFAIL <- terra::rast(rangeFAIL)
rangeFAIL[rangeFAIL == 0] <- NA
rangeFAIL <- terra::as.polygons(rangeFAIL)
rangeFAIL <- sf::st_as_sf(rangeFAIL)
#wrong crs
rasPathFAIL <- system.file("extdata/B_neblina_WCEA.tif",
                           package = "wallace")

################ Tests ####################################

### test if the error messages appear when they are supposed to
test_that("error checks", {
  # if the raster is in the wrong projection
  expect_error(indic_raster(rasPathFAIL,
                            rasName,
                            overlapArea = rangeMap,
                            logger = NULL, spN = NULL),
               "Wrong extent projection. 'Bassaricyon_neblina' cannot be uploaded.")

  # do not intersect
  expect_error(indic_raster(rasPath,
                            rasName,
                            overlapArea = rangeFAIL,
                            logger = NULL,
                            spN = NULL),
             "Overlap raster does not match with range map extent. Please specify a new raster.")
})

### test if the warning messages appear when they are supposed to
# No warning messages

### test output features
test_that("output checks", {
  # r is a rasterlayer
  expect_is(r, "RasterLayer")
})

### test function steps
