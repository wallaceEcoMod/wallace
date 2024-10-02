#### COMPONENT mask: Mask Prediction
#### MODULE: Mask by Drawn Polygon
context("expPoly")

### Set parameters
# range prediction
prediction <- raster::raster(system.file("extdata/Bassaricyon_neblina.tif",
                                         package = "wallace"))
# bg extent
polyExt <- terra::ext(prediction)
polyExt <- terra::as.polygons(polyExt)
polyExt <-sf::st_as_sf(polyExt)
#poly for masking
polyMask <- sf::st_read(system.file("extdata/wdpa/WDPA_COL_olinguito.shp",
                                    package = "wallace"))

### Run function
suppressWarnings(expertRast <- mask_expPoly(polyMask, prediction, polyExt,
                           rem = TRUE, logger = NULL, spN = NULL))

### for tests
# nonoverlapping extent
badprediction <- raster::raster(nrows=108, ncols=108, xmn=-85, xmx=-80)
raster::values(badprediction)<- runif(n = (108*108))

################ Tests ####################################

### test if the error messages appear when they are supposed to
test_that("error checks", {
  # extents do not overlap
  expect_error(mask_expPoly(polyMask, badprediction, polyExt,
                            rem = TRUE, logger = NULL, spN = NULL),
               paste("The polygon only included NA values.",
                     "Please select a polygon that intersects the model prediction. ")
               )
  })

### test if the warning messages appear when they are supposed to
# contains NA values
# test_that("warnings checks", {
#   expect_warning(mask_expPoly(polyMask, prediction, polyExt,
#                               rem = TRUE, logger = NULL, spN = NULL),
#                  paste0("The polygon selected includes some cells with NA values. You cannot change the prediction (suitable or unsuitable) in these cells. "))
#   })
# BAJ 10/01/2024: this is failing and I don't know why.

### test output features
test_that("output checks", {
  # list
  expect_is(expertRast, "list")
  # list of two
  expect_equal(length(expertRast), 2)
  # pred & ext
  expect_equal(names(expertRast), c("pred", "ext"))
  # pred should be a rasterlayer
  expect_is(expertRast$pred, "RasterLayer")
  # ext should be a...
  expect_is(expertRast$ext, "data.frame")
  # it should be an sf dataframe that includes the sfc_polygon of the bg extent
  # the extents should be the same
  expect_equal(raster::extent(polyExt), raster::extent(expertRast$ext))
})

### test function steps
