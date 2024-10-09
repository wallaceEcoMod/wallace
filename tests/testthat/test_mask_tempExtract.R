#### COMPONENT mask: Mask Prediction
#### MODULE: Calculate Change Over Time
context("tempExtract")

### Set parameters
lowerInp <- 50
upperInp <- 100
maskRaster <- raster::raster(system.file("extdata/MODIS/2010_olinguito_Modis.tif",
                                         package = "wallace"))
pred <- raster::raster(system.file("extdata/Bassaricyon_neblina.tif",
                                   package = "wallace"))
### Run function
postPred <- mask_tempExtract(lowerInp, upperInp,
                              maskRaster, pred,
                              logger = NULL, spN = NULL)

### for testing errors & warnings
lowerInp_na <- NA
upperInp_na <- NA
wkt <- getWKT("wcea")
mask_wrong <- terra::project(terra::rast(maskRaster), wkt)
pred <- terra::rast(pred)
mask_wrong <- raster::raster(mask_wrong)

################ Tests ####################################

### test if the error messages appear when they are supposed to
test_that("error checks", {
  # Please provide bounds for masking.
  expect_error(mask_tempExtract(lowerInp_na, upperInp_na, maskRaster,
                                pred, logger = NULL, spN = NULL),
               paste0("Please provide bounds for masking. "))
})


### test if the warning messages appear when they are supposed to
test_that("warnings checks", {
  # Rasters don't have the same resolution, CRS, or origin.
  expect_warning(mask_tempExtract(lowerInp, upperInp,mask_wrong,
                                  pred,logger = NULL, spN = NULL),
                 paste0("Rasters don't have the same resolution, CRS, and/or origin. "))
})


### test output features
test_that("output checks", {
  # numeric
  expect_is(postPred, "RasterLayer")
  # length 164920
  expect_equal(length(postPred), 164920)
  # 1 layer
  expect_equal(raster::nlayers(postPred), 1)
})

### test function steps
