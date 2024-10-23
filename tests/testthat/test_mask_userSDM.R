#### COMPONENT mask: Calculate Indicators
#### MODULE: Upload user-specified SDM prediction
context("userSDM")

### Set parameters
rasPath <- system.file("extdata/Bassaricyon_neblina.tif",package = "wallace")
rasName <- "Bassaricyon_neblina.tif"

### Run function
sdm <- mask_userSDM(rasPath, rasName, logger = NULL, spN = NULL)

# to test errors
rasPath_error <- system.file("extdata/B_neblina_WCEA.tif",package = "wallace")
#rasPath_error2 <- system.file("something with the wrong extent. see note below")

################ Tests ####################################

### test if the error messages appear when they are supposed to
test_that("error checks", {
  # unspecified CRS
  expect_error(mask_userSDM(rasPath_error, rasName, logger = NULL, spN = NULL),
               paste0("Input rasters have undefined coordinate reference system (CRS). Mapping functionality will not work. Please define their projections and upload again. See guidance text in this module for more details."),
               fixed = TRUE)

  # extents do not overlap
  # BAJ 10/10/2024: need to upload a raster with xmax > 180 and xmin < -180 to get this error
  # expect_error(mask_userSDM(rasPath_error2, rasName, logger = NULL, spN = NULL),
  #              paste0("Wrong extent projection. '", rasName,"' cannot be uploaded. "))
})


### test if the warning messages appear when they are supposed to
# No warning messages

### test output features
test_that("output checks", {
  # list
  expect_is(sdm, "list")
  # list of 2
  expect_equal(length(sdm),2)
  # two items should be sdm$sdm & sdm$extSdm
  expect_equal(names(sdm), c("sdm", "extSdm"))
  # sdm$sdm should be a rasterlayer
  expect_is(sdm$sdm, "RasterLayer")
  # sdm$extSdm should be a spatialpolygon
  expect_is(sdm$extSdm, "SpatialPolygons")
  # same extents
  expect_equal(raster::extent(sdm$sdm), raster::extent(sdm$extSdm))
})

### test function steps
