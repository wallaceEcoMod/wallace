#### COMPONENT indic: Calculate Indicators
#### MODULE: Calculate Change Over Time
context("indic_time")

### Set parameters
# rasterstack of envs
pathRast <- list.files(system.file("extdata/MODIS", package = "wallace"),
                       pattern = ".tif$", full.names = TRUE)
envs <- raster::stack(pathRast)

# range map
range <- raster::raster(system.file("extdata/Bassaricyon_neblina.tif",
                                    package = "wallace"))
# match projections
projected_range <- raster::projectRaster(range, envs, method = 'bilinear')
# alternative- see note from BAJ in indic_time.R from 9/25/2024.
#projected_envs <- raster::projectRaster(envs, range, method = 'bilinear')

# threshold & bounds
thrh <- 20
bound <- "lower"

### Run function
envChangeArea <- indic_time(projected_range, envs, thrh, bound,
                            logger = NULL, spN = NULL)

################ Tests ####################################

### test if the error messages appear when they are supposed to
# No error messages

### test if the warning messages appear when they are supposed to
# No warning messages

### test output features
test_that("output checks", {
  # matrix array
  expect_is(envChangeArea, "matrix")
  # length of 5
  expect_equal(length(envChangeArea), 5)
})

### test function steps
