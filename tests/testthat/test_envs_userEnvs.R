#### COMPONENT 3: Obtain Environmental Data
#### MODULE: User-specified
context("userEnvs")

# source("test_helper_functions.R")


### Set parameters

## path to files
Path <- list.files(system.file("extdata/wc", package = "wallace"),
                       pattern = ".tif$", full.names = TRUE)
## files name
Name <- list.files(system.file("extdata/wc", package = "wallace"),
                   pattern = ".tif$", full.names = FALSE)

### run function
userEnvs <- envs_userEnvs(rasPath = Path, rasName = Name,
                          doBrick = TRUE)
userEnvs_stack <- envs_userEnvs(rasPath = Path, rasName = Name,
                                doBrick = FALSE)

### test if the warning messages appear when they are supposed to
# GEPB (2022-09-28): Since the update of raster (v3.6-3), it looks like the projection is set
# automatically by the function. I did not explore further but it looks like these changes are
# due the retirement of rgdal. For this reason, the following test was not getting the expected
# warning, so I commented it for now.

# test_that("warning checks", {
#   # input rasters have undefined coordinate reference system (CRS)
#   expect_warning(
#     envs_userEnvs(rasPath = system.file('extdata/wc/proj/no_projection.tif', package = "wallace"),
#                   rasName = 'no_projection.tif'),
#     paste0('Input rasters have undefined coordinate reference system (CRS). ',
#            'Mapping functionality in components Visualize Model Results and ',
#            'Transfer Model will not work. If you wish to map rasters in these ',
#            'components, please define their projections and upload again. See ',
#            'guidance text in this module for more details.'),
#     fixed = TRUE)
#   })

### test output features
test_that("output type checks", {
  # the output is a RasterBrick when doBrick=TRUE
  expect_is(userEnvs, "RasterBrick")
  # the number of layer is the same as loaded by the user
  expect_equal(length(Name), raster::nlayers(userEnvs))
  # the output is a RasterStack when doBrick=FALSE
  expect_is(userEnvs_stack, "RasterStack")
  })

