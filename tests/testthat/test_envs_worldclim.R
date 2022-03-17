#### COMPONENT 3: Obtain Environmental Data
#### MODULE: WorldClim
context("WorldClim")

### Set parameters
## resolution
bcRes <- 10 # (10 arcmin)
## variables to download
envar <- c('bio05', 'bio06', 'bio13', 'bio14')

### run function
arcmin10 <- envs_worldclim(bcRes, bcSel = envar)


### test if the error messages appear when they are supposed to
test_that("error checks", {
  # the user has not selected a raster resolution
  expect_error(envs_worldclim(bcRes = '', bcSel = envar, doBrick = FALSE),
               'Select a raster resolution.')
  })

### test output features
test_that("output type checks", {
  # the output is a RasterBrick
  expect_is(arcmin10, "RasterStack")
  # the number of layer is the same as specified in the selected variables list
  expect_equal(length(envar), raster::nlayers(arcmin10))
  # the resolution is right
  expect_equal((raster::res(arcmin10)), c(10/60, 10/60))
  # the names are right
  expect_equal(names(arcmin10), c("bio05", "bio06", "bio13", "bio14"))
  })

