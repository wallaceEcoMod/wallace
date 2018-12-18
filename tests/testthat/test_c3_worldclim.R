##### QUESTIONS
  # 1. If I add "FALSE" values to the selected variables vector, the function doesn't work 
    # >> They'll fix it
  # 2. What about the names
    # >> They'll fix it
  # 3. What to do with the downloads 
    # >> Let's have them within the test folder 
  # 4. I think that I should do the test for the 30arsec and other resolution
    # >> Just one
  # 5. What about the resolution (raster::res(arcmin10))
    # >> DO IT

#### COMPONENT 3: Obtain Environmental Data
#### MODULE: WorldClim 
context("WorldClim")

source("test_helper_functions.R")

### set parameters 
var = list(TRUE,TRUE,TRUE,TRUE,TRUE)

### run function 
# arcsec30 <- c3_worldclim(bcRes = 0.5, bcSel = var)
# arcmin25 <- c3_worldclim(bcRes = 2.5, bcSel = var)
# arcmin5 <- c3_worldclim(bcRes = 5, bcSel = var)
arcmin10 <- c3_worldclim(bcRes = 10, bcSel = list(TRUE, TRUE,TRUE,TRUE,TRUE))


### test if the error messages appear when they are supposed to 
test_that("error checks", {
  # the user has not selected a raster resolution 
  expect_error(c3_worldclim(bcRes = '', bcSel = list(TRUE,TRUE,TRUE,TRUE,TRUE)),
               'Select a raster resolution.')
})

### test output features
test_that("output type checks", {
  # the output is a RasterBrick
  expect_is(arcmin10, "RasterBrick")
  # the number of layer is the same as specified in the selected variables list
  expect_equal(length(var), raster::nlayers(arcmin10))
  
})

