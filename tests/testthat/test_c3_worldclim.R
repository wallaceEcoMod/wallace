##### QUESTIONS
  # 1. If I add "FALSE" values to the selected variables vector, the function doesn't work 
  # 2. The names aren't the ones specified in the function 


#### COMPONENT 3: Obtain Environmental Data
#### MODULE: WorldClim 
context("WorldClim")

source("test_helper_functions.R")


### Set parameters
## resolution 
bcRes <- 10 # (10 arcmin)
## variables to download 
envar <- list(TRUE, TRUE,TRUE,TRUE,TRUE)


### run function 
arcmin10 <- c3_worldclim(bcRes, bcSel= envar)


### test if the error messages appear when they are supposed to 
test_that("error checks", {
  # the user has not selected a raster resolution 
  expect_error(c3_worldclim(bcRes = '', bcSel = envar, doBrick = FALSE),
               'Select a raster resolution.')
  })

### test output features
test_that("output type checks", {
  # the output is a RasterBrick
  expect_is(arcmin10, "RasterBrick")
  # the number of layer is the same as specified in the selected variables list
  expect_equal(length(envar), raster::nlayers(arcmin10))
  # the resolution is right
  expect_equal((raster::res(arcmin10)), c(10/60, 10/60))
  # the names are right 
  expect_equal(names(arcmin10), c("bio1.1", "bio1.2", "bio1.3", "bio1.4", "bio1.5"))
  })
