#### COMPONENT 4: Process Environmental Data
#### MODULE: Select Study Region 
context("bgMask")

source("test_helper_functions.R")


### Set parameters

## occurrences
occs <-  c1_queryDb(spName = "panthera onca", occDb = "gbif", occNum = 100)
occs <- as.data.frame(occs$cleaned)

## enviromental variables 
envs <- c3_worldclim(bcRes = 10, bcSel = list(TRUE,TRUE,TRUE,TRUE,TRUE), doBrick = FALSE)

## background extent 
bgExt <- c4_bgExtent(occs, bgSel = 'bounding box', bgBuf = 0.5) 


### run function 
bgMask <- c4_bgMask(occs, envs, bgExt)


### test if the error messages appear when they are supposed to 
test_that("error checks", {
  # the user has not selected the background extent 
  expect_error(c4_bgMask(occs, envs, bgExt=NULL),
               'Before sampling background points, define the background extent.')
  })

### test output features
test_that("output type checks", {
  # the output is a RasterBrick
  expect_is(bgMask, "RasterBrick")
  # the amount of masked layers are the same as uploaded in the comp. 3
  expect_equal(raster::nlayers(envs), raster::nlayers(bgMask))
  # the masked layers are the same as uploaded in the comp. 3
  expect_equal(names(bgMask), names(envs))
  # all the environmental layers have the same amount of pixels
  expect_equal(raster::cellStats(bgMask, sum), raster::cellStats(bgMask, sum))
  # the original layers have more pixels than the masked ones
  expect_true(raster::cellStats(bgMask$bio1.1, sum) < raster::cellStats(envs$bio1.1, sum))
  expect_true(raster::cellStats(bgMask$bio1.2, sum) < raster::cellStats(envs$bio1.2, sum))
  expect_true(raster::cellStats(bgMask$bio1.3, sum) < raster::cellStats(envs$bio1.3, sum))
  expect_true(raster::cellStats(bgMask$bio1.4, sum) < raster::cellStats(envs$bio1.4, sum))
  expect_true(raster::cellStats(bgMask$bio1.5, sum) < raster::cellStats(envs$bio1.5, sum))
  })
