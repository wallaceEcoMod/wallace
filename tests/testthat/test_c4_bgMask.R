##### QUESTIONS
  # 1. Do we want this an the next one as a separate context?
  # 2. cellStats for all the layers

#### COMPONENT 4: Process Environmental Data
#### MODULE: Select Study Region 
context("bgMask - Step 2.1")

source("test_helper_functions.R")


### get data

## occurrence
occs <-  c1_queryDb(spName = "panthera onca", occDb = "gbif", 
                   occNum = 100)
occs <- as.data.frame(occs$cleaned)

## Enviromental data
envs <- c3_worldclim(bcRes = 10, bcSel = (list(TRUE,TRUE,TRUE,TRUE,TRUE)))

## background extent 
bgExt <- c4_bgExtent(occs, envs, bgSel = 'bb', bgBuf = 0.5) 


### run function 
bgMask <- c4_bgMask(occs, envs, bgExt)


### test if the error messages appear when they are supposed to 
test_that("error checks", {
  # the user has not selected the background extent 
  expect_error(c4_bgMask(occs, envs, bgExt=NULL),'Before sampling background points, define the background extent.')
})

### test output features
test_that("output type checks", {
  # the output is a RasterBrick
  expect_is(bgMask, "RasterBrick")
  # the masked layers are the same as uploaded in the comp. 3
  expect_equal(names(bgMask), names(envs))
  expect_equal(raster::nlayers(envs), raster::nlayers(bgMask))
  # the original layers have more pixels than the masked ones
  expect_true(raster::cellStats(bgMask$bio1.1, sum) < raster::cellStats(envs$bio1.1, sum)) # an example 
})

