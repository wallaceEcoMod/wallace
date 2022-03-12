#### COMPONENT penvs: Process Environmental Data
#### MODULE: Select Study Region
context("bgMask")

## occurrences
spN<-"Panthera onca"
occs <-  occs_queryDb(spName = spN, occDb = "gbif", occNum = 100)
occs <- as.data.frame(occs[[1]]$cleaned)

## enviromental variables
envs <- envs_worldclim(bcRes = 10, bcSel = c("bio01","bio02","bio13","bio14"), doBrick = FALSE)

## background extent
bgExt <- penvs_bgExtent(occs, bgSel = 'bounding box', bgBuf = 0.5,spN=spN)

### run function
bgMask <- penvs_bgMask(occs, envs, bgExt,spN=spN)


### test if the error messages appear when they are supposed to
test_that("error checks", {
  # the user has not selected the background extent
  expect_error(penvs_bgMask(occs, envs, bgExt=NULL,spN=spN),
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
  expect_true(raster::cellStats(bgMask$bio01, sum) < raster::cellStats(envs$bio01, sum))
  expect_true(raster::cellStats(bgMask$bio02, sum) < raster::cellStats(envs$bio02, sum))
  expect_true(raster::cellStats(bgMask$bio13, sum) < raster::cellStats(envs$bio13, sum))
  expect_true(raster::cellStats(bgMask$bio14, sum) < raster::cellStats(envs$bio14, sum))
})
