#### COMPONENT penvs: Process Environmental Data
#### MODULE: Select Study Region
context("bgMask")

occs <- read.csv(system.file("extdata/Bassaricyon_alleni.csv",
                 package = "wallace"))[, 2:3]
occs$occID <- 1:nrow(occs)
envs <- envs_userEnvs(rasPath = list.files(system.file("extdata/wc",
                                           package = "wallace"),
                      pattern = ".tif$", full.names = TRUE),
                      rasName = list.files(system.file("extdata/wc",
                                           package = "wallace"),
                      pattern = ".tif$", full.names = FALSE))
bgExt <- penvs_bgExtent(occs, bgSel = 'minimum convex polygon', bgBuf = 0.5)
bgMask <- penvs_bgMask(occs, envs, bgExt)


### test if the error messages appear when they are supposed to
test_that("error checks", {
  # the user has not selected the background extent
  expect_error(penvs_bgMask(occs, envs, bgExt = NULL),
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
  expect_true(
    raster::cellStats(bgMask$bio05, sum) < raster::cellStats(envs$bio05, sum))
  expect_true(
    raster::cellStats(bgMask$bio06, sum) < raster::cellStats(envs$bio06, sum))
  expect_true(
    raster::cellStats(bgMask$bio13, sum) < raster::cellStats(envs$bio13, sum))
  expect_true(
    raster::cellStats(bgMask$bio14, sum) < raster::cellStats(envs$bio14, sum))
})
