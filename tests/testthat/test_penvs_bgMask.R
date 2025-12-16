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
crs(envs) <- "EPSG:4326"
bgExt <- penvs_bgExtent(occs, bgSel = 'minimum convex polygon', bgBuf = 0.5)
crs(bgExt) <- "EPSG:4326"
bgMask <- penvs_bgMask(occs, envs, bgExt)


### test if the error messages appear when they are supposed to
test_that("error checks", {
  # the user has not selected the background extent
  expect_error(penvs_bgMask(occs, envs, bgExt = NULL),
               'Before sampling background points, define the background extent.')
})

### test output features
test_that("output type checks", {
  # the output is a SpatRaster
  expect_is(bgMask, "SpatRaster")
  # the amount of masked layers are the same as uploaded in the comp. 3
  expect_equal(raster::nlayers(envs), nlyr(bgMask))
  # the masked layers are the same as uploaded in the comp. 3
  expect_equal(names(bgMask), names(envs))
  # all the environmental layers have the same amount of pixels
  expect_equal(terra::ncell(bgMask),  raster::ncell(envs))
  # the original layers have more pixels than the masked ones
  expect_true(
    terra::global(bgMask$bio05, "notNA") < raster::ncell(envs$bio05))
  expect_true(
    terra::global(bgMask$bio06, "notNA") < raster::ncell(envs$bio06))
  expect_true(
    terra::global(bgMask$bio13, "notNA") < raster::ncell(envs$bio13))
  expect_true(
    terra::global(bgMask$bio14, "notNA") < raster::ncell(envs$bio14))
})
