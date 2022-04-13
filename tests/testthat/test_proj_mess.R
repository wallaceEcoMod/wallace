#### COMPONENT xfer: Transfer Model
#### MODULE: Generate MESS map of transferring layers
context("xfer_mess")

envs <- envs_userEnvs(rasPath = list.files(system.file("extdata/wc",
                                           package = "wallace"),
                      pattern = ".tif$", full.names = TRUE),
                      rasName = list.files(system.file("extdata/wc",
                                           package = "wallace"),
                      pattern = ".tif$", full.names = FALSE))
# load model
m <- readRDS(system.file("extdata/model.RDS",
                         package = "wallace"))
occsEnvs <- m@occs
bgEnvs <- m@bg
envsFut <- list.files(path = system.file('extdata/wc/future',
                                         package = "wallace"),
                      full.names = TRUE)
envsFut <- raster::stack(envsFut)
## run function
xferMess <- xfer_mess(occs = occsEnvs, bg = bgEnvs, bgMsk = envs,
                      xferExtRas = envsFut)

## test output features
test_that("output type checks", {
  # the output is a list
  expect_is(xferMess, "RasterLayer")
  # the output has the sime extent as requested
  expect_equal(raster::extent(xferMess), raster::extent(envsFut))
  # RasterLayer contains numeric mess values
  expect_type(raster::values(xferMess), "double")
})
