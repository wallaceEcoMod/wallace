#### COMPONENT proj: Project Model
#### MODULE: Generate MESS map of projection layers
context("proj_mess")

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
projMess <- proj_mess(occs = occsEnvs, bg = bgEnvs, bgMsk = envs,
                      projExtRas = envsFut)

## test output features
test_that("output type checks", {
  # the output is a list
  expect_is(projMess, "RasterLayer")
  # the output has the sime extent as requested
  expect_equal(raster::extent(projMess), raster::extent(envsFut))
  # RasterLayer contains numeric mess values
  expect_type(raster::values(projMess), "double")
})
