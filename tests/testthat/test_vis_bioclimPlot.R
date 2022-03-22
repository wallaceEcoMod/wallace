#### COMPONENT vis: Visualize Model Results
#### MODULE: BIOCLIM Envelope Plots
context("bioclimPlot")

### Set parameters

## get records
envs <- envs_userEnvs(rasPath = list.files(system.file("extdata/wc",
                                           package = "wallace"),
                      pattern = ".tif$", full.names = TRUE),
                      rasName = list.files(system.file("extdata/wc",
                                           package = "wallace"),
                      pattern = ".tif$", full.names = FALSE))
occs <- read.csv(system.file("extdata/Bassaricyon_alleni.csv",
                 package = "wallace"))
bg <- read.csv(system.file("extdata/Bassaricyon_alleni_bgPoints.csv",
               package = "wallace"))
partblock <- part_partitionOccs(occs, bg, method = 'block')
m <- model_bioclim(occs, bg, partblock, envs)

### run function
bioclimPlot <- vis_bioclimPlot(x = m@models$bioclim, a = 2, b = 3,
                               p = 0.7)
bioclimPlot <- recordPlot(bioclimPlot)


## test if the error messages appear when they are supposed to
test_that("error checks", {
  # the user specified a variable that wasn't included within the model
  expect_error(vis_bioclimPlot(x = m@models$bioclim,
                               a = (raster::nlayers(envs)) + 1,
                               b = 2, p = 1))
  expect_error(vis_bioclimPlot(x = m@models$bioclim, a= 1,
                               b = (raster::nlayers(envs)) + 1, p = 1))
})

### test output features
test_that("output checks", {
  # the output is a recorded plot
  expect_is(bioclimPlot, "recordedplot")
  # the list has three elements
  expect_equal(length(bioclimPlot), 3)
  # the three elements are lists
  expect_is((bioclimPlot[c(1, 2, 3)]), "list")
})
