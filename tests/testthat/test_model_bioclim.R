#### COMPONENT 6: Build and Evaluate Niche Model : model
#### MODULE: BIOCLIM
context("model_bioclim")

### Set parameters
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
bioclimAlg <- model_bioclim(occs, bg, partblock, envs)

### test output features
test_that("output type checks", {
  # the output is a list
  expect_is(bioclimAlg, "ENMevaluation")
  #the output has 9 slots with correct names
  expect_equal(length(slotNames(bioclimAlg)), 20)
  expect_equal(
    slotNames(bioclimAlg),
    c("algorithm", "tune.settings", "partition.method", "partition.settings",
      "other.settings", "doClamp", "clamp.directions", "results",
      "results.partitions", "models", "variable.importance", "predictions",
      "taxon.name", "occs", "occs.testing", "occs.grp", "bg", "bg.grp",
      "overlap", "rmm"))
  # element within the evaluation are:
  # character
  expect_is(bioclimAlg@algorithm, "character")
  expect_is(bioclimAlg@partition.method, "character")
  # a data frame
  expect_is(bioclimAlg@tune.settings, "data.frame")
  expect_is(bioclimAlg@results, "data.frame")
  expect_is(bioclimAlg@results.partitions, "data.frame")
  expect_is(bioclimAlg@occs, "data.frame")
  expect_is(bioclimAlg@bg, "data.frame")
  # a list
  expect_is(bioclimAlg@partition.settings, "list")
  expect_is(bioclimAlg@other.settings, "list")
  expect_is(bioclimAlg@models, "list")
  # a raster Stack
  expect_is(bioclimAlg@predictions, "RasterStack")
  # factor
  expect_is(bioclimAlg@occs.grp, "factor")
  expect_is(bioclimAlg@bg.grp, "factor")
  # there is 1 model
  expect_equal(length(bioclimAlg@models), 1)
  # there is 1 prediction
  expect_equal(raster::nlayers(bioclimAlg@predictions), 1)
  # there is 1 model in the evaluation table
  expect_equal(nrow(bioclimAlg@results), 1)
  # columns name in the evaluation table are right
  expect_equal(
    colnames(bioclimAlg@results),
    c("auc.train", "cbi.train", "auc.diff.avg", "auc.diff.sd", "auc.val.avg",
      "auc.val.sd", "cbi.val.avg","cbi.val.sd", "or.10p.avg", "or.10p.sd",
      "or.mtp.avg", "or.mtp.sd", "ncoef"))

  # there are as many models in the bin evaluation table as partitions
  expect_equal(nrow(bioclimAlg@results.partitions),
               length(unique(partblock$occs.grp)))

  # # col name in the evaluation table are right
  expect_equal(colnames(bioclimAlg@results.partitions),
               c("tune.args", "fold", "auc.val", "auc.diff", "cbi.val",
                 "or.mtp", "or.10p"))
})

### test function stepts
test_that("output data checks", {
  # the AUC values are between 0 and 1
  expect_false(FALSE %in% ((
    bioclimAlg@results[, c("auc.val.avg", "auc.val.sd", "auc.diff.avg", "auc.diff.sd")]) <= 1 |
      (bioclimAlg@results[, c("auc.val.avg", "auc.val.sd", "auc.diff.avg", "auc.diff.sd")]) <= 0))
  # the predictions generated are within the background mask
  expect_equal(raster::extent(envs), raster::extent(bioclimAlg@predictions))
})
