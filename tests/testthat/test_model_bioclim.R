#### COMPONENT 6: Build and Evaluate Niche Model : model
#### MODULE: BIOCLIM
context("model_bioclim")

source("test_helper_functions.R")


### Set parameters

## get records
out.gbif <- occs_queryDb(spName = "panthera onca", occDb = "gbif", occNum = 100)
occs <- as.data.frame(out.gbif[[1]]$cleaned)

## process data
occs <- poccs_thinOccs(occs = occs, thinDist = 10,spN=occs)

## background
# enviromental data
envs <- envs_worldclim(bcRes = 10, bcSel = list(TRUE,TRUE,TRUE,TRUE,TRUE), doBrick = FALSE)
# background extent
bgExt <- penvs_bgExtent(occs, bgSel = 'bounding box', bgBuf = 0.5,spN=occs)
# background masked
bgMask <- penvs_bgMask(occs, envs, bgExt,spN=occs)
## background sample
bg <- penvs_bgSample(occs, bgMask, bgPtsNum = 10000,spN=occs)

## Partition
partblock <- part_partitionOccs(occs, bg, method = 'block', kfolds = NULL, bgMask = NULL,
                              aggFact = NULL,spN=occs)

### run function
bioclimAlg <- model_bioclim(occs, bg, partblock$occ.grp, partblock$bg.grp, bgMask,spN=occs)


### test output features
test_that("output type checks", {
  # the output is a list
  expect_is(bioclimAlg, "list")
  # the output list has five elements
  expect_equal(length(bioclimAlg), 5)
  # element within the output list are:
  # lists
  expect_is(bioclimAlg [c("models", "evalTbl")], "list")
  # a data frame
  expect_is(bioclimAlg@results.grp, "data.frame")
  # a raster Stack
  expect_is(bioclimAlg@predictions, "RasterStack")
  # a matrix
  expect_is(bioclimAlg@occPredVals, "matrix")
  # there is 1 model
  expect_equal(length(bioclimAlg@models), 1)
  # there is 1 prediction
  expect_equal(raster::nlayers(bioclimAlg@predictions), 1)
  # there are prediction values for the model
  expect_equal(names(bioclimAlg@models), colnames(bioclimAlg@occPredVals))
  # there is 1 model in the evaluation table
  expect_equal(nrow(bioclimAlg@results), 1)
  # columns name in the evaluation table are right
  expect_equal(colnames(bioclimAlg@results),c("avg.test.AUC", "var.test.AUC", "avg.diff.AUC",
                                              "var.diff.AUC", "avg.test.orMTP", "var.test.orMTP",
                                              "avg.test.or10pct", "var.test.or10pct"))
  # rows name in the evaluation table are right
  expect_equal(rownames(bioclimAlg@results), "BIOCLIM")
  # there is 1 model in the bin evaluation table
  expect_equal(nrow(bioclimAlg@results.grp), 1)
  # bin evaluation table has the right amout of columns
  expect_equal(ncol(bioclimAlg@results.grp), (length(unique(partblock$occ.grp))*4))
  # # rows name in the evaluation table are right
  expect_equal(rownames(bioclimAlg@results.grp), "BIOCLIM")
})

### test function stepts
test_that("output data checks", {
  # the AUC values are between 0 and 1
  expect_false(FALSE %in% ((bioclimAlg@results[,c("avg.test.AUC", "var.test.AUC", "avg.diff.AUC",
                                                  "var.diff.AUC")])<1 |
                             (bioclimAlg@results[,c("avg.test.AUC", "var.test.AUC", "avg.diff.AUC",
                                                    "var.diff.AUC")])<0))
  # the predictions generated are within the background mask
  expect_equal(extent(bgMask), extent(bioclimAlg@predictions))
})
