#### COMPONENT 6: Build and Evaluate Niche Model
#### MODULE: BIOCLIM 
context("runBIOCLIM")

source("test_helper_functions.R")


### Set parameters

## get records
out.gbif <- c1_queryDb(spName = "panthera onca", occDb = "gbif", occNum = 100)
occs <- as.data.frame(out.gbif$Panthera_onca$cleaned)

## process data
occs <- c2_thinOccs(occs = occs, thinDist = 10)

## background
# enviromental data
envs <- c3_worldclim(bcRes = 10, bcSel = list(TRUE,TRUE,TRUE,TRUE,TRUE), doBrick = FALSE)
# background extent 
bgExt <- c4_bgExtent(occs, bgSel = 'bounding box', bgBuf = 0.5) 
# background masked 
bgMask <- c4_bgMask(occs, envs, bgExt)
## background sample
bg <- c4_bgSample(occs, bgMask, bgPtsNum = 10000) 

## Partition 
partblock <- c5_partitionOccs(occs, bg, method = 'block', kfolds = NULL, bgMask = NULL,
                              aggFact = NULL)

### run function
bioclimAlg <- runBIOCLIM(occs, bg, partblock$occ.grp, partblock$bg.grp, bgMask) 


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
