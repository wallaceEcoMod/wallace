#### COMPONENT 6: Build and Evaluate Niche Model : model
#### MODULE: BIOCLIM
context("model_bioclim")

### Set parameters
spN="Panthera onca"
## get records
out.gbif <- occs_queryDb(spName = spN, occDb = "gbif", occNum = 100)
occs <- as.data.frame(out.gbif[[1]]$cleaned)

## process data
occs <- poccs_thinOccs(occs = occs, thinDist = 10,spN=spN)

## background
# enviromental data
envs <- envs_worldclim(bcRes = 10, bcSel = c("bio03", "bio04", "bio13", "bio14"),
                       doBrick = FALSE)
# background extent
bgExt <- penvs_bgExtent(occs, bgSel = 'bounding box', bgBuf = 0.5,spN=spN)
# background masked
bgMask <- penvs_bgMask(occs, envs, bgExt,spN=spN)
## background sample
bg <- penvs_bgSample(occs, bgMask, bgPtsNum = 1000,spN=spN)

## Partition
partblock <- part_partitionOccs(occs, bg, method = 'block', kfolds = NULL, bgMask = NULL,
                                aggFact = NULL,spN=spN)

### run function
bioclimAlg <- model_bioclim(occs, bg, user.grp=partblock, bgMask,spN=spN)

### test output features
test_that("output type checks", {
  # the output is a list
  expect_is(bioclimAlg, "ENMevaluation")
  #the output has 9 slots with correct names
  expect_equal(length(slotNames(bioclimAlg)), 20)
  expect_equal(slotNames(bioclimAlg),c("algorithm","tune.settings","partition.method","partition.settings",
                                       "other.settings","doClamp","clamp.directions","results","results.partitions","models","variable.importance",
                                       "predictions","taxon.name","occs","occs.testing","occs.grp","bg","bg.grp","overlap","rmm"))
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
  expect_equal(colnames(bioclimAlg@results),c("auc.train", "cbi.train", "auc.diff.avg",
                                              "auc.diff.sd", "auc.val.avg", "auc.val.sd","cbi.val.avg","cbi.val.sd",
                                              "or.10p.avg","or.10p.sd","or.mtp.avg","or.mtp.sd","ncoef"))

  # there are as many models in the bin evaluation table as partitions
  expect_equal(nrow(bioclimAlg@results.partitions), length(unique(partblock$occs.grp)))

  # # col name in the evaluation table are right
  expect_equal(colnames(bioclimAlg@results.partitions), c( "tune.args", "fold","auc.val","auc.diff","cbi.val","or.mtp","or.10p"))
})

### test function stepts
test_that("output data checks", {
  # the AUC values are between 0 and 1
  expect_false(FALSE %in% ((bioclimAlg@results[,c("auc.val.avg", "auc.val.sd", "auc.diff.avg",
                                                  "auc.diff.sd")])<=1 |
                             (bioclimAlg@results[,c("auc.val.avg", "auc.val.sd", "auc.diff.avg",
                                                    "auc.diff.sd")])<=0))
  # the predictions generated are within the background mask
  expect_equal(raster::extent(bgMask), raster::extent(bioclimAlg@predictions))
})
