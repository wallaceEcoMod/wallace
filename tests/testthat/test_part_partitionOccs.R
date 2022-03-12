#### COMPONENT part: Partition Occurrence Data
#### MODULE: Non-spatial Partition & Spatial Partition
context("partitionOccs")

### Set parameters

## occurrences
spN<-"Panthera onca"
occs <-  occs_queryDb(spName = spN, occDb = "gbif", occNum = 100)
occs <- as.data.frame(occs[[1]]$cleaned)

## background
# enviromental variables
envs <- envs_worldclim(bcRes = 10, bcSel = c("bio03", "bio04", "bio13", "bio14"),
                       doBrick = FALSE)
# background extent
bgExt <- penvs_bgExtent(occs, bgSel = 'bounding box', bgBuf = 0.5,spN=spN)

## background mask
bgMask <- penvs_bgMask(occs, envs, bgExt,spN=spN)
##Background points
bgSample <- penvs_bgSample(occs, bgMask, bgPtsNum=1000,spN=spN)


# partition methodology
jack <- 'jack' # Non-spatial Partition - jackknife
folds <- 'rand' # Non-spatial Partition - random k-fold
block <- 'block' # spatial Partition - block
cb1 <- 'cb1' # spatial Partition - checkerboard 1 (K=2)
cb2 <- 'cb2' # spatial Partition - checkerboard 2 (K=4)

## number of folds to partition data
kfolds <- 4

# aggregation factor
aggFact <- 2

### run function

## jackknife
partJack <- part_partitionOccs(occs, bgSample, method = jack, kfolds = NULL, bgMask = NULL,
                             aggFact = NULL,spN=spN)
## random k-fold
partfold <- part_partitionOccs(occs, bgSample, method = folds, kfolds, bgMask = NULL,
                             aggFact = NULL,spN=spN)
## block
partblock <- part_partitionOccs(occs, bgSample, method = block, kfolds = NULL, bgMask = NULL,
                              aggFact = NULL,spN=spN)
## checkerboard 1
partche1 <- part_partitionOccs(occs, bgSample, method = cb1, kfolds = NULL, bgMask,
                             aggFact,spN=spN)
## checkerboard 2
partche2 <- part_partitionOccs(occs, bgSample, method = cb2, kfolds = NULL, bgMask,
                             aggFact,spN=spN)


## test if the error messages appear when they are supposed to
test_that("error checks", {
  # the user has not selected a partitioning option
  expect_error(part_partitionOccs(occs, bgSample, method = '', kfolds = NULL, bgMask = NULL,
                                aggFact = NULL,spN=spN), 'Please select a partitioning option.')
  # method is random partition and kfolds parameter is NULL
  expect_error(part_partitionOccs(occs, bgSample, method = folds, kfolds = NULL, bgMask = NULL,
                                aggFact = NULL,spN=spN),
                                paste0(alfred.hlSpp(spN),
                                       "Please specify a kfold value to use the random partition function."),fixed=T)
  # method is random partition and kfolds < 2
  expect_error(part_partitionOccs(occs, bgSample, method = folds, kfolds = 1, bgMask = NULL,
                                aggFact = NULL,spN=spN), paste0(alfred.hlSpp(spN),
                                                                "Please specify a kfold value greater than 1."),fixed=T)
  # method is checkerboard 1 but aggregation factor parameter < 1
  expect_error(part_partitionOccs(occs, bgSample, method = cb1, kfolds = NULL, bgMask,
                                aggFact = -2,spN=spN), paste0(alfred.hlSpp(spN),
                                                              "Please specify a positive aggregation factor greater than 1."),fixed=T)
  # method is checkerboard 2 but aggregation factor parameter < 1
  expect_error(part_partitionOccs(occs, bgSample, method = cb2, kfolds = NULL, bgMask,
                                aggFact = -2,spN=spN), paste0(alfred.hlSpp(spN),
                                                              "Please specify a positive aggregation factor greater than 1."),fixed=T)
  # method is checkerboard 1 but aggregation factor parameter = 1
  expect_error(part_partitionOccs(occs, bgSample, method = cb1, kfolds = NULL, bgMask,
                                aggFact = 1,spN=spN), paste0(alfred.hlSpp(spN),
                                                             "Please specify a positive aggregation factor greater than 1."),fixed=T)
  # method is checkerboard 2 but aggregation factor parameter = 1
  expect_error(part_partitionOccs(occs, bgSample, method = cb2, kfolds = NULL, bgMask,
                                aggFact = 1,spN=spN), paste0(alfred.hlSpp(spN),
                                                             "Please specify a positive aggregation factor greater than 1."),fixed=T)
  # method is checkerboard 1 but aggregation factor parameter is NA
  expect_error(part_partitionOccs(occs, bgSample, method = cb1, kfolds = NULL, bgMask,
                                aggFact = NA,spN=spN), paste0(alfred.hlSpp(spN),
                                                              "Please specify a positive aggregation factor greater than 1."),fixed=T)
  # method is checkerboard 2 but aggregation factor parameter is NA
  expect_error(part_partitionOccs(occs, bgSample, method = cb2, kfolds = NULL, bgMask,
                                aggFact = NA,spN=spN), paste0(alfred.hlSpp(spN),
                                                              "Please specify a positive aggregation factor greater than 1."),fixed=T)
  # method is checkerboard 1 but aggregation factor parameter is NULL
  expect_error(part_partitionOccs(occs, bgSample, method = cb1, kfolds = NULL, bgMask,
                                  aggFact = NULL,spN=spN), paste0(alfred.hlSpp(spN),
                                                                   "Please specify an aggregation factor to use checkerboard partition functions."),fixed=T)
  # method is checkerboard 2 but aggregation factor parameter is NULL
  expect_error(part_partitionOccs(occs, bgSample, method = cb2, kfolds = NULL, bgMask,
                                  aggFact = NULL,spN=spN), paste0(alfred.hlSpp(spN),
                                                                  "Please specify an aggregation factor to use checkerboard partition functions."),fixed=T)
  # method is checkerboard 1 but athe bgMask parameter is NULL
  expect_error(part_partitionOccs(occs, bgSample, method = cb1, kfolds = NULL, bgMask = NULL,
                                aggFact = 2,spN=spN), paste0(alfred.hlSpp(spN),
                                                             "Please specify a background mask to use checkerboard partition functions."),fixed=T)
  # method is checkerboard 2 but athe bgMask parameter is NULL
  expect_error(part_partitionOccs(occs, bgSample, method = cb2, kfolds = NULL, bgMask = NULL,
                                aggFact = 2,spN=spN), paste0(alfred.hlSpp(spN),
                                                             "Please specify a background mask to use checkerboard partition functions."),fixed=T)
})

### test output features
test_that("output checks", {
  # the output is a list
  expect_is(partJack, "list")
  expect_is(partfold, "list")
  expect_is(partblock, "list")
  expect_is(partche1, "list")
  expect_is(partche1, "list")
  # the list has two elements
  expect_equal(length(partJack), 2)
  expect_equal(length(partfold), 2)
  expect_equal(length(partblock), 2)
  expect_equal(length(partche1), 2)
  expect_equal(length(partche2), 2)
  # jackknife: the number of partition is the same as the number of records
  expect_equal(nlevels(factor(partJack$occs.grp)), nrow(occs))
  # random: the number of partition is the same as specified in the function (kfolds). 4 in this case
  expect_equal(nlevels(factor(partfold$occs.grp)), kfolds)
  # block: the number of partition is 4
  expect_equal(nlevels(factor(partblock$occs.grp)), 4)
  # checkerboard 1: the number of partition is 2
  expect_equal(nlevels(factor(partche1$occs.grp)), 2)
  # checkerboard 2: the number of partition is 4
  expect_equal(nlevels(factor(partche2$occs.grp)), 4)
})
