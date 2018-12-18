##### QUESTIONS
  # 1. Should I do it for both check? 
  # 2. error: method is checkerboard but aggregation factor parameter is NULL

#### COMPONENT 6: Partition Occurrence Data
#### MODULE: Non-spatial Partition & Spatial Partition
context("partitionOccs")

source("test_helper_functions.R")


### get data

## occurrence
occs <-  c1_queryDb(spName = "panthera onca", occDb = "gbif", occNum = 50)
occs <- as.data.frame(occs$cleaned)

## background
# enviromental data
envs <- c3_worldclim(bcRes = 10, bcSel = (list(TRUE,TRUE,TRUE,TRUE,TRUE)))
# background extent 
bgExt <- c4_bgExtent(occs, envs, bgSel = 'bb', bgBuf = 0.5)
bg <- as.data.frame(bgExt@polygons[[1]]@Polygons[[1]]@coords)
names(bg) <- c('longitude', 'latitude')
# background masked 
bgMask <- c4_bgMask(occs, envs, bgExt)


### run function
## jackknife
partJack <- c5_partitionOccs(occs, bg, method = 'jack', kfolds=NULL, bgMask=NULL, 
                              aggFact=NULL)
## random k-fold 
partfold <- c5_partitionOccs(occs, bg, method = 'rand', kfolds=4, bgMask=NULL, 
                             aggFact=NULL) 
## block 
partblock <- c5_partitionOccs(occs, bg, method = 'block', kfolds=NULL, 
                              bgMask=NULL, aggFact=NULL) 
## checkerboard 1 (K=2)
partche1 <- c5_partitionOccs(occs, bg, method = 'cb1', kfolds=NULL, bgMask, 
                              aggFact=2) 
## checkerboard 2 (K=4)
partche2 <- c5_partitionOccs(occs, bg, method = 'cb2', kfolds=NULL, bgMask, 
                             aggFact=3) 


## test if the error messages appear when they are supposed to 
test_that("error checks", {
  # the user has not selected a partitioning option
  expect_error(c5_partitionOccs(occs, bg, method='', kfolds=NULL, bgMask=NULL, 
                   aggFact=NULL), 'Please select a partitioning option.')
  # method is random partition but kfolds parameter is NULL 
  expect_error(c5_partitionOccs(occs, bg, method='rand', kfolds=NULL, bgMask=NULL, 
                                aggFact=NULL), paste0("Please specify a kfold value to 
                             use the random partition function for  ", 
               em("Panthera onca"), "."))
  # kfolds < 2 
  expect_error(c5_partitionOccs(occs, bg, method='rand', kfolds=1, bgMask=NULL, 
                                aggFact=NULL), paste0("Please specify a kfold value 
                             greater than 1 for  ", em("Panthera onca"), "."))
  # method is checkerboard 1 but aggregation factor parameter <= 1
  expect_error(c5_partitionOccs(occs, bg, method='cb1', kfolds=NULL, bgMask, 
                                aggFact=-2), paste0("Please specify a positive aggregation 
                        factor greater than 1 for  ", em("Panthera onca"), "."))
  # method is checkerboard 2 but aggregation factor parameter <= 1
  expect_error(c5_partitionOccs(occs, bg, method='cb2', kfolds=NULL, bgMask, 
                                aggFact=-2), paste0("Please specify a positive aggregation 
                        factor greater than 1 for  ", em("Panthera onca"), "."))
  # method is checkerboard but athe bnMask parameter is NULL
  expect_error(c5_partitionOccs(occs, bg, method='cb2', kfolds=NULL, bgMask=NULL, 
                                aggFact=2), paste0("Please specify a background mask 
                             to use checkerboard partition functions for  ", 
                                                   em("Panthera onca"), "."))
  # method is checkerboard but aggregation factor parameter is NULL
  #expect_error(c5_partitionOccs(occs, bg, method='cb1', kfolds=NULL, bgMask, 
                                #aggFact=NULL), paste0("Please specify an aggregation 
                             #factor to use checkerboard partition functions for  ",
                                                      #em("Panthera onca"), "."))
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
  # jackknife :the number of partition is the same as the number of records
  expect_equal(nlevels(factor(partJack$occ.grp)), nrow(occs))
  # random: the number of partition is the same as specified in the function (kfolds)
  expect_equal(nlevels(factor(partfold$occ.grp)), 4)
  # block: the number of partition is 4
  expect_equal(nlevels(factor(partblock$occ.grp)), 4)
  # checkerboard 1: the number of partition is 2
  expect_equal(nlevels(factor(partche1$occ.grp)), 2)
  # checkerboard 2: the number of partition is 4
  expect_equal(nlevels(factor(partche2$occ.grp)), 4)
})
