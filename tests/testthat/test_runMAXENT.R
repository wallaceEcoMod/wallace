##### QUESTIONS
  # 1. How to test the maxent.jar error message 


#### COMPONENT 6: Build and Evaluate Niche Model
#### MODULE: Maxent.Jar & Maxnet
context("runMAXENT")

source("test_helper_functions.R")


### Set parameters

## occurrences
out.gbif <- c1_queryDb(spName = "panthera onca", occDb = "gbif", occNum = 100)
occs <- as.data.frame(out.gbif$Panthera_onca$cleaned)

## background mask
# enviromental data
envs <- c3_worldclim(bcRes = 10, bcSel = list(TRUE,TRUE,TRUE,TRUE,TRUE), doBrick = FALSE)
# remove records without enviromental values 
records <- which(is.na(raster::extract(envs$bio1.1, occs[,3:4])) == TRUE)
occs <- occs[-records, ] 
# background extent 
bgExt <- c4_bgExtent(occs, bgSel = 'bounding box', bgBuf = 0.5) 
# background masked 
bgMsk <- c4_bgMask(occs, envs, bgExt)

## background sample
bg <- c4_bgSample(occs, bgMsk, bgPtsNum = 10000) 

## partition data
partblock <- c5_partitionOccs(occs, bg, method = 'block', kfolds = NULL, bgMask = NULL,
                              aggFact = NULL) 
# occurrences partitioned
occsGrp = partblock$occ.grp
# background points partitioned
bgGrp = partblock$bg.grp

## regularization multipliers 
rms <- c(1:2)
## regularization multipliers step value
rmsStep <- 1
## feature classes
fcs <- c('L', 'LQ', 'H', 'LQH', 'LQHP', 'LQHPT')

## algorithm
algoritm <- c('maxent.jar','maxnet')


## test if the error messages appear when they are supposed to 
test_that("error checks", {
  # user has not partitioned occurrences 
  expect_error(runMaxent(occs, bg, occsGrp = NULL, bgGrp, bgMsk, rms, rmsStep, fcs, 
                         clampSel = TRUE, algMaxent = algoritm[1]), "Before building a model, please partition 
                        occurrences for cross-validation.")
  })

### test output features 
i <- algoritm[1]
for (i in algoritm) { 
  ### run function
  maxentAlg <- runMaxent(occs, bg, occsGrp, bgGrp, bgMsk, rms, rmsStep, fcs, clampSel = TRUE, 
                         algMaxent = i)

  test_that("output type checks", {
    # the output is a list
    expect_is(maxentAlg, "list")
    # the output list has five elements 
    expect_equal(length(maxentAlg), 5)
    # element within the output list are: 
      # lists
    expect_is(maxentAlg[c("evalTbl","evalTblBins", "models")], "list")
     # a raster Stack
    expect_is(maxentAlg@predictions, "RasterStack")
      # a matrix
    expect_is(maxentAlg@occPredVals, "matrix")
    # there are as much models as feature classes * rms/rmsStep
    expect_equal(length(maxentAlg@models), (length(rms)/rmsStep)*length(fcs))
    # as many rasters as models are generated
    expect_equal(length(maxentAlg@models), raster::nlayers(maxentAlg@predictions))
    # there is a model for each combination of feature classes and regularization multiplier 
    expect_equal(sort(names(maxentAlg@models)),
                 paste0(sort(rep(fcs, length(rms)/rmsStep)), paste0("_", seq(rms[1], rms[2], by = rmsStep))))
    # there are prediction values for each model 
    expect_equal(sort(names(maxentAlg@models)), sort(colnames(maxentAlg@occPredVals))) 
    # evaluation table has the right amout of rows
    expect_equal(nrow(maxentAlg@results), (length(rms)/rmsStep)*length(fcs))
    # columns name in the evaluation table are right
    expect_equal(names(maxentAlg@results), c("features", "rm", "train.AUC", "avg.test.AUC", "var.test.AUC", 
                                             "avg.diff.AUC", "var.diff.AUC", "avg.test.orMTP", "var.test.orMTP", "avg.test.or10pct", "var.test.or10pct",
                                             "AICc", "delta.AICc", "w.AIC", "parameters"))
    # bin evaluation table has the right amout of columns and rows
    expect_equal(nrow(maxentAlg@results.grp), (length(rms)/rmsStep)*length(fcs))
    expect_equal(ncol(maxentAlg@results.grp), (nlevels(factor(occsGrp)))*4)
  })
  
  ### test function stepts 
  test_that("output data checks", {
    # the AUC values are between 0 and 1
    expect_false(FALSE %in% ((maxentAlg@results[c("var.diff.AUC", "avg.diff.AUC", "var.test.AUC", "avg.test.AUC",
                                              "train.AUC")])<1 | 
                           (maxentAlg@results[c("var.diff.AUC", "avg.diff.AUC", "var.test.AUC", "avg.test.AUC",
                                                "train.AUC")])>0))
  })
  }
