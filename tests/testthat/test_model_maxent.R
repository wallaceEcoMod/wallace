##### QUESTIONS
# 1. How to test the maxent.jar error message


#### COMPONENT 6: Build and Evaluate Niche Model
#### MODULE: Maxent.Jar & Maxnet
context("model_maxent")

source("test_helper_functions.R")


### Set parameters

## occurrences
out.gbif <- occs_queryDb(spName = "panthera onca", occDb = "gbif", occNum = 100)
occs <- as.data.frame(out.gbif[[1]]$cleaned)

## background mask
# enviromental data
envs <- envs_worldclim(bcRes = 10, bcSel = list(TRUE,TRUE,TRUE,TRUE,TRUE), doBrick = FALSE)
# remove records without enviromental values
records <- which(is.na(raster::extract(envs$bio1.1, occs[,3:4])) == TRUE)
occs <- occs[-records, ]
# background extent
bgExt <- penvs_bgExtent(occs, bgSel = 'bounding box', bgBuf = 0.5,spN=occs)
# background masked
bgMsk <- penvs_bgMask(occs, envs, bgExt,spN=occs)

## background sample
bg <- penvs_bgSample(occs, bgMsk, bgPtsNum = 10000,spN=occs)

## partition data
partblock <- part_partitionOccs(occs, bg, method = 'block', kfolds = NULL, bgMask = NULL,
                              aggFact = NULL,spN=occs)
# occurrences partitioned
occsGrp = partblock$occ.grp
# background points partitioned
bgGrp = partblock$bg.grp

## regularization multipliers
rms <- c(1:2)
## regularization multipliers step value
rmsStep <- 1
## feature classes
fcs <- c('L', 'LQ', 'H', 'LQH', 'LQHP')

## algorithm
algoritm <- c('maxent.jar','maxnet')

#Java file route
jar <- paste(system.file(package = "dismo"), "/java/maxent.jar", sep = '')
#wrong java file route
jar_f <- paste(system.file(package = "dismo"), "/maxent.jar", sep = '')
## test if the error messages appear when they are supposed to
test_that("error checks", {
  # user has not partitioned occurrences
  expect_error(model_maxent(occs, bg, occsGrp = NULL, bgGrp, bgMsk, rms, rmsStep, fcs,
                         clampSel = TRUE, algMaxent = algoritm[1]), "Before building a model, please partition occurrences for cross-validation.")
})
##missing 2 errors related to jar


### test output features

for (i in algoritm) {
  ### run function
  maxentAlg <- model_maxent(occs, bg, occsGrp, bgGrp, bgMsk, rms, rmsStep, fcs, clampSel = TRUE,
                         algMaxent = i,catEnvs=NULL,spN=occs)

  test_that("output type checks", {
    # the output is an ENMeval object
    expect_is(maxentAlg, "ENMevaluation")
    #the output has 9 slots with correct names
    expect_equal(length(slotNames(maxentAlg)), 14)
    expect_equal(slotNames(maxentAlg),c("algorithm","tune.settings","partition.method","partition.settings",
                                        "other.settings","results","results.grp","models",
                                        "predictions","occs","occ.grp","bg","bg.grp","overlap"))
    # element within the evaluation are:
    # character
    expect_is(maxentAlg@algorithm, "character")
    expect_is(maxentAlg@partition.method, "character")
    # a data frame
    expect_is(maxentAlg@tune.settings, "data.frame")
    expect_is(maxentAlg@results, "data.frame")
    expect_is(maxentAlg@results.grp, "data.frame")
    expect_is(maxentAlg@occs, "data.frame")
    expect_is(maxentAlg@bg, "data.frame")
    # a list
    expect_is(maxentAlg@partition.settings, "list")
    expect_is(maxentAlg@other.settings, "list")
    expect_is(maxentAlg@models, "list")
    # a raster Stack
    expect_is(maxentAlg@predictions, "RasterStack")
    # factor
    expect_is(maxentAlg@occ.grp, "factor")
    expect_is(maxentAlg@bg.grp, "factor")
    # there is 1 model
    # there are as much models as feature classes * rms/rmsStep
    expect_equal(length(maxentAlg@models), (length(rms)/rmsStep)*length(fcs))
    # as many rasters as models are generated
    expect_equal(length(maxentAlg@models), raster::nlayers(maxentAlg@predictions))
    # there is a model for each combination of feature classes and regularization multiplier
    expect_equal(sort(as.character(maxentAlg@results$tune.args)),
                 sort(paste0(rep(fcs, length(rms)/rmsStep), paste0("_", seq(rms[1], rms[2], by = rmsStep)))))
    # evaluation table has the right amout of rows
    expect_equal(nrow(maxentAlg@results), (length(rms)/rmsStep)*length(fcs))
    # columns name in the evaluation table are right for each algorithm assuming block partition
    if (i=="maxent.jar"){
    expect_equal(colnames(maxentAlg@results),c("fc","rm","tune.args","auc.train","cbi.train","auc.diff.avg",
                                               "auc.diff.sd","auc.test.avg","auc.test.sd","maxKappa.test.avg",
                                               "maxKappa.test.sd","maxTSS.test.avg","maxTSS.test.sd","or.10p.avg","or.10p.sd",
                                               "or.mtp.avg","or.mtp.sd","AICc","delta.AICc","w.AIC","nparam"))
    }
    else if (i=="maxnet"){
    expect_equal(colnames(maxentAlg@results),c("fc","rm","tune.args","auc.train","cbi.train","auc.diff.avg",
                                                 "auc.diff.sd","auc.test.avg","auc.test.sd","or.10p.avg","or.10p.sd",
                                                 "or.mtp.avg","or.mtp.sd","AICc","delta.AICc","w.AIC","nparam"))
    }
    # bin evaluation table has the right amout of rows
    expect_equal(nrow(maxentAlg@results.grp), (nlevels(factor(occsGrp)))*10) ##colnumbers minus the 16 minimum
  })

  ### test function stepts
  test_that("output data checks", {
    # the AUC values are between 0 and 1
    expect_false(FALSE %in% ((maxentAlg@results[c("auc.test.avg", "auc.test.sd", "auc.diff.avg",
                                                  "auc.diff.sd")])<=1 |
                               (maxentAlg@results[c("auc.test.avg", "auc.test.sd", "auc.diff.avg",
                                                    "auc.diff.sd")])>=0))
  })
}
