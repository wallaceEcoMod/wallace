#### COMPONENT model: Build and Evaluate Niche Model
#### MODULE: Maxent.Jar & Maxnet
context("model_maxent")

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

## regularization multipliers
rms <- 1:2
## regularization multipliers step value
rmsStep <- 1
## feature classes
fcs <- c('L', 'LQ')

## algorithm
algorithm <- c('maxent.jar','maxnet')

# Java file route
jar <- paste(system.file(package = "dismo"), "/java/maxent.jar", sep = '')
# wrong java file route
jar_f <- paste(system.file(package = "dismo"), "/maxent.jar", sep = '')
## test if the error messages appear when they are supposed to
test_that("error checks", {
  # user has not partitioned occurrences
  expect_error(model_maxent(occs, bg, bgMsk = envs, user.grp = NULL,
                            rms, rmsStep, fcs, clampSel = TRUE,
                            algMaxent = algorithm[1]),
               paste0("Before building a model, please partition occurrences ",
                      "for cross-validation."))
})
##missing 2 errors related to jar


### test output features

for (i in algorithm) {
  ### run function

  maxentAlg <- model_maxent(occs = occs, bg = bg, user.grp = partblock,
                            bgMsk = envs, rms, rmsStep, fcs, clampSel = TRUE,
                            algMaxent = i, parallel = FALSE)

  test_that("output type checks", {
    # the output is an ENMeval object
    expect_is(maxentAlg, "ENMevaluation")
    #the output has 9 slots with correct names
    expect_equal(length(slotNames(maxentAlg)), 20)
    expect_equal(slotNames(maxentAlg),
                 c("algorithm", "tune.settings", "partition.method",
                   "partition.settings", "other.settings", "doClamp",
                   "clamp.directions", "results", "results.partitions",
                   "models", "variable.importance", "predictions", "taxon.name",
                   "occs", "occs.testing", "occs.grp", "bg", "bg.grp",
                   "overlap", "rmm"))
    # element within the evaluation are:
    # character
    expect_is(maxentAlg@algorithm, "character")
    expect_is(maxentAlg@partition.method, "character")
    # a data frame
    expect_is(maxentAlg@tune.settings, "data.frame")
    expect_is(maxentAlg@results, "data.frame")
    expect_is(maxentAlg@results.partitions, "data.frame")
    expect_is(maxentAlg@occs, "data.frame")
    expect_is(maxentAlg@bg, "data.frame")
    # a list
    expect_is(maxentAlg@partition.settings, "list")
    expect_is(maxentAlg@other.settings, "list")
    expect_is(maxentAlg@models, "list")
    expect_is(maxentAlg@variable.importance,"list")
    # a raster Stack
    expect_is(maxentAlg@predictions, "RasterStack")
    # factor
    expect_is(maxentAlg@occs.grp, "factor")
    expect_is(maxentAlg@bg.grp, "factor")
    # there is 1 model
    # there are as much models as feature classes * rms/rmsStep
    expect_equal(length(maxentAlg@models), (length(rms)/rmsStep)*length(fcs))
    # as many rasters as models are generated
    expect_equal(length(maxentAlg@models), raster::nlayers(maxentAlg@predictions))
    # there is a model for each combination of feature classes and
    # regularization multiplier
    expect_equal(sort(as.character(maxentAlg@results$tune.args)),
                 sort(paste0("fc.", rep(fcs, each = length(rms) / rmsStep),
                             "_", "rm.", seq(rms[1], rms[2], by = rmsStep))))
    # evaluation table has the right amout of rows
    expect_equal(nrow(maxentAlg@results), (length(rms)/rmsStep)*length(fcs))
    # columns name in the evaluation table are right for each algorithm
    # assuming block partition
    if (i == "maxent.jar"){
    expect_equal(colnames(maxentAlg@results),
                 c("fc", "rm", "tune.args", "auc.train", "cbi.train",
                   "auc.diff.avg", "auc.diff.sd", "auc.val.avg", "auc.val.sd",
                   "cbi.val.avg", "cbi.val.sd", "or.10p.avg", "or.10p.sd",
                   "or.mtp.avg", "or.mtp.sd", "AICc", "delta.AICc", "w.AIC",
                   "ncoef"))
    }
    else if (i=="maxnet"){
    expect_equal(colnames(maxentAlg@results),
                 c("fc", "rm", "tune.args", "auc.train", "cbi.train",
                   "auc.diff.avg", "auc.diff.sd", "auc.val.avg", "auc.val.sd",
                   "cbi.val.avg", "cbi.val.sd", "or.10p.avg", "or.10p.sd",
                   "or.mtp.avg", "or.mtp.sd", "AICc", "delta.AICc", "w.AIC",
                   "ncoef"))
    }
    # bin evaluation table has the right amout of rows
    expect_equal(nrow(maxentAlg@results.partitions),
                 (nlevels(factor(partblock$occs.grp))) * 4)
    ##colnumbers minus the 16 minimum
  })

  ### test function stepts
  test_that("output data checks", {
    # the AUC values are between 0 and 1
    expect_false(FALSE %in% ((maxentAlg@results[c("auc.val.avg",
                                                  "auc.val.sd", "auc.diff.avg",
                                                  "auc.diff.sd")]) <= 1 |
                               (maxentAlg@results[c("auc.val.avg", "auc.val.sd",
                                                    "auc.diff.avg",
                                                    "auc.diff.sd")]) >= 0))
  })
}
