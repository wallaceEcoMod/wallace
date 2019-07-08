#### COMPONENT 7: Visualize Model Results
#### MODULE: Maxent Evaluation Plots
context("maxentEvalPlot")

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
fcs <- c('L', 'LQ')
## algorithm
algoritm <- c('maxent.jar','maxnet')
## model
maxentAlg <- runMaxent(occs, bg, occsGrp, bgGrp, bgMsk, rms, rmsStep, fcs, clampSel = TRUE, 
          algMaxent = algoritm[1])

## value
eVal <- c("delta.AICc", "avg.test.AUC", "avg.diff.AUC", "avg.test.orMTP", "avg.test.or10pct")


### run function 
maxentPlot <- makeMaxentEvalPlot(evalTbl = maxentAlg$evalTbl, value = eVal[1])


## test if the error messages appear when they are supposed to 
test_that("error checks", {
  # the input value isn't right
  expect_error(makeMaxentEvalPlot(evalTbl = maxentAlg$evalTbl, value = "test.or10pct"))
  })

### test output features 
i <- eVal[1]
for (i in eVal) {
  ### run function
  maxentPlot <- makeMaxentEvalPlot(evalTbl = maxentAlg$evalTbl, value = i)
  
  test_that("output checks", {
    # the output is a list
    expect_is(maxentPlot, "list")
    # the output has 2 elements
    expect_equal(length(maxentPlot), 2)
    # the two elements are lists too
    expect_is(maxentPlot[c("rect", "text")], "list")
    # the names are right
    expect_equal(names(maxentPlot), c("rect", "text"))
  })
}
   