#### COMPONENT proj: Project Model
#### MODULE: Project to New Extent
context("proj_area")

source("test_helper_functions.R")

## occurrences
out.gbif <- occs_queryDb(spName = "panthera onca", occDb = "gbif", occNum = 100)
occs <- as.data.frame(out.gbif[[1]]$cleaned)
## background mask
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
# occurrences partitioned
occsGrp = partblock$occ.grp
# background points partitioned
bgGrp = partblock$bg.grp

## model
# regularization multipliers
rms <- c(1:2)
# regularization multipliers step value
rmsStep <- 1
# feature classes
fcs <- c('L', 'H', 'LQH')
# algorithm
algorithm <- c('maxent.jar','maxnet')

# build model and test for both algorithms
for (i in algorithm) {
maxentAlg <- model_maxent(occs, bg, occsGrp, bgGrp, bgMask, rms, rmsStep, fcs, clampSel = TRUE,
                          algMaxent = i,catEnvs=NULL,spN=occs)

## extent to project
# set coordinates
longitude <- c(-71.58400, -78.81300, -79.34034, -69.83331, -66.47149, -66.71319, -71.11931)
latitude <- c(13.18379, 7.52315, 0.93105, -1.70167, 0.98391, 6.09208, 12.74980)
# generate matrix
expertAddedPoly <- matrix(c(longitude, latitude), byrow = F, ncol = 2)

# outputType
outputType <- c('raw', 'logistic', 'cloglog')
j<-'raw'
i<-'maxent.jar'
for (j in outputType) {
  ### run function
  modProj <- proj_area(evalOut = maxentAlg@results, curModel = 'L_1', envs, outputType = j,
                            alg=i,pjExt = expertAddedPoly )
  proj_area <- function(evalOut, curModel, envs, outputType, alg, clamp, pjExt,
                        logger = NULL)
   ### test output features
  test_that("output type checks", {
    # the output is a list
    expect_is(modProj, "list")
    # the output list has five elements
    expect_equal(length(modProj), 2)
    # element within the output list are:
    # a rasterBrick
    expect_is(modProj$projExt, "RasterBrick")
    # a rasterLayer
    expect_is(modProj$projArea, "RasterLayer")
    # there are as many projection extents as environmental variables used
    expect_equal(raster::nlayers(envs), raster::nlayers(modProj$projExt))
    # there is 1 projection area
    expect_equal(raster::nlayers(modProj$projArea), 1)
  })
}
}
