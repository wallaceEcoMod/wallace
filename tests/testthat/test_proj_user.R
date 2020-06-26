#### COMPONENT proj: Project Model
#### MODULE: Project to User provided area
context("proj_user")

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

## extent to project from user provided shapefile
Path <- list.files(path='./shapefile', pattern = "COL_adm0.", full.names = TRUE)
userExt<-rgdal::readOGR(Path[2])
###iterating items
# outputType
outputType <- c('raw', 'logistic', 'cloglog')
# algorithm
algorithm <- c('maxent.jar','maxnet','bioclim')
# build model and test for both algorithms
for (i in algorithm) {
  if(i == 'bioclim'){
    modAlg <- model_bioclim(occs, bg, occsGrp, bgGrp, bgMask,spN=occs)
    curModel=1
  }
  else{
    modAlg <- model_maxent(occs, bg, occsGrp, bgGrp, bgMask, rms, rmsStep, fcs, clampSel = TRUE,
                           algMaxent = i,catEnvs=NULL,spN=occs)
    curModel='L_1'
  }



  for (j in outputType) {
    ### run function
    modProj <- proj_user(evalOut = modAlg, curModel, envs=envs, outputType = j,
                         alg=i,clamp=FALSE, pjExt = userExt )


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
      expect_is(modProj$projUser, "RasterLayer")
      # there are as many projection extents as environmental variables used
      expect_equal(raster::nlayers(envs), raster::nlayers(modProj$projExt))
      # there is 1 projection area
      expect_equal(raster::nlayers(modProj$projUser), 1)
    })
  }
}
