#### COMPONENT proj: Project Model
#### MODULE: Project to New time
context("proj_time")

source("test_helper_functions.R")

###
spN="Panthera onca"
## occurrences
out.gbif <- occs_queryDb(spName = spN, occDb = "gbif", occNum = 1000)
occs <- as.data.frame(out.gbif[[1]]$cleaned)
## background mask
# enviromental data
envs <- envs_worldclim(bcRes = 10, bcSel = c('bio01','bio19'), doBrick = FALSE)
# background extent
bgExt <- penvs_bgExtent(occs, bgSel = 'bounding box', bgBuf = 0.5,spN=spN)
# background masked
bgMask <- penvs_bgMask(occs, envs, bgExt,spN=spN)
## background sample
bg <- penvs_bgSample(occs, bgMask, bgPtsNum = 10000,spN=spN)

## Partition
partblock <- part_partitionOccs(occs, bg, method = 'block', kfolds = NULL, bgMask = NULL,
                                aggFact = NULL,spN=spN)
## model
# regularization multipliers
rms <- c(1:2)
# regularization multipliers step value
rmsStep <- 1
# feature classes
fcs <- c('L', 'H', 'LQH')

## extent to project
# set coordinates
longitude <- c(-71.58400, -78.81300, -79.34034, -69.83331, -66.47149, -66.71319, -71.11931)
latitude <- c(13.18379, 7.52315, 0.93105, -1.70167, 0.98391, 6.09208, 12.74980)
selCoords <- matrix(c(longitude, latitude), byrow = F, ncol = 2)
expertAddedPoly <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(selCoords)), ID=1)))
##projection time layers, using worldclim 2.1 Future 2021-2040 MIROC6 ssp126 bioclims as example
envsFut<-list.files(path='./wc10/Future', pattern = ".tif$", full.names = TRUE)
envsFut<-raster::stack(envsFut)
###iterating items
# outputType
outputType <- c('raw', 'logistic', 'cloglog')
# algorithm
algorithm <- c('maxent.jar','maxnet','bioclim')
# build model and test for both algorithms
for (i in algorithm) {
  if(i == 'bioclim'){
    modAlg <- model_bioclim(occs, bg, partblock, bgMask,spN=spN)
    curModel=1
  }
  else{
    modAlg <- model_maxent(occs, bg, partblock, bgMask, rms, rmsStep, fcs, clampSel = TRUE,
                           algMaxent = i,catEnvs=NULL,spN=spN)
    curModel='L_1'

  }



  for (j in outputType) {
    ### run function
    modProj <- proj_time(evalOut = modAlg, curModel, envs=envsFut, outputType = j,
                         alg=i,clamp=FALSE, pjExt = expertAddedPoly, spN=spN )


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
      expect_is(modProj$projTime, "RasterLayer")
      # there are as many projection extents as environmental variables used
      expect_equal(raster::nlayers(envs), raster::nlayers(modProj$projExt))
      # there is 1 projection area
      expect_equal(raster::nlayers(modProj$projTime), 1)
    })
  }
}
