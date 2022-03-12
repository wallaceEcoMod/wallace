#### COMPONENT proj: Project Model
#### MODULE: Project to User provided area
context("proj_userEnvs")

## occurrences
spN="Panthera onca"
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
rms <- c(1, 1)
# regularization multipliers step value
rmsStep <- 1
# feature classes
fcs <- c('L')

## extent to project from user provided shapefile
Path <- list.files(path = system.file("extdata/shp", package = "wallace"),
                   pattern = "COL_adm0.", full.names = TRUE)
userExt<-rgdal::readOGR(Path[2])
#new envs
envsFut<-list.files(path = system.file("extdata/wc10/future", package = "wallace"),
                    pattern = ".tif$", full.names = TRUE)
envsFut<-raster::stack(envsFut)
###iterating items
# outputType
outputType <- c('raw', 'logistic', 'cloglog')
# algorithm
algorithm <- c('maxent.jar','maxnet','BIOCLIM')
# build model and test for both algorithms
for (i in algorithm) {
  if(i == 'BIOCLIM'){
    modAlg <- model_bioclim(occs, bg, partblock, bgMask,spN=spN)
    curModel=1
  }
  else{
    modAlg <- model_maxent(occs, bg, partblock, bgMask, rms, rmsStep, fcs, clampSel = TRUE,
                           algMaxent = i,catEnvs=NULL,spN=spN)
    curModel='fc.L_rm.1'
  }



  for (j in outputType) {
    ### run function
    modProj <- proj_userEnvs(evalOut = modAlg, curModel, envs=envsFut, outputType = j,
                              alg=i,clamp=FALSE, pjExt = userExt, spN=spN)


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
