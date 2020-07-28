#### COMPONENT proj: Project Model
#### MODULE: Generate MESS map of projection layers
context("proj_mess")

source("test_helper_functions.R")

## occurrences
spN<-"Panthera onca"
out.gbif <- occs_queryDb(spName = spN, occDb = "gbif", occNum = 100)
occs <- as.data.frame(out.gbif[[1]]$cleaned)
## background mask
# enviromental data
envs <- envs_worldclim(bcRes = 10, bcSel = c('bio01','bio19'), doBrick = FALSE)
# background extent
bgExt <- penvs_bgExtent(occs, bgSel = 'bounding box', bgBuf = 0.5,spN=spN)
# background masked
bgMsk <- penvs_bgMask(occs, envs, bgExt,spN=spN)
## background sample
bg <- penvs_bgSample(occs, bgMsk, bgPtsNum = 10000,spN=spN)

## Partition
partblock <- part_partitionOccs(occs, bg, method = 'block', kfolds = NULL, bgMask = NULL,
                                aggFact = NULL,spN=spN)
### Create model
bioclimAlg <- model_bioclim(occs, bg, partblock, bgMsk,spN=spN)
modelOccs<-bioclimAlg@occs
modelBg<-bioclimAlg@bg
## extent to project
# set coordinates
longitude <- c(-71.58400, -78.81300, -79.34034, -69.83331, -66.47149, -66.71319, -71.11931)
latitude <- c(13.18379, 7.52315, 0.93105, -1.70167, 0.98391, 6.09208, 12.74980)
# generate matrix
selCoords <- matrix(c(longitude, latitude), byrow = F, ncol = 2)
expertAddedPoly <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(selCoords)), ID=1)))
##projection time layers, using worldclim 2.1 Future 2021-2040 MIROC6 ssp126 bioclims as example
envsFut<-list.files(path='./wc10/Future', pattern = ".tif$", full.names = TRUE)
envsFut<-raster::stack(envsFut)
projExtRas<-raster::crop(envsFut,expertAddedPoly)
projExtRas<-raster::mask(projExtRas,expertAddedPoly)
time<-"2021-2040 MIROC6 ssp126"
### run function
projMess<- proj_mess(occs=modelOccs, bg=modelBg, bgMsk=bgMsk, projExtRas=projExtRas, time=time, logger = NULL)

    ### test output features
    test_that("output type checks", {
      # the output is a list
      expect_is(projMess, "RasterLayer")
      # the output has the sime extent as requested
      expect_equal(raster::extent(projMess),raster::extent(projExtRas))
      # RasterLayer contains numeric mess values
      expect_type(raster::values(projMess),"double")

    })
