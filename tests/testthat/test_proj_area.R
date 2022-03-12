#### COMPONENT proj: Project Model
#### MODULE: Project to New Extent
context("proj_area")

## occurrences
spN="Panthera onca"
out.gbif <- occs_queryDb(spName = spN, occDb = "gbif", occNum = 1000)
occs <- as.data.frame(out.gbif[[1]]$cleaned)

## background mask
# enviromental data
envs <- envs_worldclim(bcRes = 10, bcSel = c("bio01","bio02","bio13","bio14"), doBrick = FALSE)
# background extent
bgExt <- penvs_bgExtent(occs, bgSel = 'bounding box', bgBuf = 0.5)
# background masked
bgMask <- penvs_bgMask(occs, envs, bgExt)
## background sample
bg <- penvs_bgSample(occs, bgMask, bgPtsNum = 10000)

## Partition
partblock <- part_partitionOccs(occs, bg, method = 'block', kfolds = NULL, bgMask = NULL,
                                aggFact = NULL)

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
# generate matrix
selCoords <- matrix(c(longitude, latitude), byrow = F, ncol = 2)
expertAddedPoly <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(selCoords)),
                                                         ID = 1)))
##select coordinates only
occs.xy <- occs %>% dplyr::select(longitude, latitude)
bg.xy <- bg %>% dplyr::select(longitude, latitude)
###iterating items
# outputType
outputType <- c('raw', 'logistic', 'cloglog')
# algorithm
algorithm <- c('maxent.jar','maxnet','BIOCLIM')
# build model and test for both algorithms
for (i in algorithm) {
  if(i== 'BIOCLIM'){
    modAlg <- model_bioclim(occs, bg, partblock, bgMask)
    curModel=1
  }
  else{
    modAlg <- model_maxent(occs.xy, bg.xy, partblock, bgMask, rms, rmsStep, fcs, clampSel = TRUE,
                          algMaxent = i, catEnvs = NULL)
    curModel = 'fc.L_rm.1'
  }

for (j in outputType) {
  ### run function
  modProj <- proj_area(evalOut = modAlg, curModel, envs, outputType = j,
                       alg = i, clamp = FALSE, pjExt = expertAddedPoly)

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
