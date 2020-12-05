#### COMPONENT espace: Do analysis in environmental space.
#### MODULE: Occurence density grid
context("espace_occDens")

source("test_helper_functions.R")

##Function has no warning or error messages.
##To run this function a PCA must exist, this requires a model to be created first allowing for access to environmental data for occs and background
###Using bioclim for testing

###SET PARAMETERS (running model)
sp.name1<-"Pristimantis bogotensis"
sp.name2<-"Dendropsophus labialis"
species<-c(sp.name1,sp.name2)
model<-list()
for (i in 1:2){

  ## occurrences
  occs <-  occs_queryDb(spName = species[i], occDb = "gbif", occNum = 1000)
  occs <- as.data.frame(occs[[1]]$cleaned)
  ## process data
  #occs <- poccs_thinOccs(occs = occs, thinDist = 10,spN=species[i]) ##Removed because warning from spthin on different names
  # enviromental data
  envs <- envs_worldclim(bcRes = 10,  bcSel = c("bio01","bio02","bio13","bio14"), doBrick = FALSE)
  # background extent
  bgExt <- penvs_bgExtent(occs, bgSel = 'bounding box', bgBuf = 0.5,spN=species[i])
  # background masked
  bgMask <- penvs_bgMask(occs, envs, bgExt,spN=species[i])
  ## background sample
  bg <- penvs_bgSample(occs, bgMask, bgPtsNum = 1000,spN=species[i])
  ## Partition
  partblock <- part_partitionOccs(occs, bg, method = 'block', kfolds = NULL, bgMask = NULL,
                                  aggFact = NULL,spN=species[i])
  ### Create model
  bioclimAlg <- model_bioclim(occs, bg, partblock, bgMask,spN=species[i])

  model[[i]]<-bioclimAlg
}
##Set parameters
##Remove coordinates (lat/long from tables)
occs.z1<-model[[1]]@occs[3:length(model[[1]]@occs)]
occs.z2<-model[[2]]@occs[3:length(model[[2]]@occs)]
bgPts.z1<-model[[1]]@bg[3:length(model[[1]]@bg)]
bgPts.z2<-model[[2]]@bg[3:length(model[[2]]@bg)]
###Generate pca for further analyses
Testpca<-espace_pca(sp.name1,sp.name2,occs.z1,occs.z2,bgPts.z1,bgPts.z2)
###RUN FUNCTION
TestOccDens<-espace_occDens(sp.name1, sp.name2,Testpca)

test_that("output checks", {
  #output is a list
  expect_equal(mode(TestOccDens),"list")
  #list is of objects of type list
  expect_is(TestOccDens,"list")
  #list has 2 objects (2 species)
  expect_equal(length(TestOccDens),2)
  #The name of each list is the name of the species
  expect_equal(names(TestOccDens),c(sp.name1,sp.name2))
  #each list contains 10 objects
  expect_equal(length(TestOccDens[[sp.name1]]),10)
  expect_equal(length(TestOccDens[[sp.name2]]),10)
  ##The name of the slots of each list is correct
  expect_equal(names(TestOccDens[[sp.name1]]),c("x","y","z","z.uncor","z.cor","Z","glob","glob1","sp","w"))
  expect_equal(names(TestOccDens[[sp.name2]]),c("x","y","z","z.uncor","z.cor","Z","glob","glob1","sp","w"))
   ##Test that all outputs but x and y and inputs (including occupancy, density and weights) are all raster layers
  #sp1
  expect_is(TestOccDens[[sp.name1]]$z,'RasterLayer')
  expect_is(TestOccDens[[sp.name1]]$Z,'RasterLayer')
  expect_is(TestOccDens[[sp.name1]]$z.uncor,'RasterLayer')
  expect_is(TestOccDens[[sp.name1]]$z.cor,'RasterLayer')
  expect_is(TestOccDens[[sp.name1]]$w,'RasterLayer')
  #sp2
  expect_is(TestOccDens[[sp.name2]]$z,'RasterLayer')
  expect_is(TestOccDens[[sp.name2]]$Z,'RasterLayer')
  expect_is(TestOccDens[[sp.name2]]$z.uncor,'RasterLayer')
  expect_is(TestOccDens[[sp.name2]]$z.cor,'RasterLayer')
  expect_is(TestOccDens[[sp.name2]]$w,'RasterLayer')

  })




