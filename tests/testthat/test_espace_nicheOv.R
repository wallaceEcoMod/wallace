#### COMPONENT espace: Do analysis in environmental space.
#### MODULE: espace
context("espace_nicheOv")

source("test_helper_functions.R")

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
 # occs <- poccs_thinOccs(occs = occs, thinDist = 10,spN=species[i]) ##Removed because warning from spthin on different names
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
###Generate ecospat occDens objects
TestOccDens<-espace_occDens(sp.name1, sp.name2,Testpca)
z1=TestOccDens[[sp.name1]]
z2=TestOccDens[[sp.name2]]
###RUN FUNCTION
iter=100
TestNicheOv<-espace_nicheOv(z1, z2, iter , equivalency = TRUE, similarity = TRUE, logger = NULL)


  ### test output features
  test_that("output checks", {
  ##Output is a list
  expect_is(TestNicheOv,"list")
  ##all entries are there
  expect_equal(length(TestNicheOv),4)
  expect_equal(names(TestNicheOv),c("overlap","USE","equiv","simil"))
  ###output type and content is correct
  expect_is(TestNicheOv$overlap,'list')
  expect_is(TestNicheOv$overlap$D,'numeric')
  expect_is(TestNicheOv$overlap$I,'numeric')

  expect_is(TestNicheOv$USE,'numeric')
  expect_equal(length(TestNicheOv$USE),3)

   expect_is(TestNicheOv$equiv,'list')
   expect_equal(length(TestNicheOv$equiv),4)

   expect_is(TestNicheOv$simil,'list')
   expect_equal(length(TestNicheOv$simil),4)
   ###number of iterations for tests matches
   expect_equal(length(TestNicheOv$equiv$sim$D),iter)
   expect_equal(length(TestNicheOv$simil$sim$D),iter)
  })

  ##Function has no warning or error messages
