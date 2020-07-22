#### COMPONENT espace: Do analysis in environmental space.
#### MODULE: Principal component analysis
context("espace_pca")

source("test_helper_functions.R")

##Function has no warning or error messages.
##To run this function a model must be created first allowing for access to environmental data for occs and background
###Using bioclim for testing

###SET PARAMETERS (running model)
  sp.name1<-"Panthera onca"
  sp.name2<-"Procyon lotor"
  species<-c(sp.name1,sp.name2)
  model<-list()
  for (i in 1:2){

## occurrences
occs <-  occs_queryDb(spName = species[i], occDb = "gbif", occNum = 100)
occs <- as.data.frame(occs[[1]]$cleaned)
## process data
occs <- poccs_thinOccs(occs = occs, thinDist = 10,spN=species[i])
# enviromental data
envs <- envs_worldclim(bcRes = 10, bcSel = c("bio01","bio02","bio13","bio14"), doBrick = FALSE)
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
bioclimAlg <- model_bioclim(occs, bg, partblock$occ.grp, partblock$bg.grp, bgMask,spN=species[i])

model[i]<-bioclimAlg
  }
  ##Set parameters
      ##Remove coordinates (lat/long from tables)
  occs.z1<-model[[1]]@occs[3:length(model[[1]]@occs)]
  occs.z2<-model[[2]]@occs[3:length(model[[2]]@occs)]
  bgPts.z1<-model[[1]]@bg[3:length(model[[1]]@bg)]
  bgPts.z2<-model[[2]]@bg[3:length(model[[2]]@bg)]
###RUN FUNCTION

  Testpca<-espace_pca(sp.name1,sp.name2,occs.z1,occs.z2,bgPts.z1,bgPts.z2)

  test_that("output checks", {
    #output is a list
    expect_equal(mode(Testpca),"list")
    #list is of objects of typoe "pca" and "dudi"
    expect_is(Testpca,"pca")
    expect_is(Testpca,"dudi")
    #list has 14 objects
    expect_equal(length(Testpca),14)
    #objects in list are as expected
    expect_equal(c('tab', 'cw','lw','eig','rank','nf','c1','li','co','l1','call','cent','norm','scores'), names(Testpca))
  })



