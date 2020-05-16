#### COMPONENT espace: Do analysis in environmental space.
#### MODULE: espace
context("espace_pca")

source("test_helper_functions.R")

##Function has no warning or error messages.
##To run this function a model must be created first allowing for access to environmental data for occs and background
###Using bioclim for testing

###SET PARAMETERS (running model)
  sp.name1<-"Panthera onca"

## occurrences
occs <-  occs_queryDb(spName = sp.name1, occDb = "gbif", occNum = 100)
occs <- as.data.frame(occs[[1]]$cleaned)
## process data
occs <- poccs_thinOccs(occs = occs, thinDist = 10,spN=occs)
# enviromental data
envs <- envs_worldclim(bcRes = 10, bcSel = list(TRUE,TRUE,TRUE,TRUE,TRUE), doBrick = FALSE)
# background extent
bgExt <- penvs_bgExtent(occs, bgSel = 'bounding box', bgBuf = 0.5,spN=occs)
# background masked
bgMask <- penvs_bgMask(occs, envs, bgExt,spN=occs)
## background sample
bg <- penvs_bgSample(occs, bgMask, bgPtsNum = 1000,spN=occs)
## Partition
partblock <- part_partitionOccs(occs, bg, method = 'block', kfolds = NULL, bgMask = NULL,
                                aggFact = NULL,spN=occs)
### Create model
bioclimAlg <- model_bioclim(occs, bg, partblock$occ.grp, partblock$bg.grp, bgMask,spN=occs)


##DELETE at the ends
##espace_pca<- function(sp.name1, sp.name2 = NULL, occs.z1, occs.z2 = NULL,
  ##                    bgPts.z1, bgPts.z2 = NULL, logger = NULL)

###RUN FUNCTION

  Testpca<-espace_pca(sp.name1,occs.z1=bioclimAlg@occs,bgPts.z1=bioclimAlg@bg)

  test_that("output checks", {
    #output is a list
    expect_is(Testpca,list)
    #list has 14 objects
    expect_equal(length(Testpca),14)
    #objects in list are as expected
    expect_equal(c('tab', 'cw','lw','eig','rank','nf','c1','li','co','l1','call','cent','norm','scores'), names(Testpca))
  })



