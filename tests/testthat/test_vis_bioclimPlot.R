#### COMPONENT vis: Visualize Model Results
#### MODULE: BIOCLIM Envelope Plots
context("bioclimPlot")

### Set parameters

## get records
spN<-"Panthera onca"
out.gbif <- occs_queryDb(spName = spN, occDb = "gbif", occNum = 100)
occs <- as.data.frame(out.gbif[[1]]$cleaned)

## background
# enviromental data
envs <- envs_worldclim(bcRes = 10, bcSel = c("bio01","bio02","bio13","bio14"), doBrick = FALSE)
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
bioclimAlg <- model_bioclim(occs, bg, partblock, bgMask,spN=spN)


### run function
bioclimPlot <- vis_bioclimPlot(x = bioclimAlg@models$bioclim, a=2, b=3, p=0.7)
bioclimPlot<-recordPlot(bioclimPlot)


## test if the error messages appear when they are supposed to
test_that("error checks", {
  # the user specified a variable that wasn't included within the model
  expect_error(vis_bioclimPlot(x = bioclimAlg@models$bioclim, a=(raster::nlayers(envs))+1,
                               b=2, p=1))
  expect_error(vis_bioclimPlot(x = bioclimAlg@models$bioclim, a= 1,
                               b=(raster::nlayers(envs))+1, p=1))
})

### test output features
test_that("output checks", {
  # the output is a recordedplot
  expect_is(bioclimPlot, "recordedplot")
  # the list has three elements
  expect_equal(length(bioclimPlot), 3)
  # the three elements are lists
  expect_is((bioclimPlot[c(1,2,3)]), "list")
})
