#### COMPONENT 7: Visualize Model Results
#### MODULE: BIOCLIM Envelope Plots
context("bioclimPlot")

source("test_helper_functions.R")


### Set parameters

## get records
out.gbif <- c1_queryDb(spName = "panthera onca", occDb = "gbif", occNum = 100)
occs <- as.data.frame(out.gbif$Panthera_onca$cleaned)

## background
# enviromental data
envs <- c3_worldclim(bcRes = 10, bcSel = list(TRUE,TRUE,TRUE,TRUE,TRUE), doBrick = FALSE)
# background extent 
bgExt <- c4_bgExtent(occs, bgSel = 'bounding box', bgBuf = 0.5) 
# background masked 
bgMask <- c4_bgMask(occs, envs, bgExt)
## background sample
bg <- c4_bgSample(occs, bgMask, bgPtsNum = 10000) 

## Partition 
partblock <- c5_partitionOccs(occs, bg, method = 'block', kfolds = NULL, bgMask = NULL,
                              aggFact = NULL)
# occurrences partitioned
occs$partition
occs$partition <- partblock$occ.grp
# background points partitioned
bg$partition
bg$partition <- partblock$bg.grp

## model
bioclimAlg <- runBIOCLIM(occs, bg, bgMask) 


### run function 
bioclimPlot <- recordPlot(makeBioclimPlot(x = bioclimAlg$models$BIOCLIM, a=1, b=2, p=1))


## test if the error messages appear when they are supposed to 
test_that("error checks", {
  # the user specified a variable that wasn't included within the model
  expect_error(makeBioclimPlot(x = bioclimAlg$models$BIOCLIM, a=(raster::nlayers(envs))+1, 
                               b=2, p=1))
  expect_error(makeBioclimPlot(x = bioclimAlg$models$BIOCLIM, a= 1, 
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
