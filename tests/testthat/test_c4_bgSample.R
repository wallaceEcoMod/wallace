#### COMPONENT 4: Process Environmental Data
#### MODULE: Select Study Region 
context("bgSample - Step 2.2")

source("test_helper_functions.R")


### get data

## occurrence
occs <-  c1_queryDb(spName = "panthera onca", occDb = "gbif", 
                    occNum = 100)
occs <- as.data.frame(occs$cleaned)

## enviromental data
envs <- c3_worldclim(bcRes = 10, bcSel = (list(TRUE,TRUE,TRUE,TRUE,TRUE)))

## background extent 
bgExt <- c4_bgExtent(occs, envs, bgSel = 'bb', bgBuf = 0.5) 

## background masked 
bgMask <- c4_bgMask(occs, envs, bgExt)


### run function 
bgsample <- c4_bgSample(occs, bgMask, bgPtsNum = 1000) 


### test output features
test_that("output type checks", {
  # the output is a data frame
  expect_is(bgsample, "data.frame")
  # both latitude and longitude were sampled
  expect_equal(ncol(bgsample), 2)
  # the number of background pints sampled are the same as specified in the function
  expect_equal(nrow(bgsample), 1000)
})
