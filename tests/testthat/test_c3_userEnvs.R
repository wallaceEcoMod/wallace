##### QUESTIONS
  # 1. problems with the warning message
  # 2. warning message appear in R but not in Wallace 


#### COMPONENT 3: Obtain Environmental Data
#### MODULE: User-specified
context("userEnvs")

source("test_helper_functions.R")


### get data 
## path
resPath <- list.files(path='./wc10/', pattern = "*.bil", full.names = TRUE)
## names 
rasName <- list.files(path='./wc10/', pattern = "*.bil", full.names = FALSE)

### run function
userEnvs <- c3_userEnvs(rasPath = resPath, rasName = rasName)


### test if the warning messages appear when they are supposed to 
test_that("warning checks", {
  # Input rasters have undefined coordinate reference system (CRS)  
  expect_warning(c3_userEnvs(rasPath = './wc10/No_projection/no_projection.tif',
                             rasName = 'no_Projection'), 'Input rasters have undefined coordinate reference system (CRS). Mapping functionalityin components Visualize Model Results and Project Model will not work. If you wish to map rasters in these components, please define their projections and upload again. See guidance text in this module for more details.')
})

### test output features
test_that("output type checks", {
  # the output is a RasterStack
  expect_is(userEnvs, "RasterStack")
  # the number of layer is the same as loaded by the user 
  expect_equal(length(rasName), raster::nlayers(userEnvs))
})
