##### QUESTIONS
  # 1. problems with the warning message (warning message appear in R but not in the log window in Wallace)


#### COMPONENT 3: Obtain Environmental Data
#### MODULE: User-specified
context("userEnvs")

source("test_helper_functions.R")


### Set parameters

## path to files
Path <- list.files(path='./wc10/', pattern = "*.bil$", full.names = TRUE)
## files name
Name <- list.files(path='./wc10/', pattern = "*.bil$", full.names = FALSE)


### run function
userEnvs <- envs_userEnvs(rasPath = Path, rasName = Name)


### test if the warning messages appear when they are supposed to
test_that("warning checks", {
  # input rasters have undefined coordinate reference system (CRS)
  expect_warning(envs_userEnvs(rasPath = './wc10/bio_No_Projection.tif',
                             rasName = 'no_Projection'),'Input rasters have undefined coordinate reference system (CRS). Mapping functionality in components Visualize Model Results and Project Model will not work. If you wish to map rasters in these components, please define their projections and upload again. See guidance text in this module for more details.',fixed=T)
  })

### test output features
test_that("output type checks", {
  # the output is a RasterStack
  expect_is(userEnvs, "RasterStack")
  # the number of layer is the same as loaded by the user
  expect_equal(length(Name), raster::nlayers(userEnvs))
  })
