#### COMPONENT mask: Mask Prediction
#### MODULE: Temporal
context("tempAnnotate")

### Set parameters
occs <- read.csv(system.file("extdata/Bassaricyon_alleni.csv",
                             package = "wallace"))
# assigning dummy dates, but ideally the occ data would have years
#BAJ 10/4/2024: replace Bassaricyon_alleni.csv with one that includes dates & remove this
dates <- sample(2000:2019,35, replace=T)
occs$year <- dates

env <- list.files(path = system.file('extdata/MODIS', package = "wallace"),
                  full.names = TRUE)
env <- raster::stack(env)

envDates <- c(2005,2006,2008,2009,2010)
### Run function
bounds <- mask_tempAnnotate(occs, env, envDates, logger = NULL)

################ Tests ####################################

### test if the error messages appear when they are supposed to
# No error messages

### test if the warning messages appear when they are supposed to
# No warning messages

### test output features
test_that("output checks", {
  # numeric
  expect_is(bounds, "numeric")
  # quantiled are correctly set
  expect_equal(names(bounds), c("0%", "2.5%", "25%", "50%", "75%", "97.5%", "100%"))
})

### test function steps
