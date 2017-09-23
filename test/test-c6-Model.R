context("component_6_Model")

# Load the package
library(RSelenium)
library(testthat)

# Connect to the app (open another rstudio and run_wallace())
remDr <- remoteDriver() # use the right address by running code in test/run_wallace.r
remDr$open(silent = TRUE)
appURL <- "http://127.0.0.1:5556" # use the right address by running code in test/run_wallace.r

### NOTE: up to moving to component 6, this is redundant with the tests in comp 5, so you may want to consolidate

# move to Component 1 (to download the data to partition)
remDr$navigate(appURL)
# necessary for waiting for db query to load
remDr$setImplicitWaitTimeout(milliseconds = 100000)
compTabs <- remDr$findElements("css selector", ".nav a")
compTabLabels <- sapply(compTabs, function(x){x$getElementText()})
comp1Tab <- compTabs[[which(compTabLabels == "1 Occ Data")]]  
# appCtrlLabels <- unlist(strsplit(appCtrlLabels[[1]], "\n"))
comp1Tab$clickElement()

# select data (Comp1)
field.gbif <- comp1Tab$findChildElement(value = "//input[@type='radio' and @value='gbif']")
field.gbif$clickElement()
speciesField<- comp1Tab$findChildElement(value = "//input[@id='c1_queryDb-spName']")
speciesField$clickElement()
speciesField$sendKeysToElement(list('Acer rubrum'))
queryButton= comp1Tab$findChildElement(value = "//button[@type='button' and @id='goDbOccs']")
queryButton$clickElement()
remDr$setImplicitWaitTimeout(milliseconds = 100000)

# get env data (Comp3)
comp3Tab <- compTabs[[which(compTabLabels == "3 Env Data")]]  
comp3Tab$clickElement()
# select resolution of 10
drop.menu <- comp3Tab$findChildElement(value = "//div[@class='selectize-control shinyjs-resettable single']")
drop.menu$clickElement()
res="'10'"
select.res.10 <- comp3Tab$findChildElement(value = paste0("//div[@data-value=",
                                                          res, "and @class='option']"))
select.res.10$clickElement()
#selected.item <- comp3Tab$findChildElement(value = paste0("//div[@class='item' and @data-value=", res,  "]"))
# select just a few predictors for speed
check.spec <- comp3Tab$findChildElement(value = "//input[@id='c3_wcBioclims-bcSelChoice']")
check.spec$clickElement()
for (bio in c(4:19)) {
  check.vars <- check.spec$findChildElement(
    value = paste0("//input[@name='c3_wcBioclims-bcSels' and @value='bio", bio, "']"))
  check.vars$clickElement()
}
# load env data
loadEnvButton= comp1Tab$findChildElement(value = "//button[@type='button' and @id='goEnvData']")
loadEnvButton$clickElement()
# takes a moment to download the first time
remDr$setImplicitWaitTimeout(milliseconds = 100000)

# choose domain (Comp4)
comp4Tab <- compTabs[[which(compTabLabels == "4 Process Envs")]]  
comp4Tab$clickElement()
extentButton= comp1Tab$findChildElement(value = "//button[@type='button' and @id='goBgExt']")
extentButton$clickElement()
backgroundButton= comp1Tab$findChildElement(value = "//button[@type='button' and @id='goBgMask']")
backgroundButton$clickElement()

# move to Comp 5, partition Occ
comp5Tab <- compTabs[[which(compTabLabels == "5 Partition Occs")]]  
comp5Tab$clickElement()
nFoldsField=comp1Tab$findChildElement(value = "//input[@id='c5_partNsp-kfolds']")
nFoldsField$clickElement()
nFoldsField$clearElement()
nFoldsField$sendKeysToElement(list('2'))
partitionButton= comp1Tab$findChildElement(value = "//button[@type='button' and @id='goPartNsp']")
partitionButton$clickElement()

# move to Comp 6
comp6Tab <- compTabs[[which(compTabLabels == "6 Model")]]  
comp6Tab$clickElement()

test_that("Comp 6 Buttons Click", {
  # click Maxent
  field <- comp6Tab$findChildElement(value = "//input[@type='radio' and @value='Maxent']")
  initState <- field$isElementSelected()[[1]]
  field$clickElement()
  changeState <- field$isElementSelected()[[1]]
  expect_is(initState, "logical")
  expect_is(changeState, "logical")
  expect_false(initState == changeState)

  # click bioclim
  field <- comp6Tab$findChildElement(value = "//input[@type='radio' and @value='BIOCLIM']")
  initState <- field$isElementSelected()[[1]]
  field$clickElement()
  changeState <- field$isElementSelected()[[1]]
  expect_is(initState, "logical")
  expect_is(changeState, "logical")
  expect_false(initState == changeState)

})

test_that("Comp 6 Maxent", {
  # click Maxent
  field <- comp6Tab$findChildElement(value = "//input[@type='radio' and @value='Maxent']")
  field$clickElement()
 
  # Select features
  linear <- comp6Tab$findChildElement(value = "//input[@name='c6_maxent-fcs' and @value='L']")
  expect_false(linear$isElementSelected()[[1]])
  linear$clickElement()
  expect_true( linear$isElementSelected()[[1]])
  quad <- comp6Tab$findChildElement(value = "//input[@name='c6_maxent-fcs' and @value='LQ']")
  expect_false(quad$isElementSelected()[[1]])
  quad$clickElement()
  expect_true( quad$isElementSelected()[[1]])
  hinge <- comp6Tab$findChildElement(value = "//input[@name='c6_maxent-fcs' and @value='H']")
  expect_false(hinge$isElementSelected()[[1]])
  hinge$clickElement()
  expect_true( hinge$isElementSelected()[[1]])
  lqh <- comp6Tab$findChildElement(value = "//input[@name='c6_maxent-fcs' and @value='LQH']")
  expect_false(lqh$isElementSelected()[[1]])
  lqh$clickElement()
  expect_true( lqh$isElementSelected()[[1]])
  lqhp <- comp6Tab$findChildElement(value = "//input[@name='c6_maxent-fcs' and @value='LQHP']")
  expect_false(lqhp$isElementSelected()[[1]])
  lqhp$clickElement()
  expect_true( lqhp$isElementSelected()[[1]])
  lqhpt <- comp6Tab$findChildElement(value = "//input[@name='c6_maxent-fcs' and @value='LQHPT']")
  expect_false(lqhpt$isElementSelected()[[1]])
  lqhpt$clickElement()
  expect_true( lqhpt$isElementSelected()[[1]])

})

test_that("Comp 6 Regularization Slider", {
  slider6 <- comp6Tab$findChildElement(value = "//input[@id='c6_maxent-rms']")
  sliderDim <- slider6$getElementSize()
  expect_equal(sliderDim$width, 4)
  expect_equal(sliderDim$height, 4)
  # would be good to try changing the values
})

test_that("Comp 6 Regularization Multiplier Step", {
  rmsField=comp6Tab$findChildElement(value = "//input[@id='c6_maxent-rmsStep']")
  rmsField$clickElement()
  rmsField$clearElement()
  rmsField$sendKeysToElement(list('10'))
  expect_equal(rmsField$getElementAttribute("value")[[1]], "10")
  rmsField$clearElement()
  rmsField$sendKeysToElement(list('1'))
  expect_equal(rmsField$getElementAttribute("value")[[1]], "1")
})

test_that("Comp 6 Run Maxent", {
  # just run a simple model for speed
  hinge$clickElement()
  lqh$clickElement()
  lqhp$clickElement()
  lqhpt$clickElement()
  
  maxentButton= comp6Tab$findChildElement(value = "//button[@type='button' and @id='goMaxent']")
  maxentButton$clickElement()
  remDr$setImplicitWaitTimeout(milliseconds = 100000)
  logBoxText <- comp6Tab$findChildElement(value = "//div[@id='logContent']")
  # not sure how to grab the text to be sure that the value is 
  # could try grabbing the Results Table instead
  #resultsTable=comp6Tab$findChildElement(value = "//div[@href='#tab-4286-3']")
  
  
})

# Close the connection
remDr$close()
