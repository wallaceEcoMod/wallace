context("component_1_Occ_Data")

# Load the package
library(RSelenium)
library(testthat)
# test_dir('/Users/musasabi/Documents/github/wallace/test', filter = 'mod1', reporter = "Tap")

# Connect to the app (open another rstudio and run_wallace())
remDr <- remoteDriver() 
remDr$open(silent = TRUE)
appURL <- "http://127.0.0.1:5556"

# move to Component 1
remDr$navigate(appURL)
# necessary for waiting for db query to load
remDr$setImplicitWaitTimeout(milliseconds = 100000)
compTabs <- remDr$findElements("css selector", ".nav a")
compTabLabels <- sapply(compTabs, function(x){x$getElementText()})
comp1Tab <- compTabs[[which(compTabLabels == "1 Occ Data")]]  
# appCtrlLabels <- unlist(strsplit(appCtrlLabels[[1]], "\n"))
comp1Tab$clickElement()

# Here if the app contains the correct tabs and their respective names.
test_that("Component 1 Module Query Database: Buttons", {  
  # click gbif button
  field.gbif <- comp1Tab$findChildElement(value = "//input[@type='radio' and @value='gbif']")
  initState <- field.gbif$isElementSelected()[[1]]
  field.gbif$clickElement()
  changeState <- field.gbif$isElementSelected()[[1]]
  expect_is(initState, "logical")  
  expect_is(changeState, "logical")  
  expect_true(initState == changeState)  
  
  # click Vertnet button
  field.vnet <- comp1Tab$findChildElement(value = "//input[@type='radio' and @value='vertnet']")
  initState <- field.vnet$isElementSelected()[[1]]
  field.vnet$clickElement()
  changeState <- field.vnet$isElementSelected()[[1]]
  expect_is(initState, "logical")  
  expect_is(changeState, "logical")  
  expect_false(initState == changeState)  
  
  #click BISON button (isn't this database defunded?)
  field.bison <- comp1Tab$findChildElement(value = "//input[@type='radio' and @value='bison']")
  initState <- field.bison$isElementSelected()[[1]]
  field.bison$clickElement()
  changeState <- field.bison$isElementSelected()[[1]]
  expect_is(initState, "logical")  
  expect_is(changeState, "logical")  
  expect_false(initState == changeState)  
  
  # switch back to gbif
  field.gbif$clickElement()
})

test_that("Component 1 Module Query Database: Slider", {
  slider <- comp1Tab$findChildElement(value = "//input[@id='c1_queryDb-occNum']")
  sliderDim <- slider$getElementSize()
  expect_equal(sliderDim$width, 4)
  expect_equal(sliderDim$height, 4)
})

test_that("Component 1 Module User-specified: Buttons", {
  #click User points button 
  field <- comp1Tab$findChildElement(value = "//input[@type='radio' and @value='user']")
  initState <- field$isElementSelected()[[1]]
  field$clickElement()
  
  field <- comp1Tab$findChildElement(value = "//input[@id='c1_userOccs-userCSV']")
  initState <- field$isElementSelected()[[1]]
  field$clickElement()
  changeState <- field$isElementSelected()[[1]]
  expect_is(initState, "logical")
  expect_is(changeState, "logical")
  # don't think the states should be different, so maybe not necessary here
  # expect_false(initState == changeState)
  
  field <- comp1Tab$findChildElement(value = "//button[@type='button' and @id='goUserOccs']")
  initState <- field$isElementSelected()[[1]]
  field$clickElement()
  changeState <- field$isElementSelected()[[1]]
  expect_is(initState, "logical")
  expect_is(changeState, "logical")
})

# Close the connection
remDr$close()
