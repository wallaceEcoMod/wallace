context("module_1_Occ_Data")

# Load the package
library(RSelenium)
library(testthat)
# test_dir('/Users/musasabi/Documents/github/wallace/test', filter = 'mod1', reporter = "Tap")

# Connect to the app (open another rstudio and run_wallace())
# NOTE: use the right address by running code in test/run_wallace.r
remDr <- remoteDriver() 
remDr$open(silent = TRUE)
appURL <- "http://127.0.0.1:5556"


# move to Component 1
remDr$navigate(appURL)
# necessary for waiting for db query to load
remDr$setImplicitWaitTimeout(milliseconds = 10000)
compTabs <- remDr$findElements("css selector", ".nav a")
compTabLabels <- sapply(compTabs, function(x){x$getElementText()})
comp1Tab <- compTabs[[which(compTabLabels == "1 Occ Data")]]  
# appCtrlLabels <- unlist(strsplit(appCtrlLabels[[1]], "\n"))
comp1Tab$clickElement()

resultsTabs <- remDr$findElements("css selector", ".nav a")
resultsTabLabels <- sapply(resultsTabs, function(x){x$getElementText()})
occsTblTab <- resultsTabs[[which(resultsTabLabels == "Occs Tbl")]]  


# Here if the app contains the correct tabs and their respective names.
test_that("Component 1 Module Query Database: Buttons", {  
  # click gbif button
  field <- comp1Tab$findChildElement(value = "//input[@type='radio' and @value='gbif']")
  initState <- field$isElementSelected()[[1]]
  field$clickElement()
  changeState <- field$isElementSelected()[[1]]
  expect_is(initState, "logical")  
  expect_is(changeState, "logical")  
  expect_true(initState == changeState)  
  
  # click Vertnet button
  field <- comp1Tab$findChildElement(value = "//input[@type='radio' and @value='vertnet']")
  initState <- field$isElementSelected()[[1]]
  field$clickElement()
  changeState <- field$isElementSelected()[[1]]
  expect_is(initState, "logical")  
  expect_is(changeState, "logical")  
  expect_false(initState == changeState)  
  
  #click BISON button (isn't this database defunded?)
  field <- comp1Tab$findChildElement(value = "//input[@type='radio' and @value='bison']")
  initState <- field$isElementSelected()[[1]]
  field$clickElement()
  changeState <- field$isElementSelected()[[1]]
  expect_is(initState, "logical")  
  expect_is(changeState, "logical")  
  expect_false(initState == changeState)  
})

test_that("Component 1 Module Query Database: Slider", {
  slider <- comp1Tab$findChildElement(value = "//input[@id='c1_queryDb-occNum']")
  sliderDim <- slider$getElementSize()
  expect_equal(sliderDim$width, 4)
  expect_equal(sliderDim$height, 4)
  # this is not working for some reason, probably bc of link below
  # https://stackoverflow.com/questions/29065334/rselenium-not-able-to-run-example-code
  # sliderButton <- comp1Tab$findChildElement(value = "//span[@class='irs-slider single']")
  # remDr$mouseMoveToLocation(webElement = sliderButton)
})

test_that("Component 1 Module Query Database: Check DB Query", {
  # find textInput field and click
  field <- comp1Tab$findChildElement(value = "//input[@type='text']")
  field$clickElement()
  # write text
  field$sendKeysToElement(list("Puma concolor"))
  # find button and click
  button <- comp1Tab$findChildElement(value = "//button[@id='goDbOccs']")
  button$clickElement()
  # click occsTbl tab
  occsTblTab$clickElement()
  # find data table
  # NOTE: finding elements (plural) does not error when none exist
  occsTbl <- occsTblTab$findChildElements(value = '//table')
  # check if the table's there
  expect_true(occsTbl[[1]]$isElementDisplayed()[[1]])
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
