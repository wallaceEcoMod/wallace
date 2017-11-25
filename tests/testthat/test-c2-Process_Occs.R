# Load the package
library(RSelenium)
library(testthat)

context("component_2_Proccess_Occs")
# test_dir('/Users/musasabi/Documents/github/wallace/test', filter = 'c2', reporter = "Tap")

# Connect to the app (open another rstudio and run_wallace())
remDr <- remoteDriver() # use the right address by running code in test/run_wallace.r
remDr$open(silent = TRUE)
appURL <- "http://127.0.0.1:5556"

# move to Component 2
remDr$navigate(appURL)
compTabs <- remDr$findElements("css selector", ".nav a")
compTabLabels <- sapply(compTabs, function(x) x$getElementText())
comp2Tab <- compTabs[[which(compTabLabels == "2 Process Occs")]]  
comp2Tab$clickElement()

test_that("Component 2: Radio Buttons", {  
  # click Remove by ID radio button
  field.remID <- comp2Tab$findChildElement(value = "//input[@type='radio' and @value='remID']")
  initState <- field.remID$isElementSelected()[[1]]
  field.remID$clickElement()
  changeState <- field.remID$isElementSelected()[[1]]
  expect_is(initState, "logical")
  expect_is(changeState, "logical")
  expect_false(initState == changeState)
  
  # click Spatial Thin radio button
  field.spthin <- comp2Tab$findChildElement(value = "//input[@type='radio' and @value='spthin']")
  initState <- field.spthin$isElementSelected()[[1]]
  field.spthin$clickElement()
  changeState <- field.spthin$isElementSelected()[[1]]
  expect_is(initState, "logical")
  expect_is(changeState, "logical")
  expect_false(initState == changeState)
  
  # click Select Occs radio button
  field.selOccs <- comp2Tab$findChildElement(value = "//input[@type='radio' and @value='selOccs']")
  initState <- field.selOccs$isElementSelected()[[1]]
  field.selOccs$clickElement()
  changeState <- field.selOccs$isElementSelected()[[1]]
  expect_is(initState, "logical")
  expect_is(changeState, "logical")
  expect_false(initState == changeState)
})

test_that("Component 2 Module Select Occurrences on Map: Buttons", {  
  button <- remDr$findElement('id', "goSelectOccs")
  expect_true(button$isElementDisplayed()[[1]])
  
  button <- remDr$findElement('id', "goResetOccs")
  expect_true(button$isElementDisplayed()[[1]])
  
  button <- remDr$findElement('id', "dlProcOccs")
  expect_true(button$isElementDisplayed()[[1]])
})

test_that("Component 2 Remove Occurrences by ID: Buttons", {
  # switch to remID
  field.remID <- comp2Tab$findChildElement(value = "//input[@type='radio' and @value='remID']")
  field.remID$clickElement()
  button <- remDr$findElement('id', "goRemoveByID")
  expect_true(button$isElementDisplayed()[[1]])
})

test_that("Component 2 Remove Occurrences by ID: Numeric Input", {
  field <- remDr$findElement('id', "c2_removeByID-removeID")
  field$clickElement()
  field$clearElement()
  field$sendKeysToElement(list("10"))
  val <- field$getElementAttribute('value')[[1]]
  expect_equal(val, "10")
})
  
test_that("Component 2 Spatial Thin: Buttons", {
  # switch to spthin
  field.spthin <- comp2Tab$findChildElement(value = "//input[@type='radio' and @value='spthin']")
  field.spthin$clickElement()
  button <- remDr$findElement('id', "goThinOccs")
  expect_true(button$isElementDisplayed()[[1]])
})

test_that("Component 2 Spatial Thin: Numeric Input", {
  field <- remDr$findElement('id', "c2_thinOccs-thinDist")
  field$clickElement()
  field$clearElement()
  field$sendKeysToElement(list("10"))
  val <- field$getElementAttribute('value')[[1]]
  expect_equal(val, "10")
})

# Close the connection
remDr$close()
