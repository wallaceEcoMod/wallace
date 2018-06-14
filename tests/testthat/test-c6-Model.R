# Load the package
library(RSelenium)
library(testthat)

context("component_6_Model")
# test_dir('/Users/musasabi/Documents/github/wallace/test', filter = 'c6', reporter = "Tap")

# Connect to the app (open another rstudio and run_wallace())
remDr <- remoteDriver() 
remDr$open(silent = TRUE)
appURL <- "http://127.0.0.1:5556"

remDr$navigate(appURL)
compTabs <- remDr$findElements("css selector", ".nav a")
compTabLabels <- sapply(compTabs, function(x) x$getElementText())
# Move to Component 3
comp6Tab <- compTabs[[which(compTabLabels == "6 Model")]]    
comp6Tab$clickElement()

field.maxent <- comp6Tab$findChildElement(value = "//input[@type='radio' and @value='Maxent']")
field.bioclim <- comp6Tab$findChildElement(value = "//input[@type='radio' and @value='BIOCLIM']")

test_that("Component 6: Radio Buttons", {
  clickButton(field.maxent)
  clickButton(field.bioclim)
})

test_that("Comp 6 Module BIOCLIM: Buttons", {
  button <- remDr$findElement('id', "goBioclim")
  expect_true(button$isElementDisplayed()[[1]])
})

test_that("Comp 6 Module Maxent: Checkboxes", {
  # click Maxent
  field.maxent$clickElement()
 
  # Select features
  linear <- comp6Tab$findChildElement(value = "//input[@name='c6_maxent-fcs' and @value='L']")
  expect_false(linear$isElementSelected()[[1]])
  linear$clickElement()
  expect_true(linear$isElementSelected()[[1]])
  quad <- comp6Tab$findChildElement(value = "//input[@name='c6_maxent-fcs' and @value='LQ']")
  expect_false(quad$isElementSelected()[[1]])
  quad$clickElement()
  expect_true( quad$isElementSelected()[[1]])
  hinge <- comp6Tab$findChildElement(value = "//input[@name='c6_maxent-fcs' and @value='H']")
  expect_false(hinge$isElementSelected()[[1]])
  hinge$clickElement()
  expect_true(hinge$isElementSelected()[[1]])
  lqh <- comp6Tab$findChildElement(value = "//input[@name='c6_maxent-fcs' and @value='LQH']")
  expect_false(lqh$isElementSelected()[[1]])
  lqh$clickElement()
  expect_true(lqh$isElementSelected()[[1]])
  lqhp <- comp6Tab$findChildElement(value = "//input[@name='c6_maxent-fcs' and @value='LQHP']")
  expect_false(lqhp$isElementSelected()[[1]])
  lqhp$clickElement()
  expect_true(lqhp$isElementSelected()[[1]])
  lqhpt <- comp6Tab$findChildElement(value = "//input[@name='c6_maxent-fcs' and @value='LQHPT']")
  expect_false(lqhpt$isElementSelected()[[1]])
  lqhpt$clickElement()
  expect_true(lqhpt$isElementSelected()[[1]])
})

test_that("Component 6 Module Maxent: Slider", {
  slider6 <- comp6Tab$findChildElement(value = "//input[@id='c6_maxent-rms']")
  sliderDim <- slider6$getElementSize()
  expect_equal(sliderDim$width, 4)
  expect_equal(sliderDim$height, 4)
})

test_that("Component 6 Module Maxent: Slider", {
  rmsField=comp6Tab$findChildElement(value = "//input[@id='c6_maxent-rmsStep']")
  rmsField$clickElement()
  rmsField$clearElement()
  rmsField$sendKeysToElement(list('10'))
  expect_equal(rmsField$getElementAttribute("value")[[1]], "10")
  rmsField$clearElement()
  rmsField$sendKeysToElement(list('1'))
  expect_equal(rmsField$getElementAttribute("value")[[1]], "1")
})

test_that("Comp 6 Module Maxent: Buttons", {
  button <- remDr$findElement('id', "goMaxent")
  expect_true(button$isElementDisplayed()[[1]])
})

# Close the connection
remDr$close()
