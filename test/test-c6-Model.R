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
compTabs <- remDr$findElements("css selector", ".nav a")
compTabLabels <- sapply(compTabs, function(x){x$getElementText()})

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



# Close the connection
remDr$close()
