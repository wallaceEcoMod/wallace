context("module_6_Model")

# Load the package
library(RSelenium)
library(testthat)

# Connect to the app (open another rstudio and run_wallace())
remDr <- remoteDriver(port=5556) # use the right address by running code in test/run_wallace.r
remDr$open(silent = TRUE)
appURL <- "http://127.0.0.1:5556" # use the right address by running code in test/run_wallace.r

# Here if the app contains the correct tabs and their respective names.
test_that("Module 6 Buttons Click", {  
  # move to Module 6
  remDr$navigate(appURL)
  webElems <- remDr$findElements("css selector", ".nav a")
  appTabLabels <- sapply(webElems, function(x){x$getElementText()})
  comp2Tab <- webElems[[which(appTabLabels == "6 Model")]]  
  # appCtrlLabels <- unlist(strsplit(appCtrlLabels[[1]], "\n"))
  comp2Tab$clickElement()
  
  # click Maxent
  field <- comp2Tab$findChildElement(value = "//input[@type='radio' and @value='Maxent']")
  initState <- field$isElementSelected()[[1]]
  field$clickElement()
  changeState <- field$isElementSelected()[[1]]
  expect_is(initState, "logical")
  expect_is(changeState, "logical")
  expect_false(initState == changeState)
  
  # click bioclim
  field <- comp2Tab$findChildElement(value = "//input[@type='radio' and @value='BIOCLIM']")
  initState <- field$isElementSelected()[[1]]
  field$clickElement()
  changeState <- field$isElementSelected()[[1]]
  expect_is(initState, "logical")
  expect_is(changeState, "logical")
  expect_false(initState == changeState)  
  
}) 


# Close the connection
remDr$close()
