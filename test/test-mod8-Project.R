
context("module_8_Project")

# Load the package
library(RSelenium)
library(testthat)

# Connect to the app (open another rstudio and run_wallace())
remDr <- remoteDriver() # use the right address by running code in test/run_wallace.r
remDr$open(silent = TRUE)
appURL <- "http://127.0.0.1:5556" # use the right address by running code in test/run_wallace.r

# Here if the app contains the correct tabs and their respective names.
test_that("Module 8 Buttons Click", {  
  # move to Module 8
  remDr$navigate(appURL)
  webElems <- remDr$findElements("css selector", ".nav a")
  appTabLabels <- sapply(webElems, function(x){x$getElementText()})
  comp2Tab <- webElems[[which(appTabLabels == "8 Project")]]  
  # appCtrlLabels <- unlist(strsplit(appCtrlLabels[[1]], "\n"))
  comp2Tab$clickElement()
 
  # click project new time
  field <- comp2Tab$findChildElement(value = "//input[@type='radio' and @value='pjTime']")
  initState <- field$isElementSelected()[[1]]
  field$clickElement()
  changeState <- field$isElementSelected()[[1]]
  expect_is(initState, "logical")
  expect_is(changeState, "logical")
  expect_false(initState == changeState)
  
  # click project new area
  field <- comp2Tab$findChildElement(value = "//input[@type='radio' and @value='pjArea']")
  initState <- field$isElementSelected()[[1]]
  field$clickElement()
  changeState <- field$isElementSelected()[[1]]
  expect_is(initState, "logical")
  expect_is(changeState, "logical")
  expect_false(initState == changeState)  
  
  # click Calc Env Sim
  field <- comp2Tab$findChildElement(value = "//input[@type='radio' and @value='mess']")
  initState <- field$isElementSelected()[[1]]
  field$clickElement()
  changeState <- field$isElementSelected()[[1]]
  expect_is(initState, "logical")
  expect_is(changeState, "logical")
  expect_false(initState == changeState)  
  
}) 


# Close the connection
remDr$close()
