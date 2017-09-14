
context("module_2_Proccess_Occs")

# Load the package
library(RSelenium)
library(testthat)

# Connect to the app (open another rstudio and run_wallace())
remDr <- remoteDriver() # use the right address by running code in test/run_wallace.r
remDr$open(silent = TRUE)
appURL <- "http://127.0.0.1:5556" # use the right address by running code in test/run_wallace.r

# move to Module 2
remDr$navigate(appURL)
webElems <- remDr$findElements("css selector", ".nav a")
appTabLabels <- sapply(webElems, function(x){x$getElementText()})
comp2Tab <- webElems[[which(appTabLabels == "2 Process Occs")]]  
# appCtrlLabels <- unlist(strsplit(appCtrlLabels[[1]], "\n"))
comp2Tab$clickElement()


# NOT DONE - can figure out how to get a list of buttons
# test_that("Module 2 Buttons are Present", {  
#   field <- comp2Tab$findChildElement(value = "//input[@class='btn btn-default action-button']")
# })

# test_that("Module 2 thinning controls are present", {  
#   field <- comp2Tab$findChildElement(value = "//input[@type='radio' and @value='selpts']")
#   field$clickElement()
#   webElems <- remDr$findElements(value = "//input[@class='button'")
#   
#   (webElems <- field$findElements(value = "//input[@type='number' and @id='thinDist']"))
#   (appCtrlLabels <- sapply(webElems, function(x){x$getElementText()}))
#   # expect_equal(appCtrlLabels[[1]], "Select controls required:")  
#   # expect_equal(appCtrlLabels[[2]], "selectInput")  
#   # expect_equal(appCtrlLabels[[3]], "numericInput")  
#   # expect_equal(appCtrlLabels[[4]], "dateRangeInput")  
#   # expect_equal(appCtrlLabels[[5]], "sliderInput")  
# })

# Here if the app contains the correct tabs and their respective names.
test_that("Module 2 Buttons Click", {  
  
  field <- comp2Tab$findChildElement(value = "//input[@type='radio' and @value='spthin']")
  initState <- field$isElementSelected()[[1]]
  field$clickElement()
  changeState <- field$isElementSelected()[[1]]
  expect_is(initState, "logical")
  expect_is(changeState, "logical")
  expect_false(initState == changeState)
  
  field <- comp2Tab$findChildElement(value = "//input[@type='radio' and @value='selpts']")
  initState <- field$isElementSelected()[[1]]
  field$clickElement()
  changeState <- field$isElementSelected()[[1]]
  expect_is(initState, "logical")
  expect_is(changeState, "logical")
  expect_false(initState == changeState)
  
  # subTitles <- sapply(webElem, function(x){x$getElementText()})
  # subTitles <- sapply(webElem, function(x){x$getElementText()})
   
})


#webElem <- remDr$findElement("css selector")

#webElems <- remDr$findElements("id", "tabs")
#remDr$mouseMoveToLocation(webElement = webElems[[1]])


#initState <- webElems$isElementSelected()[[1]]


# Close the connection
remDr$close()
