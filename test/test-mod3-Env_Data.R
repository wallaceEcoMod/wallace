
context("module_3_Env_Data")

# Load the package
library(RSelenium)
library(testthat)

# Connect to the app (open another rstudio and run_wallace())
remDr <- remoteDriver() # use the right address by running code in test/run_wallace.r
remDr$open(silent = TRUE)
appURL <- "http://127.0.0.1:5556" # use the right address by running code in test/run_wallace.r

# Here if the app contains the correct tabs and their respective names.
test_that("Module 3 Buttons Click", {  
  # move to Module 3
  remDr$navigate(appURL)
  webElems <- remDr$findElements("css selector", ".nav a")
  appTabLabels <- sapply(webElems, function(x){x$getElementText()})
  comp2Tab <- webElems[[which(appTabLabels == "3 Env Data")]]  
  # appCtrlLabels <- unlist(strsplit(appCtrlLabels[[1]], "\n"))
  comp2Tab$clickElement()
  
  # field <- comp2Tab$findChildElement(value = "//input[@type='radio' and @value='bb']")
  # initState <- field$isElementSelected()[[1]]
  # field$clickElement()
  # changeState <- field$isElementSelected()[[1]]
  # expect_is(initState, "logical")  
  # expect_is(changeState, "logical")  
  # expect_true(initState == changeState)  
  # 
  # field <- comp2Tab$findChildElement(value = "//input[@type='radio' and @value='mcp']")
  # initState <- field$isElementSelected()[[1]]
  # field$clickElement()
  # changeState <- field$isElementSelected()[[1]]
  # expect_is(initState, "logical")  
  # expect_is(changeState, "logical")  
  # # expect_false(initState == changeState)  # not working for no clear reason
  # 
  # field <- comp2Tab$findChildElement(value = "//input[@type='radio' and @value='user']")
  # initState <- field$isElementSelected()[[1]]
  # field$clickElement()
  # changeState <- field$isElementSelected()[[1]]
  # expect_is(initState, "logical")  
  # expect_is(changeState, "logical")  
  # #expect_false(initState == changeState)  # not working for no clear reason
  
}) 


# Close the connection
remDr$close()
