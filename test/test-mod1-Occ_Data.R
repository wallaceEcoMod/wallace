context("module_1_Occ_Data")

# Load the package
library(RSelenium)
library(testthat)

# Connect to the app (open another rstudio and run_wallace())
remDr <- remoteDriver(port=5556) # use the right address by running code in test/run_wallace.r
remDr$open(silent = TRUE)
appURL <- "http://127.0.0.1:5556" # use the right address by running code in test/run_wallace.r

# Here if the app contains the correct tabs and their respective names.
test_that("Module 1 Buttons Click", {  
  # move to Module 1
  remDr$navigate(appURL)
  webElems <- remDr$findElements("css selector", ".nav a")
  appTabLabels <- sapply(webElems, function(x){x$getElementText()})
  comp2Tab <- webElems[[which(appTabLabels == "1 Occ Data")]]  
  # appCtrlLabels <- unlist(strsplit(appCtrlLabels[[1]], "\n"))
  comp2Tab$clickElement()
 
  # click gbif button
  field <- comp2Tab$findChildElement(value = "//input[@type='radio' and @value='gbif']")
  initState <- field$isElementSelected()[[1]]
  field$clickElement()
  changeState <- field$isElementSelected()[[1]]
  expect_is(initState, "logical")  
  expect_is(changeState, "logical")  
  expect_true(initState == changeState)  
  
  # click Vertnet button
  field <- comp2Tab$findChildElement(value = "//input[@type='radio' and @value='vertnet']")
  initState <- field$isElementSelected()[[1]]
  field$clickElement()
  changeState <- field$isElementSelected()[[1]]
  expect_is(initState, "logical")  
  expect_is(changeState, "logical")  
  expect_false(initState == changeState)  
  
  #click BISON button (isn't this database defunded?)
  field <- comp2Tab$findChildElement(value = "//input[@type='radio' and @value='bison']")
  initState <- field$isElementSelected()[[1]]
  field$clickElement()
  changeState <- field$isElementSelected()[[1]]
  expect_is(initState, "logical")  
  expect_is(changeState, "logical")  
  expect_false(initState == changeState)  
  
  #click User points button 
  ## not sure how to select 'Module: User-specified Occurrences' line 327 of ui.r
  # field <- comp2Tab$findChildElement(value = "//input[@id='mod' and @value='Module: User-specified Occurrences']")
  # initState <- field$isElementSelected()[[1]]
  # field$clickElement()
  # changeState <- field$isElementSelected()[[1]]
  # expect_is(initState, "logical")  
  # expect_is(changeState, "logical")  
  # expect_false(initState == changeState)  
  
}) 

# test_that("species data downloads from GBIF", {  
#   remDr$navigate(appURL)
#   webElems <- remDr$findElements("css selector", ".nav a")
#   appTabLabels <- sapply(webElems, function(x){x$getElementText()})
#   comp2Tab <- webElems[[which(appTabLabels == "1 Occ Data")]]  
#   # appCtrlLabels <- unlist(strsplit(appCtrlLabels[[1]], "\n"))
#   comp2Tab$clickElement()
#   field <- comp2Tab$findChildElement(value = "//input[@type='radio' and @value='gbif']")
#   initState <- field$isElementSelected()[[1]]  
#     
#   
#   # subTitles <- sapply(webElem, function(x){x$getElementText()})
#   # subTitles <- sapply(webElem, function(x){x$getElementText()})
# 
# 
#   #field1 <- comp2Tab$findChildElement(value = "//input[@type='spName' and @value='']")
# 
#   #field1 <- field$findChildElement(value = "//input[@type='spName' and @value='']")
# })


#webElem <- remDr$findElement("css selector")

#webElems <- remDr$findElements("id", "tabs")
#remDr$mouseMoveToLocation(webElement = webElems[[1]])

#initState <- webElems$isElementSelected()[[1]]

# Close the connection
remDr$close()
