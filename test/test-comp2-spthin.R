context("module_1")

# Load the package
library(RSelenium)
library(testthat)

# Connect to the app (open another rstudio and run_wallace())
remDr <- remoteDriver()
remDr$open(silent = TRUE)
appURL <- "http://127.0.0.1:6030"

# Here if the app contains the correct tabs and their respective names.
test_that("Tabs are present", {  
  remDr$navigate(appURL)
  webElems <- remDr$findElements("css selector", ".nav a")
  appTabLabels <- sapply(webElems, function(x){x$getElementText()})
  comp2Tab <- webElems[[which(appTabLabels == "2 Process Occs")]]  
  # appCtrlLabels <- unlist(strsplit(appCtrlLabels[[1]], "\n"))
  comp2Tab$clickElement()
  spthin <- comp2Tab$findChildElement(value = "//input[@type='radio' and @value='spthin']")
  spthin$clickElement()
  
  
  # subTitles <- sapply(webElem, function(x){x$getElementText()})
  # subTitles <- sapply(webElem, function(x){x$getElementText()})
   
})

#webElem <- remDr$findElement("css selector")

#webElems <- remDr$findElements("id", "tabs")
#remDr$mouseMoveToLocation(webElement = webElems[[1]])


#initState <- webElems$isElementSelected()[[1]]


# Close the connection
remDr$close()
