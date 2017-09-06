context("module_1")

# Load the package
library(RSelenium)
library(testthat)

# Connect to the app (open another rstudio and run_wallace())
remDr <- remoteDriver(port=5556)
remDr$open(silent = TRUE)
appURL <- "http://127.0.0.1:5556"

# Here if the app contains the correct tabs and their respective names.
test_that("Tabs are present", {  
  remDr$navigate()
  webElems <- remDr$findElements("id", "tabs")
  appCtrlLabels <- sapply(webElems, function(x){x$getElementText()})
  appCtrlLabels <- unlist(strsplit(appCtrlLabels[[1]], "\n"))
  expect_equal(appCtrlLabels[1], "Intro")  
  expect_equal(appCtrlLabels[2], "1 Occ Data")  
  expect_equal(appCtrlLabels[3], "2 Process Occs")  
  expect_equal(appCtrlLabels[4], "3 Env Data")  
  expect_equal(appCtrlLabels[5], "4 Process Envs")  
  expect_equal(appCtrlLabels[6], "5 Partition Occs")  
  expect_equal(appCtrlLabels[7], "6 Model")
  expect_equal(appCtrlLabels[8], "7 Visualize")
  expect_equal(appCtrlLabels[9], "8 Project")
  expect_equal(appCtrlLabels[10], "Session Code")  
})

#webElem <- remDr$findElement("css selector")

#webElems <- remDr$findElements("id", "tabs")
#remDr$mouseMoveToLocation(webElement = webElems[[1]])


#initState <- webElems$isElementSelected()[[1]]


# Close the connection
remDr$close()
