context("module_1_Occ_Data")

# Load the package
library(RSelenium)
library(testthat)
# test_dir('/Users/musasabi/Documents/github/wallace/test', filter = 'mod1', reporter = "Tap")

# Connect to the app (open another rstudio and run_wallace())
# NOTE: use the right address by running code in test/run_wallace.r
# !!!!!!!!!!! leave this as port = 5556 ! unless that port doesn't work for you, then suggest another. or are we not supposed to set a port when others test? if that's the case do we really have to manually set it for development?
#remDr <- remoteDriver(port=5556) ## MIGHT BREAK THINGS
remDr <- remoteDriver() 
remDr$open(silent = TRUE)
appURL <- "http://127.0.0.1:5556"


# move to Component 1
remDr$navigate(appURL)
# necessary for waiting for db query to load
remDr$setImplicitWaitTimeout(milliseconds = 100000)
compTabs <- remDr$findElements("css selector", ".nav a")
compTabLabels <- sapply(compTabs, function(x){x$getElementText()})
comp1Tab <- compTabs[[which(compTabLabels == "1 Occ Data")]]  
# appCtrlLabels <- unlist(strsplit(appCtrlLabels[[1]], "\n"))
comp1Tab$clickElement()

resultsTabs <- remDr$findElements("css selector", ".nav a")
resultsTabLabels <- sapply(resultsTabs, function(x){x$getElementText()})
occsTblTab <- resultsTabs[[which(resultsTabLabels == "Occs Tbl")]]  


# Here if the app contains the correct tabs and their respective names.
test_that("Component 1 Module Query Database: Buttons", {  
  # click gbif button
  field <- comp1Tab$findChildElement(value = "//input[@type='radio' and @value='gbif']")
  initState <- field$isElementSelected()[[1]]
  field$clickElement()
  changeState <- field$isElementSelected()[[1]]
  expect_is(initState, "logical")  
  expect_is(changeState, "logical")  
  expect_true(initState == changeState)  
  
  # click Vertnet button
  field <- comp1Tab$findChildElement(value = "//input[@type='radio' and @value='vertnet']")
  initState <- field$isElementSelected()[[1]]
  field$clickElement()
  changeState <- field$isElementSelected()[[1]]
  expect_is(initState, "logical")  
  expect_is(changeState, "logical")  
  expect_false(initState == changeState)  
  
  #click BISON button (isn't this database defunded?)
  field <- comp1Tab$findChildElement(value = "//input[@type='radio' and @value='bison']")
  initState <- field$isElementSelected()[[1]]
  field$clickElement()
  changeState <- field$isElementSelected()[[1]]
  expect_is(initState, "logical")  
  expect_is(changeState, "logical")  
  expect_false(initState == changeState)  
})

test_that("Component 1 Module Query Database: Slider", {
  # CM - this line doesn't work for me; can't find this element and also cant find it when searching for the string in ui.R. here's the error (and the help website it suggests is useless)
  # Selenium message:Unable to locate element: //input[@id='c1_queryDb-occNum']
  # For documentation on this error, please visit: http://seleniumhq.org/exceptions/no_such_element.html
  # Build info: version: '3.4.0', revision: 'unknown', time: 'unknown'
  # System info: host: 'CORYs-MacBook-Pro.local', ip: 'fe80:0:0:0:c50:e4d9:989a:f8f3%en0', os.name: 'Mac OS X', os.arch: 'x86_64', os.version: '10.12.6', java.version: '1.8.0_144'
  # Driver info: org.openqa.selenium.firefox.FirefoxDriver
  # Capabilities [{moz:profile=/var/folders/_3/kr0fjz7s55v5bfw8bmqwhld00000gn/T/rust_mozprofile.FkxP54rBs3ms, rotatable=false, timeouts={implicit=0.0, pageLoad=300000.0, script=30000.0}, pageLoadStrategy=normal, platform=ANY, specificationLevel=0.0, moz:accessibilityChecks=false, acceptInsecureCerts=false, browserVersion=55.0.3, platformVersion=16.7.0, moz:processID=10759.0, browserName=firefox, javascriptEnabled=true, platformName=darwin}]
  # Session ID: c53e5716-3b47-864c-bc50-3137487f4b0c
  # *** Element info: {Using=xpath, value=//input[@id='c1_queryDb-occNum']}
  
  # Error: 	 Summary: NoSuchElement
  # Detail: An element could not be located on the page using the given search parameters.
  # class: org.openqa.selenium.NoSuchElementException
  # Further Details: run errorDetails method
  
  slider <- comp1Tab$findChildElement(value = "//input[@id='c1_queryDb-occNum']")
  sliderDim <- slider$getElementSize()
  expect_equal(sliderDim$width, 4)
  expect_equal(sliderDim$height, 4)
  # this is not working for some reason, probably bc of link below
  # https://stackoverflow.com/questions/29065334/rselenium-not-able-to-run-example-code
  # sliderButton <- comp1Tab$findChildElement(value = "//span[@class='irs-slider single']")
  # remDr$mouseMoveToLocation(webElement = sliderButton)
})

test_that("Component 1 Module Query Database: Check DB Query", {
  # find textInput field and click
  field <- comp1Tab$findChildElement(value = "//input[@type='text']")
  field$clickElement()
  # write text
  field$sendKeysToElement(list("Puma concolor"))
  # find button and click
  # CM: can't find this element, but i do see it on line 43 of ui.r, so seems like i should. here's the error:
  # Selenium message:Unable to locate element: //button[@id='goDbOccs']
  # For documentation on this error, please visit: http://seleniumhq.org/exceptions/no_such_element.html
  # Build info: version: '3.4.0', revision: 'unknown', time: 'unknown'
  # System info: host: 'CORYs-MacBook-Pro.local', ip: 'fe80:0:0:0:c50:e4d9:989a:f8f3%en0', os.name: 'Mac OS X', os.arch: 'x86_64', os.version: '10.12.6', java.version: '1.8.0_144'
  # Driver info: org.openqa.selenium.firefox.FirefoxDriver
  # Capabilities [{moz:profile=/var/folders/_3/kr0fjz7s55v5bfw8bmqwhld00000gn/T/rust_mozprofile.FkxP54rBs3ms, rotatable=false, timeouts={implicit=0.0, pageLoad=300000.0, script=30000.0}, pageLoadStrategy=normal, platform=ANY, specificationLevel=0.0, moz:accessibilityChecks=false, acceptInsecureCerts=false, browserVersion=55.0.3, platformVersion=16.7.0, moz:processID=10759.0, browserName=firefox, javascriptEnabled=true, platformName=darwin}]
  # Session ID: c53e5716-3b47-864c-bc50-3137487f4b0c
  # *** Element info: {Using=xpath, value=//button[@id='goDbOccs']}
  # 
  # Error: 	 Summary: NoSuchElement
  # Detail: An element could not be located on the page using the given search parameters.
  # class: org.openqa.selenium.NoSuchElementException
  # Further Details: run errorDetails method
  button <- comp1Tab$findChildElement(value = "//button[@id='goDbOccs']")
  button$clickElement()
  # click occsTbl tab
  occsTblTab$clickElement()
  # find data table
  # NOTE: finding elements (plural) does not error when none exist
  occsTbl <- occsTblTab$findChildElements(value = '//table')
  # check if the table's there
  # CM: can't find this element
  expect_true(occsTbl[[1]]$isElementDisplayed()[[1]])
})

test_that("Component 1 Module User-specified: Buttons", {
  #click User points button 
  field <- comp1Tab$findChildElement(value = "//input[@type='radio' and @value='user']")
  initState <- field$isElementSelected()[[1]]
  field$clickElement()
  
  field <- comp1Tab$findChildElement(value = "//input[@id='c1_userOccs-userCSV']")
  initState <- field$isElementSelected()[[1]]
  field$clickElement()
  changeState <- field$isElementSelected()[[1]]
  expect_is(initState, "logical")
  expect_is(changeState, "logical")
  # don't think the states should be different, so maybe not necessary here
  # expect_false(initState == changeState)
  
  field <- comp1Tab$findChildElement(value = "//button[@type='button' and @id='goUserOccs']")
  initState <- field$isElementSelected()[[1]]
  field$clickElement()
  changeState <- field$isElementSelected()[[1]]
  expect_is(initState, "logical")
  expect_is(changeState, "logical")
})

# Close the connection
remDr$close()
