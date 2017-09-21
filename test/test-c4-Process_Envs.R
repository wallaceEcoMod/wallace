context("component_4_Process_Env")

# Load the package
library(RSelenium)
library(testthat)


# Connect to the app (open another rstudio and run_wallace())
remDr <- remoteDriver() 
remDr$open(silent = TRUE)
appURL <- "http://127.0.0.1:5556"

# Move to comp4
remDr$navigate(appURL)
webElems <- remDr$findElements("css selector", ".nav a")
appTabLabels <- sapply(webElems, function(x){x$getElementText()})
comp4Tab <- webElems[[which(appTabLabels == "4 Process Envs")]]  
Sys.sleep(10)
comp4Tab$clickElement()

# Here if the app contains the correct tabs and their respective names.
test_that("Component 4 Buttons Click", {  
  
  # The click function
  check_click <- function(value = 'mcp', initial = FALSE) {
    field <- comp4Tab$findChildElement(
      value = paste0("//input[@type='radio' and @value='", value, "']"))
    initState <- field$isElementSelected()[[1]]
    field$clickElement()
    changeState <- field$isElementSelected()[[1]]
    expect_is(initState, "logical")  
    expect_is(changeState, "logical") 
    if (initial) {
      expect_true(initState == changeState)  
    } else {
      expect_false(initState == changeState)    
    }
  }
  
  # Click on background extent options
  check_click(value = 'mcp', initial = FALSE)
  check_click(value = 'bb', initial = FALSE)
  check_click(value = 'ptbuf', initial = FALSE)
  
  # Click on the user-specified module
  check_click(value = 'bgUser', initial = FALSE)
  # Go back
  check_click(value = 'bgSel', initial = FALSE)
  
  
}) 

test_that("Buffer distance component 4", {
  
  field <- comp4Tab$findChildElement(value = "//input[@id='c4_bgExtent-bgBuf']")
  
  # Arrow test
  expect_equal(field$getElementAttribute("value")[[1]], "0.5")
  field$sendKeysToElement(list(key ="up_arrow"))
  expect_equal(field$getElementAttribute("value")[[1]], "1")
  field$sendKeysToElement(list(key ="up_arrow"))
  expect_equal(field$getElementAttribute("value")[[1]], "1.5")
  field$sendKeysToElement(list(key ="down_arrow"))
  expect_equal(field$getElementAttribute("value")[[1]], "1")
  
  # Writting test
  field$sendKeysToElement(list(key = "backspace"))
  field$sendKeysToElement(list("50"))
  expect_equal(field$getElementAttribute("value")[[1]], "50")
  
})


test_that("Component 4 step 2 test", {
  
  field <- comp4Tab$findChildElement(value = "//input[@id='c4_bgMskAndSamplePts-bgPtsNum']")
  
  # Arrow test
  expect_equal(field$getElementAttribute("value")[[1]], "10000")
  field$sendKeysToElement(list(key ="up_arrow"))
  expect_equal(field$getElementAttribute("value")[[1]], "10001")
  field$sendKeysToElement(list(key ="up_arrow"))
  expect_equal(field$getElementAttribute("value")[[1]], "10002")
  field$sendKeysToElement(list(key ="down_arrow"))
  expect_equal(field$getElementAttribute("value")[[1]], "10001")
  
  # Writting test
  for (i in 1:nchar("10000")) {
    field$sendKeysToElement(list(key = "backspace"))  
  }
  field$sendKeysToElement(list("50"))
  expect_equal(field$getElementAttribute("value")[[1]], "50")
})




test_that("Component 4 select download file type", {  
  #skip_on_cran()
  
  # This path change once it is clicked (has-options and full change position)
  path <- "//div[@class='selectize-input items full has-options has-items']"
  
  drop.menu <- comp4Tab$findChildElement(value = path)
  
  # Test if we can select all resolutions 
  select_resol <- function(res = "'ascii'") {
    drop.menu$clickElement()
    select.res.05 <- comp4Tab$findChildElement(value = paste0("//div[@data-value=",
                                                                res, "and @class='option']"))
    initState <- select.res.05$isElementSelected()[[1]]
    expect_false(initState)
    select.res.05$clickElement()
    selected.item <- comp4Tab$findChildElement(value = paste0("//div[@class='item' and @data-value=", res,  "]"))
    expect_true(length(selected.item) == 1)
  }
  # Select a bioclimatic variable resolution of 30
  select_resol("'ascii'")
  select_resol("'raster'")
  select_resol("'GTiff'")
})




# Close the connection
remDr$close()
