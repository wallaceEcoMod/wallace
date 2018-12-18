# Load the package
library(RSelenium)
library(testthat)
source('test-functions.R')

context("component_4_Process_Envs")
# test_dir('/Users/musasabi/Documents/github/wallace/test', filter = 'c4', reporter = "Tap")

# Connect to the app (open another rstudio and run_wallace())
remDr <- remoteDriver() 
remDr$open(silent = TRUE)
appURL <- "http://127.0.0.1:5556"

remDr$navigate(appURL)
compTabs <- remDr$findElements("css selector", ".nav a")
compTabLabels <- sapply(compTabs, function(x) x$getElementText())
# Move to Component 3
comp4Tab <- compTabs[[which(compTabLabels == "4 Process Envs")]]    
comp4Tab$clickElement()

test_that("Component 4: Radio Buttons", {  
  field.user <- comp4Tab$findChildElement(value = "//input[@type='radio' and @value='bgUser']")
  buttonClick(field.user)
  
  field.bgSel <- comp4Tab$findChildElement(value = "//input[@type='radio' and @value='bgSel']")
  buttonClick(field.user)
})

test_that("Component 4 Module Select Study Region: Radio Buttons", {  
  field.bb <- comp4Tab$findChildElement(value = "//input[@type='radio' and @value='bb']")
  buttonClick(field.bb)
  
  field.ptbuf <- comp4Tab$findChildElement(value = "//input[@type='radio' and @value='ptbuf']")
  buttonClick(field.ptbuf)
  
  field.mcp <- comp4Tab$findChildElement(value = "//input[@type='radio' and @value='mcp']")
  buttonClick(field.mcp)
}) 

test_that("Component 4 Module Select Study Region: Buttons", {
  button <- remDr$findElement('id', "goBgExt")
  expect_true(button$isElementDisplayed()[[1]])
})

test_that("Component 4 Module Select Study Region: Input Background Extent", {  
  input <- comp4Tab$findChildElement(value = "//input[@id='c4_bgExtent-bgBuf']")
  
  # Arrow test
  expect_equal(input$getElementAttribute("value")[[1]], "0.5")
  input$sendKeysToElement(list(key ="up_arrow"))
  expect_equal(input$getElementAttribute("value")[[1]], "1")
  input$sendKeysToElement(list(key ="up_arrow"))
  expect_equal(input$getElementAttribute("value")[[1]], "1.5")
  input$sendKeysToElement(list(key ="down_arrow"))
  expect_equal(input$getElementAttribute("value")[[1]], "1")
  
  input$sendKeysToElement(list(key = "backspace"))
  input$sendKeysToElement(list("50"))
  expect_equal(input$getElementAttribute("value")[[1]], "50")
})

test_that("Component 4 Module Select Study Region: Input Background Points", {  
  input <- comp4Tab$findChildElement(value = "//input[@id='c4_bgMskAndSamplePts-bgPtsNum']")
  
  # Arrow test
  expect_equal(input$getElementAttribute("value")[[1]], "10000")
  input$sendKeysToElement(list(key ="up_arrow"))
  expect_equal(input$getElementAttribute("value")[[1]], "10001")
  input$sendKeysToElement(list(key ="up_arrow"))
  expect_equal(input$getElementAttribute("value")[[1]], "10002")
  input$sendKeysToElement(list(key ="down_arrow"))
  expect_equal(input$getElementAttribute("value")[[1]], "10001")
  
  for (i in 1:nchar("10000")) {
    input$sendKeysToElement(list(key = "backspace"))  
  }
  input$sendKeysToElement(list("50"))
  expect_equal(input$getElementAttribute("value")[[1]], "50")
})

test_that("Component 4 Module Select Study Region: Input Download", { 
  # This path change once it is clicked (has-options and full change position)
  path <- "//div[@class='selectize-input items full has-options has-items']"
  
  drop.menu <- comp4Tab$findChildElement(value = path)
  drop.menu$clickElement()
  selectDropdownOption(comp4Tab, "'ascii'")
  drop.menu$clickElement()
  selectDropdownOption(comp4Tab, "'raster'")
  drop.menu$clickElement()
  selectDropdownOption(comp4Tab, "'GTiff'")
})

test_that("Component 4 Module User_specified Study Region: Input", {
  field.user <- comp4Tab$findChildElement(value = "//input[@type='radio' and @value='bgUser']")
  field.user$clickElement()
  input <- remDr$findElements('class', 'input-group')
  expect_true(input[[3]]$isElementDisplayed()[[1]])
})

# Close the connection
remDr$close()
