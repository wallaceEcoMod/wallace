# Load the package
library(RSelenium)
library(testthat)
source('test-functions.R')

context("component_1_Occ_Data")
# test_dir('/Users/musasabi/Documents/github/wallace/test', filter = 'c1', reporter = "Tap")

# Connect to the app (open another rstudio and run_wallace())
remDr <- remoteDriver() 
remDr$open(silent = TRUE)
appURL <- "http://127.0.0.1:5556"

remDr$navigate(appURL)
compTabs <- remDr$findElements("css selector", ".nav a")
compTabLabels <- sapply(compTabs, function(x) x$getElementText())
# Move to Component 3
comp1Tab <- compTabs[[which(compTabLabels == "1 Occ Data")]]  
comp1Tab$clickElement()

# Here if the app contains the correct tabs and their respective names.
test_that("Component 1 Module Query Database: Radio Buttons", {  
  # click gbif radio button
  field.gbif <- comp1Tab$findChildElement(value = "//input[@type='radio' and @value='gbif']")
  clickButton(field.gbif)
  
  # click Vertnet radio button
  field.vnet <- comp1Tab$findChildElement(value = "//input[@type='radio' and @value='vertnet']")
  clickButton(field.vnet)
  
  #click BISON radio button
  field.bison <- comp1Tab$findChildElement(value = "//input[@type='radio' and @value='bison']")
  clickButton(field.bison)
  
  # switch back to gbif
  field.gbif$clickElement()
})

test_that("Component 1 Module Query Database: Text Input", {
  field <- remDr$findElement('id', "c1_queryDb-spName")
  field$clickElement()
  field$clearElement()
  field$sendKeysToElement(list("Tremarctos ornatus"))
  val <- field$getElementAttribute('value')[[1]]
  expect_equal(val, "Tremarctos ornatus")
})

test_that("Component 1 Module Query Database: Slider", {
  slider <- comp1Tab$findChildElement(value = "//input[@id='c1_queryDb-occNum']")
  sliderDim <- slider$getElementSize()
  expect_equal(sliderDim$width, 4)
  expect_equal(sliderDim$height, 4)
})

test_that("Component 1 Module Query Database: Buttons", {
  button <- remDr$findElement('id', "goDbOccs")
  expect_true(button$isElementDisplayed()[[1]])
  
  button <- remDr$findElement('id', "dlDbOccs")
  expect_true(button$isElementDisplayed()[[1]])
})

test_that("Component 1 Module User-specified: Input", {
  field.userOccs <- comp1Tab$findChildElement(value = "//input[@type='radio' and @value='user']")
  field.userOccs$clickElement()
  input <- remDr$findElement('class', 'input-group')
  expect_true(input$isElementDisplayed()[[1]])
})

test_that("Component 1 Module User-specified: Buttons", {
  button <- remDr$findElement('id', "goUserOccs")
  expect_true(button$isElementDisplayed()[[1]])
})

# Close the connection
remDr$close()
