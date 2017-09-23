context("component_5_Partition_Occs")

# Load the package
library(RSelenium)
library(testthat)

# Connect to the app (open another rstudio and run_wallace())
remDr <- remoteDriver() # use the right address by running code in test/run_wallace.r
remDr$open(silent = TRUE)
appURL <- "http://127.0.0.1:5556" # use the right address by running code in test/run_wallace.r

# move to Component 1 (to download the data to partition)
remDr$navigate(appURL)
# necessary for waiting for db query to load
remDr$setImplicitWaitTimeout(milliseconds = 100000)
compTabs <- remDr$findElements("css selector", ".nav a")
compTabLabels <- sapply(compTabs, function(x){x$getElementText()})
comp1Tab <- compTabs[[which(compTabLabels == "1 Occ Data")]]  
# appCtrlLabels <- unlist(strsplit(appCtrlLabels[[1]], "\n"))
comp1Tab$clickElement()

# select data (Comp1)
field.gbif <- comp1Tab$findChildElement(value = "//input[@type='radio' and @value='gbif']")
field.gbif$clickElement()
speciesField<- comp1Tab$findChildElement(value = "//input[@id='c1_queryDb-spName']")
speciesField$clickElement()
speciesField$sendKeysToElement(list('Acer rubrum'))
queryButton= comp1Tab$findChildElement(value = "//button[@type='button' and @id='goDbOccs']")
queryButton$clickElement()
remDr$setImplicitWaitTimeout(milliseconds = 100000)

# get env data (Comp3)
comp3Tab <- compTabs[[which(compTabLabels == "3 Env Data")]]  
comp3Tab$clickElement()
# select resolution of 10
drop.menu <- comp3Tab$findChildElement(value = "//div[@class='selectize-control shinyjs-resettable single']")
drop.menu$clickElement()
res="'10'"
select.res.10 <- comp3Tab$findChildElement(value = paste0("//div[@data-value=",
                                                          res, "and @class='option']"))
select.res.10$clickElement()
#selected.item <- comp3Tab$findChildElement(value = paste0("//div[@class='item' and @data-value=", res,  "]"))
# select just a few predictors for speed
check.spec <- comp3Tab$findChildElement(value = "//input[@id='c3_wcBioclims-bcSelChoice']")
check.spec$clickElement()
for (bio in c(4:19)) {
  check.vars <- check.spec$findChildElement(
    value = paste0("//input[@name='c3_wcBioclims-bcSels' and @value='bio", bio, "']"))
  check.vars$clickElement()
}
# load env data
loadEnvButton= comp1Tab$findChildElement(value = "//button[@type='button' and @id='goEnvData']")
loadEnvButton$clickElement()
  # takes a moment to download the first time
remDr$setImplicitWaitTimeout(milliseconds = 100000)

# choose domain (Comp4)
comp4Tab <- compTabs[[which(compTabLabels == "4 Process Envs")]]  
comp4Tab$clickElement()
extentButton= comp1Tab$findChildElement(value = "//button[@type='button' and @id='goBgExt']")
extentButton$clickElement()
backgroundButton= comp1Tab$findChildElement(value = "//button[@type='button' and @id='goBgMask']")
backgroundButton$clickElement()

# move to Comp 5
comp5Tab <- compTabs[[which(compTabLabels == "5 Partition Occs")]]  
comp5Tab$clickElement()

test_that("Component 5 Random-K fold", {
  nFoldsField=comp1Tab$findChildElement(value = "//input[@id='c5_partNsp-kfolds']")
  nFoldsField$clickElement()
  nFoldsField$clearElement()
  nFoldsField$sendKeysToElement(list('2'))
  partitionButton= comp1Tab$findChildElement(value = "//button[@type='button' and @id='goPartNsp']")
  partitionButton$clickElement()
  logBoxText <- comp5Tab$findChildElement(value = "//div[@id='logContent']")
  # not sure how to grabd the text to be sure that hte value is 
  # >Occurrences partitioned by random k-fold (k = 2 )
  #logBoxTextVals <- sapply(logBoxText, function(x){x$getElementText()})
  
  # 
  
})

## NOTE: Could add more tests of different partitioning here, but moving to modeling to make sure others aren't held up there.

# test_that("Component 5 Jackknife", {
#   drop.menu.partition <- comp3Tab$findChildElement(value = "//div[@class='selectize-input items full has-options has-items']")
#   drop.menu.partition$clickElement()
# 
#   
# })


# Close the connection
remDr$close()
