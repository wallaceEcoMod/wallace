# Load the package
library(RSelenium)
library(testthat)

context("component_5_Partition_Occs")

# Connect to the app (open another rstudio and run_wallace())
remDr <- remoteDriver() 
remDr$open(silent = TRUE)
appURL <- "http://127.0.0.1:5556"

remDr$navigate(appURL)
compTabs <- remDr$findElements("css selector", ".nav a")
compTabLabels <- sapply(compTabs, function(x) x$getElementText())
# Move to Component 3
comp5Tab <- compTabs[[which(compTabLabels == "5 Partition Occs")]]    
comp5Tab$clickElement()


# Close the connection
remDr$close()
