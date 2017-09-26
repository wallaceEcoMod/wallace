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
compTabs <- remDr$findElements("css selector", ".nav a")
compTabLabels <- sapply(compTabs, function(x){x$getElementText()})

# move to Comp 5
comp5Tab <- compTabs[[which(compTabLabels == "5 Partition Occs")]]  
comp5Tab$clickElement()




# Close the connection
remDr$close()
