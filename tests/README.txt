Instructions for developing RSelenium tests for Wallace

1. Install geckodriver for Firefox or chromedriver for Chrome. This can be done easily with homebrew on MacOS with the following formulas:

brew install geckodriver
brew install chromedriver

2. When opening the remote driver, soimply specify which browser to open this way:

remDr <- remoteDriver(browserName = "chrome") 


# Developing and running shiny unit tests

1. Start selenium server from terminal: `java -jar selenium-server-standalone.jar` 

2. Startup an RStudio window and start Wallace by using the `run_wallace.R` script in the `wallace/test/` directory

3. Start ANOTHER RStudio window and run the first 21 lines of the test-workflow.R code (you might have to `library(testthat)` first, so `context` is actually defined.
