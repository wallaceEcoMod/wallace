# test that button changes state when clicked
clickButton <- function(webElem) {
  initState <- webElem$isElementSelected()[[1]]
  webElem$clickElement()
  changeState <- webElem$isElementSelected()[[1]]
  expect_is(initState, "logical")  
  expect_is(changeState, "logical")
  expect_true(initState == changeState)
}

# test that select dropdown choices are correctly selected 
selectDropdown <- function(tab, dataValue) {
  cElem <- tab$findChildElement(value = paste0("//div[@data-value=", dataValue, "and @class='option']"))
  initState <- cElem$isElementSelected()[[1]]
  expect_false(initState)
  cElem$clickElement()
  selected.item <- tab$findChildElement(value = paste0("//div[@class='item' and @data-value=", dataValue,  "]"))
  expect_true(length(selected.item) == 1)
}
