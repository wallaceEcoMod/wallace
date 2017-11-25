# test that button changes state when clicked
clickButton <- function(webElem) {
  initState <- webElem$isElementSelected()[[1]]
  webElem$clickElement()
  changeState <- webElem$isElementSelected()[[1]]
  expect_is(initState, "logical")  
  expect_is(changeState, "logical")
  expect_false(initState == changeState)
}

# test that select dropdown choices are correctly selected 
# for dropdowns with the initial "data-value" class "option"
selectDropdownOption <- function(tab, dataValue) {
  cElem <- tab$findChildElement(value = paste0("//div[@data-value=", dataValue, " and @class='option']"))
  initState <- cElem$isElementSelected()[[1]]
  expect_false(initState)
  cElem$clickElement()
  selected.item <- tab$findChildElement(value = paste0("//div[@class='item' and @data-value=", dataValue,  "]"))
  expect_true(length(selected.item) == 1)
}

# test that select dropdown choices are correctly selected 
# for dropdowns with the initial "data-value" class "item"
selectDropdownItem <- function(tab, dataValueInitial, dataValues) {
  cElem <- tab$findChildElement(value = paste0("//div[@data-value=", dataValueInitial, "and @class='item']"))
  initState <- cElem$isElementSelected()[[1]]
  expect_false(initState)
  cElem$clickElement()
  selected.item <- tab$findChildElement(value = paste0("//div[@class='item' and @data-value=", dataValueInitial,  "]"))
  expect_true(length(selected.item) == 1)
  
  for (i in 1:length(dataValues)) {
    cElemNext <- tab$findChildElement(value = paste0("//div[@data-value=", dataValues[[i]], "and @class='option']"))  
    initState <- cElemNext$isElementSelected()[[1]]
    expect_false(initState)
    cElemNext$clickElement()
    cElemNext.sel <- tab$findChildElement(value = paste0("//div[@class='item' and @data-value=", dataValues[[i]],  "]"))
    expect_true(length(cElemNext.sel) == 1)
    cElemNext.sel$clickElement()
  }
  cElemNext.sel$clickElement()
}
