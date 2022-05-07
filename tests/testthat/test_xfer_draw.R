#### COMPONENT xfer: Transfer Model
#### MODULE: Transfer to User provided area
context("xfer_draw")

# set coordinates (as in GUI)
longitude <- c(-27.78641, -74.09170, -84.01930, -129.74867, -142.19085,
               -45.55045, -28.56050)
latitude <- c(-40.40539, -37.02010, 2.28455, 40.75350, 56.35954,
              54.55045, -7.11861)
##make into matrix
userDrawPoly <- matrix(c(longitude, latitude), byrow = FALSE, ncol = 2)
##set buffer test 0 and >0
drawXfBufZero = 0
drawXfBuf = 0.5
## setId to 1
polyXfID = 1
polygonTest <- xfer_draw(polyXfXY = userDrawPoly, polyXfID, drawXfBuf)
polygonTestZero <- xfer_draw(polyXfXY = userDrawPoly, polyXfID,
                             drawXfBuf = drawXfBufZero)

test_that("output type checks", {
  # the drawn polygon does not include all localities
  expect_is(polygonTest,'SpatialPolygonsDataFrame')
  expect_is(polygonTestZero,'SpatialPolygonsDataFrame')
  ###the extent of the polygon for buffer = 0 corresponds to maximum provided
  expect_equal(sp::bbox(polygonTestZero)[1,1],min(longitude))
  expect_equal(sp::bbox(polygonTestZero)[2,1],min(latitude))
  expect_equal(sp::bbox(polygonTestZero)[1,2],max(longitude))
  expect_equal(sp::bbox(polygonTestZero)[2,2],max(latitude))
  ###the extent of the polygon for buffer > 0 is bigger than to maximum provided
  expect_lt(sp::bbox(polygonTest)[1,1],min(longitude))
  expect_lt(sp::bbox(polygonTest)[2,1],min(latitude))
  expect_gt(sp::bbox(polygonTest)[1,2],max(longitude))
  expect_gt(sp::bbox(polygonTest)[2,2],max(latitude))
  })
