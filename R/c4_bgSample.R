c4_bgSample <- function(bgMask, bgPtsNum, logs=NULL, shiny=FALSE) {
  # sample random background points
  withProgress(message = "Generating background points...", {
    rvals <- raster::getValues(bgMask)
    num.vals <- sum(!is.na(rvals))
    pct <- round((bgPtsNum / num.vals) * 100, digits = 2)
    bgXY <- dismo::randomPoints(bgMask, bgPtsNum)
  })
  logs %>% writeLog(curSp(), ': Random background points sampled (n =', bgPtsNum, 
                    ':', pct, '% of cells with values).')
  
  return(bgXY)
}