
bgMskAndSamplePts_UI <- function(id) {
  ns <- NS(id)
  tagList(
    numericInput(ns("bgPtsNum"), label = "No. of background points", value = 10000, min = 1, step = 1)
  )
}

bgMskAndSamplePts_MOD <- function(input, output, session, rvs) {
  reactive({
    # FUNCTION CALL ####
    bgMask <- c4_bgMask(spp[[curSp()]]$occs, spp[[curSp()]]$envs, spp[[curSp()]]$bgExt, logs, shiny=TRUE)
    bgPts <- c4_bgSample(spp[[curSp()]]$occs, bgMask, input$bgPtsNum, logs, shiny=TRUE)
    
    withProgress(message = "Extracting values...", {
      z <- raster::extract(spp[[curSp()]]$envs, bgPts)
    })
    
    
    if (is.null(bgMask) | is.null(bgPts)) return()
    
    # LOAD INTO SPP ####
    spp[[curSp()]]$bgMask <- bgMask
    spp[[curSp()]]$bgPts <- bgPts
    spp[[curSp()]]$bgPts.z <- z
    
    # RMD VALUES ####
    x <- list(bgPtsNum = input$bgPtsNum)
    if(is.null(spp[[curSp()]]$rmd$c4)) {
      spp[[curSp()]]$rmd$c4 <- x
    }else{
      spp[[curSp()]]$rmd$c4 <- c(spp[[curSp()]]$rmd$c4, x)
    }
    
    # RETURN ####
    # output the species name
    # return(bgExt)
  })
}
