
bgExtent_UI <- function(id) {
  ns <- NS(id)
  tagList(
    radioButtons(ns("bgSel"), "Background Extents:",
                 choices = list("Bounding box" = 'bb', 
                                "Minimum convex polygon" = 'mcp',
                                "Point buffers" = 'ptbuf'),
                 selected='bb'),
    tags$div(title='Buffer area in degrees (1 degree = ~111 km). Exact length varies based on latitudinal position.',
             numericInput(ns("bgBuf"), label = "Study region buffer distance (degree)", value = 0.5, min = 0, step = 0.5))
  )
}

bgExtent_MOD <- function(input, output, session) {
  reactive({
    # FUNCTION CALL ####
    sp <- spp[[curSp()]]
    bgExt <- c4_bgExtent(sp$occs, sp$envs, input$bgSel, input$bgBuf, logs, shiny=TRUE)
    
    if (is.null(bgExt)) return()
    
    # LOAD INTO SPP ####
    # record for RMD
    spp[[curSp()]]$bgExt <- bgExt
    
    # RMD VALUES ####
    x <- list(shp = input$bgSel, buf = input$bgBuf)
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
