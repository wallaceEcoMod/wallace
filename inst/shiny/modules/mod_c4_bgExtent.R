
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
    bgExt <- c4_bgExtent(spp[[curSp()]]$occs, 
                         spp[[curSp()]]$envs, 
                         input$bgSel, 
                         input$bgBuf, 
                         logs, shiny=TRUE)
    
    req(bgExt)
    
    # LOAD INTO SPP ####
    spp[[curSp()]]$procEnvs$bgExt <- bgExt
    
    # METADATA ####
    spp[[curSp()]]$rmm$model$maxent$backgroundSizeRule <- paste0(input$bgSel, ', ', input$bgBuf, ' degree buffer')
    
    # RETURN ####
    # output the species name
    # return(bgExt)
  })
}
