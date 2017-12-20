xx_UI <- function(id) {
  ns <- NS(id)
  tagList(
    
  )
}

xx_MOD <- function(input, output, session) {
  reactive({
    # FUNCTION CALL ####
    out <- cx_xx(param1, param2, param3, logs, shiny=TRUE)
    
    if (is.null(out)) return()
    
    # RMD VALUES ####
    rmd$cx$xx <- input$xx
    
    # METADATA ####
    #
    #
    
    # MAPPING ####
    #
    #
    
    # RETURN ####
    return(out)
  })
}