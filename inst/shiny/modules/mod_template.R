xx_UI <- function(id) {
  ns <- NS(id)
  tagList(
    
  )
}

xx_MOD <- function(input, output, session) {
  reactive({
    # FUNCTION CALL ####
    out <- cx_xx(param1, param2, param3, logs)
    
    if (is.null(out)) return()
    
    # LOAD INTO SPP ####
    spp[[curSp()]]$xx <- out
    
    # RMD VALUES ####
    xxRMD <- list(xx = xx)
    spp[[curSp()]]$rmd <- list(cx = xxRMD)
    
    # RETURN ####
    return(out)
  })
}