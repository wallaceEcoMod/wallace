
occDens_UI <- function(id) {
  ns <- NS(id)
  tagList(

      )
}

occDens_MOD <- function(input, output, session) {
  reactive({
    # FUNCTION CALL ####
    occDens <- cESpace_occDens(curSp()[1], curSp()[2], 
                               msp[[curMSp()]]$pca, logs, shiny = TRUE)
    if (is.null(occDens)) return()
    
    # LOAD INTO MSP ####
    if(is.null(msp[[curMSp()]])) {
      msp[[curMSp()]] <- list(occDens = occDens)
    }else{
      msp[[curMSp()]]$occDens <- occDens
    }
    
    # RMD VALUES ####
    # add to vector of IDs removed
    
    
    return(occDens)
  })
}
