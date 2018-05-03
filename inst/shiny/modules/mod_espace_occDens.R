
occDens_controlsUI <- function(id) {
  ns <- NS(id)
  tagList(

      )
}

occDens_resultsUI <- function(id) {
  ns <- NS(id)
  plotOutput(ns("occDensPlot"))
}

occDens_MOD <- function(input, output, session) {
  reactive({
    # FUNCTION CALL ####
    sp1 <- curSp()[1]
    sp2 <- curSp()[2]
    occDens <- cESpace_occDens(sp1, sp2, msp[[curMSp()]]$pca, shinyLogs)
    if (is.null(occDens)) return()
    
    # LOAD INTO MSP ####
    if(is.null(msp[[curMSp()]])) {
      msp[[curMSp()]] <- list(occDens = occDens)
    }else{
      msp[[curMSp()]]$occDens <- occDens
    }
    
    # RMD VALUES ####
    # add to vector of IDs removed
    
    # PLOTS ####
    output$occDensPlot <- renderPlot({
      par(mfrow=c(1,2))
      ecospat::ecospat.plot.niche(occDens[[sp1]], title = sp1)
      ecospat::ecospat.plot.niche(occDens[[sp2]], title = sp2)
    })
    
    return(occDens)
  })
}

espace_occDens_INFO <- infoGenerator(modName = "Occurrence Density Grid", 
                                     modAuts = "Olivier Broennimann, Jamie Kass", 
                                     pkgName = c("ecospat", "adehabitatHR"))
