
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
    # ERRORS ####
    if(length(curSp()) != 2) {
      shinyLogs %>% writeLog(type = "error", "Please select two species to run the occurrence density grid module.")
      return()
    }
    # if no multispecies analysis has been run yet
    mspName <- paste(curSp(), collapse = ".")
    if(is.null(spp[[mspName]])) {
      shinyLogs %>% writeLog(type = "error", "Please run PCA with two species before running the occurrence density grid module.")
      return()
    }
    # if a multispecies analysis has been run, but not PCA
    if(is.null(spp[[mspName]]$pca)) {
      shinyLogs %>% writeLog(type = "error", "Please run PCA with two species before running the occurrence density grid module.")
      return()
    }
    # FUNCTION CALL ####
    sp1 <- curSp()[1]
    sp2 <- curSp()[2]
    occDens <- cESpace_occDens(sp1, sp2, spp[[mspName]]$pca, shinyLogs)
    req(occDens)
    
    # LOAD INTO MSP ####
    spp[[mspName]]$occDens <- occDens
    
    # RMD VALUES ####
    # add to vector of IDs removed
    
    # PLOTS ####
    output$occDensPlot <- renderPlot({
      par(mfrow=c(1,2))
      ecospat::ecospat.plot.niche(occDens[[sp1]], title = spName(sp1))
      ecospat::ecospat.plot.niche(occDens[[sp2]], title = spName(sp2))
    })
    
    return(occDens)
  })
}

espace_occDens_INFO <- infoGenerator(modName = "Occurrence Density Grid", 
                                     modAuts = "Olivier Broennimann, Jamie Kass", 
                                     pkgName = c("ecospat", "adehabitatHR"))
