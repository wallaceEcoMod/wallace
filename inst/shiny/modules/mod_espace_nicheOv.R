
nicheOv_controlsUI <- function(id) {
  ns <- NS(id)
  tagList(
  )
}

nicheOv_resultsUI <- function(id) {
  ns <- NS(id)
  tagList(
    htmlOutput(ns("nicheOvText")),br(),br(),
    plotOutput(ns("nicheOvPlot"))
  )
}

nicheOv_MOD <- function(input, output, session) {
  reactive({
    # ERRORS ####
    if(length(curSp()) != 2) {
      shinyLogs %>% writeLog(type = "error", "Please select two species to run the niche overlap module.")
      return()
    }
    mspName <- paste(curSp(), collapse = "|")
    if(is.null(spp[[mspName]])) {
      shinyLogs %>% writeLog(type = "error", "Please run PCA and occurrence density with two species before running the niche overlap module.")
      return()
    }
    # if a multispecies analysis has been run, but not occDens
    if(is.null(spp[[mspName]]$occDens)) {
      shinyLogs %>% writeLog(type = "error", "Please run occurrence density with two species before running the niche overlap module.")
      return()
    }
    # FUNCTION CALL ####
    
    sp1 <- curSp()[1]
    sp2 <- curSp()[2]
    z1 <- spp[[mspName]]$occDens[[sp1]]
    z2 <- spp[[mspName]]$occDens[[sp2]]
    nicheOv <- cESpace_nicheOv(z1, z2, shinyLogs = shinyLogs)
    if (is.null(nicheOv)) return()
    
    # LOAD INTO MSP ####
    spp[[mspName]]$nicheOv <- nicheOv
    
    # RMD VALUES ####
    # add to vector of IDs removed
    
    output$nicheOvText <- renderUI({
      HTML(paste("Overlap D = ", round(nicheOv$overlap$D, 2),"\n",
                 "Sp1 only :", round(nicheOv$USE[3], 2),
                 " | Sp2 only :",round(nicheOv$USE[1], 2),
                 " | Both :", round(nicheOv$USE[2], 2)))
    })
    
    output$nicheOvPlot <- renderPlot({
      #plots
      layout(matrix(c(1,1,1,1,1,1,1,1,2,2,3,3), 4, 3, byrow = F))
      #layout.show(3)
      
      ecospat::ecospat.plot.niche.dyn(z1, z2, 0.5, title = mspName, colz1 = "blue", colz2 = "red", 
                                      colinter = "purple", colZ1="blue", colZ2="red")
      if(!is.null(nicheOv$equiv)) ecospat::ecospat.plot.overlap.test(nicheOv$equiv, "D", "Equivalency test")
      if(!is.null(nicheOv$simil)) ecospat::ecospat.plot.overlap.test(nicheOv$simil, "D", "Similarity test")
    })
    
    return(nicheOv)
  })
}

espace_nicheOv_INFO <- infoGenerator(modName = "Niche Overlap", modAuts = "Olivier Broennimann, Jamie Kass", pkgName = "ecospat")