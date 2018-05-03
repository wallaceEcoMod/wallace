
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
    # FUNCTION CALL ####
    
    sp1 <- curSp()[1]
    sp2 <- curSp()[2]
    z1 <- msp[[curMSp()]]$occDens[[sp1]]
    z2 <- msp[[curMSp()]]$occDens[[sp2]]
    nicheOv <- cESpace_nicheOv(z1, z2, shinyLogs)
    if (is.null(nicheOv)) return()
    
    # LOAD INTO MSP ####
    if(is.null(msp[[curMSp()]])) {
      msp[[curMSp()]] <- list(nicheOv = nicheOv)
    }else{
      msp[[curMSp()]]$nicheOv <- nicheOv
    }
    
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
      
      ecospat::ecospat.plot.niche.dyn(z1, z2, 0.5, title = curMSp(), colz1 = "blue", colz2 = "red", 
                                      colinter = "purple", colZ1="blue", colZ2="red")
      if(!is.null(nicheOv$equiv)) ecospat::ecospat.plot.overlap.test(nicheOv$equiv, "D", "Equivalency test")
      if(!is.null(nicheOv$simil)) ecospat::ecospat.plot.overlap.test(nicheOv$simil, "D", "Similarity test")
    })
    
    return(nicheOv)
  })
}

espace_nicheOv_INFO <- infoGenerator(modName = "Niche Overlap", modAuts = "Olivier Broennimann, Jamie Kass", pkgName = "ecospat")