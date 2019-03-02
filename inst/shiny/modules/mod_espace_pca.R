
pca_controlsUI <- function(id) {
  ns <- NS(id)
  tagList(
    checkboxInput(ns("pcaSelChoice"), label = "Specify variables to use in analysis?"),
    uiOutput(ns("pcaControls"))
  )
}

pca_resultsUI <- function(id) {
  ns <- NS(id)
  plotOutput(ns('pcaPlot'))
}

pca_MOD <- function(input, output, session) {
  reactive({
    # FUNCTION CALL ####
    sp1 <- curSp()[1]
    sp2 <- curSp()[2]
    
    # PROCESSING ####
    sp1.envNames <- names(envs.global[[spp[[sp1]]$envs]])
    sp2.envNames <- names(envs.global[[spp[[sp2]]$envs]])
    if(all(sp1.envNames == sp2.envNames) == FALSE) {
      shinyLogs %>% writeLog(type = "error", "Species 1 and species 2 must have the same environmental variables.")
      return()
    }
    print(sp1.envNames)
    print(head(spp[[sp1]]$bg))
    sp1.bgVals <- spp[[sp1]]$bg[sp1.envNames]
    sp2.bgVals <- spp[[sp2]]$bg[sp1.envNames]
    sp1.occsVals <- spp[[sp1]]$occs[sp1.envNames]
    sp2.occsVals <- spp[[sp2]]$occs[sp1.envNames]
    pca <- cESpace_pca(sp1, sp2, 
                       sp1.bgVals,
                       sp2.bgVals,
                       sp1.occsVals, 
                       sp2.occsVals,
                       shinyLogs)
    
    req(pca)
    
    output$pcaControls <- renderUI({
      tagList(
        # conditionalPanel(paste0("input['", "pcaSelChoice", "']"),
        #                  checkboxGroupInput("pcaSel", label = "Select",
        #                                     choices = setNames(as.list(n), n), 
        #                                     inline = TRUE, selected = n)),
        numericInput(session$ns("pc1"), "X-axis Component", value = 1, min = 1, max = length(sp1.envNames)),
        numericInput(session$ns("pc2"), "Y-axis Component", value = 2, min = 1, max = length(sp1.envNames))
      )
    })
    
    # LOAD INTO MSP ####
    
    # if(is.null(msp[[curMSp()]])) {
    #   msp[[curMSp()]] <- list(pca = pca)
    # }else{
    #   msp[[curMSp()]]$pca <- pca
    # }
    
    # RMD VALUES ####
    # add to vector of IDs removed
    
    # PLOTS ####
    output$pcaPlot <- renderPlot({
      par(mfrow=c(1,2))
      ade4::s.class(pca$scores[pca$scores$sp == 'bg',1:2], 
                    as.factor(pca$scores[pca$scores$sp == 'bg',]$bg),
                    col=c("blue","red"), cstar = 0, cpoint = 0.1)
      ade4::s.corcircle(pca$co, lab = sp1.envNames, full = FALSE, box = TRUE)  
    })
    
    return(pca)
  })
}

espace_pca_INFO <- infoGenerator(modName = "Environmental Ordination", 
                                 modAuts = "Olivier Broennimann, Jamie Kass", 
                                 pkgName = "ade4")