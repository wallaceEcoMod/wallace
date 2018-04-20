
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
    envNames <- names(spp[[curSp()]]$envs)
    sp1.bgEnvsVals <- spp[[sp1]]$bg %>% select(contains())
    sp2.bgEnvsVals <- spp[[sp2]]$bg %>% select(contains(envNames))
    sp1.occsEnvsVals <- spp[[sp1]]$occs %>% select(contains(envNames))
    sp2.occsEnvsVals <- spp[[sp2]]$occs %>% select(contains(envNames))
    pca <- cESpace_pca(sp1, sp2, 
                       sp1.bgEnvsVals,
                       sp2.bgEnvsVals,
                       sp1.occsEnvsVals, 
                       sp2.occsEnvsVals,
                       logs, shiny = TRUE)
    if (is.null(pca)) return()
    
    output$pcaControls <- renderUI({
      n <- names(spp[[sp1]]$envs)
      tagList(
        # conditionalPanel(paste0("input['", "pcaSelChoice", "']"),
        #                  checkboxGroupInput("pcaSel", label = "Select",
        #                                     choices = setNames(as.list(n), n), 
        #                                     inline = TRUE, selected = n)),
        numericInput(session$ns("pc1"), "X-axis Component", value = 1, min = 1, max = length(n)),
        numericInput(session$ns("pc2"), "Y-axis Component", value = 2, min = 1, max = length(n))
      )
    })
    
    # LOAD INTO MSP ####
    
    if(is.null(msp[[curMSp()]])) {
      msp[[curMSp()]] <- list(pca = pca)
    }else{
      msp[[curMSp()]]$pca <- pca
    }
    
    # RMD VALUES ####
    # add to vector of IDs removed
    
    # PLOTS ####
    output$pcaPlot <- renderPlot({
      par(mfrow=c(1,2))
      ade4::s.class(pca$scores[pca$scores$sp == 'bg',1:2], 
                    as.factor(pca$scores[pca$scores$sp == 'bg',]$bg),
                    col=c("blue","red"), cstar = 0, cpoint = 0.1)
      ade4::s.corcircle(pca$co, lab = names(spp[[sp1]]$envs), full = FALSE, box = TRUE)  
    })
    
    return(pca)
  })
}

espace_pca_INFO <- infoGenerator(modName = "Environmental Ordination", 
                                 modAuts = "Olivier Broennimann, Jamie Kass", 
                                 pkgName = "ade4")