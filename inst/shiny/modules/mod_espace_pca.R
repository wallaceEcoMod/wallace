
pca_UI <- function(id) {
  ns <- NS(id)
  tagList(
    checkboxInput(ns("pcaSelChoice"), label = "Specify variables to use in analysis?"),
    uiOutput(ns("pcaControls"))
  )
}

pca_MOD <- function(input, output, session) {
  reactive({
    # FUNCTION CALL ####
    pca <- cESpace_pca(spp[[curSp()[1]]]$bgPts.z, spp[[curSp()[2]]]$bgPts.z,
                       spp[[curSp()[1]]]$occs.z, spp[[curSp()[2]]]$occs.z, 
                       logs, shiny = TRUE)
    if (is.null(pca)) return()
    
    output$pcaControls <- renderUI({
      n <- names(spp[[curSp()]]$envs)
      tagList(
        conditionalPanel(paste0("input['", "pcaSelChoice", "']"),
                         checkboxGroupInput("pcaSel", label = "Select",
                                            choices = setNames(as.list(n), n), 
                                            inline = TRUE, selected = n)),
        numericInput("pc1", "X-axis Component", value = 1, min = 1, max = length(n)),
        numericInput("pc2", "Y-axis Component", value = 2, min = 1, max = length(n))
      )
    })
    
    # PLOTS ####
    output$pcaPlot <- renderPlot({
      par(mfrow=c(2,1))
      s.class(scores[sp==0,1:2],as.factor(scores[sp==0,]$bg),col=c("blue","red"),cstar = 0, cpoint=0.1)
      s.corcircle(pca$co, lab = names(pca$tab), full = FALSE, box = TRUE)  
    })
    
    
    # LOAD INTO SPP ####
    spp[[curSp()]]$occs <- occs.rem
    
    # RMD VALUES ####
    # add to vector of IDs removed
    if(is.null(spp[[curSp()]]$rmd$c2)) {
      spp[[curSp()]]$rmd$c2 <- list(removedIDs = input$removeID)
    }else{
      spp[[curSp()]]$rmd$c2$removedIDs <- c(spp[[curSp()]]$rmd$c2$removedIDs, input$removeID)
    }
    
    return(occs.rem)
  })
}
