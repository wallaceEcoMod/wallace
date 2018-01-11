
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
    pca <- cESpace_pca(curSp()[1], curSp()[2],
                       spp[[curSp()[1]]]$bgPts.z, spp[[curSp()[2]]]$bgPts.z,
                       spp[[curSp()[1]]]$occs.z, spp[[curSp()[2]]]$occs.z, 
                       logs, shiny = TRUE)
    if (is.null(pca)) return()
    
    output$pcaControls <- renderUI({
      n <- names(spp[[curSp()[1]]]$envs)
      tagList(
        # conditionalPanel(paste0("input['", "pcaSelChoice", "']"),
        #                  checkboxGroupInput("pcaSel", label = "Select",
        #                                     choices = setNames(as.list(n), n), 
        #                                     inline = TRUE, selected = n)),
        numericInput("pc1", "X-axis Component", value = 1, min = 1, max = length(n)),
        numericInput("pc2", "Y-axis Component", value = 2, min = 1, max = length(n))
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
    
    
    return(pca)
  })
}
