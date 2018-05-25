threshPred_UI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$div(title='Create binary map of predicted presence/absence assuming all values above threshold value represent presence. Also can be interpreted as a "potential distribution" (see guidance).',
             selectInput(ns('threshPred'), label = "Set threshold",
                         choices = list("No threshold" = 'noThresh',
                                        "Minimum Training Presence" = 'mtp', 
                                        "10 Percentile Training Presence" = 'p10')))
  )
}

threshPred_MOD <- function(input, output, session, predSel) {
  reactive({
    # generate binary prediction based on selected thresholding rule 
    # (same for all Maxent prediction types because they scale the same)
    r <- threshPred(occs(), results(), predSel, curModel(), input$threshPred, rmm()$output$prediction$notes, shinyLogs)
    
    # METADATA
    spp[[curSp()]]$rmm$output$prediction$thresholdRule <- input$threshPred
    
    return(r)
  })
}