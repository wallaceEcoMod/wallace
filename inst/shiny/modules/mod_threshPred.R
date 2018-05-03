threshPred_UI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$div(title='Create binary map of predicted presence/absence assuming all values above threshold value represent presence. Also can be interpreted as a "potential distribution" (see guidance).',
             selectInput(ns('predThresh'), label = "Set threshold",
                         choices = list("No threshold" = 'noThresh',
                                        "Minimum Training Presence" = 'mtp', 
                                        "10 Percentile Training Presence" = 'p10')))
  )
}

threshPred_MOD <- function(input, output, session) {
  reactive({
    # generate binary prediction based on selected thresholding rule 
    # (same for all Maxent prediction types because they scale the same)
    threshPred(spp[[curSp()]]$occs, 
               spp[[curSp()]]$results, 
               curModel(), 
               input$predThresh, 
               curPredType(),
               shinyLogs)
  })
}