
thinOccs_UI <- function(id) {
  ns <- NS(id)
  tagList(
    numericInput(ns("thinDist"), label = "Thinning distance (km)", value = 0),
    shinyBS::bsPopover(ns('thinDist'), title = 'Tip',
                       'The minimum distance between occurrence locations 
                       (nearest neighbor distance) in km for resulting thinned 
                       dataset. Ideally based on species biology 
                       (e.g., home-range size).',
                       placement = 'right', options = list(container = "body"))
  )
}

thinOccs_MOD <- function(input, output, session, logs, occs) {

  doThin <- reactive({
    if (input$thinDist <= 0) {
      logs %>% writeLog('<font color="orange"><b>! WARNING</b></font> : Assign positive distance to thinning parameter.')
      return()
    }
    
    withProgress(message = "Spatially Thinning Localities...", {  # start progress bar
      output <- spThin::thin(occs(), 'latitude', 'longitude', 'name', thin.par = input$thinDist,
                             reps = 100, locs.thinned.list.return = TRUE, write.files = FALSE,
                             verbose = FALSE)
      
      # pull thinned dataset with max records, not just the first in the list
      maxThin <- which(sapply(output, nrow) == max(sapply(output, nrow)))
      maxThin <- output[[ifelse(length(maxThin) > 1, maxThin[1], maxThin)]]  # if more than one max, pick first
      occs.thin <- occs()[as.numeric(rownames(maxThin)),]
      # if (!is.null(values$inFile)) {
      #   thinned.inFile <- values$inFile[as.numeric(rownames(output[[1]])),]
      # }
    })
    
    logs %>% writeLog('> Total records thinned to [', nrow(occs.thin), '] localities.')
    
    occs(occs.thin)
    return(occs.thin)
  })

  return(doThin)
}