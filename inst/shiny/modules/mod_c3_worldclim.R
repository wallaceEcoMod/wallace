
wcBioclims_UI <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("bcRes"), label = "Select WorldClim bioclimatic variable resolution",
                choices = list("Select resolution" = "",
                               "30 arcsec" = 0.5,
                               "2.5 arcmin" = 2.5,
                               "5 arcmin" = 5,
                               "10 arcmin" = 10)),
    shinyBS::bsPopover(ns('bcRes'), title = 'Tip',
                       'Approximate lengths at equator: 10 arcmin = ~20 km, 5 arcmin = ~10 km, 2.5 arcmin = ~5 km, 30 arcsec = ~1 km. Exact length varies based on latitudinal position.',
                       placement = 'right', options = list(container = "body"))
  )
}

wcBioclims <- function(input, output, session, logs, occs) {

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