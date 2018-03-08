c2_thinOccs <- function(occs, thinDist, logs=NULL, shiny=FALSE) {
  if (is.null(occs)) {
    logs %>% writeLog(type = 'error', "Before processing occurrences, 
                      obtain the data in component 1.")
    return()
  }
  
  if (thinDist <= 0) {
    logs %>% writeLog(type = "error", 'Assign positive distance to thinning parameter.')
    return()
  }
  
  n <- occs$taxon_name[1]
  
  # query database
  if (shiny == TRUE) {
    withProgress(message = paste0("Spatially thinning for ", n, "..."), {  # start progress bar
      output <- spThin::thin(occs, 'latitude', 'longitude', 'taxon_name', thin.par = thinDist,
                             reps = 100, locs.thinned.list.return = TRUE, write.files = FALSE,
                             verbose = FALSE)
      
      # pull thinned dataset with max records, not just the first in the list
      maxThin <- which(sapply(output, nrow) == max(sapply(output, nrow)))
      maxThin <- output[[ifelse(length(maxThin) > 1, maxThin[1], maxThin)]]  # if more than one max, pick first
      occs.thin <- occs[as.numeric(rownames(maxThin)),]
      # if (!is.null(values$inFile)) {
      #   thinned.inFile <- values$inFile[as.numeric(rownames(output[[1]])),]
      # }
    })  
  } else {
    output <- spThin::thin(occs, 'latitude', 'longitude', 'name', thin.par = thinDist,
                           reps = 100, locs.thinned.list.return = TRUE, write.files = FALSE,
                           verbose = FALSE)
    maxThin <- which(sapply(output, nrow) == max(sapply(output, nrow)))
    maxThin <- output[[ifelse(length(maxThin) > 1, maxThin[1], maxThin)]]
    occs.thin <- occs[as.numeric(rownames(maxThin)),]
  }
  
  logs %>% writeLog('Total records for', n, 'thinned to [', nrow(occs.thin), '] localities.')
  
  return(occs.thin)
}
  