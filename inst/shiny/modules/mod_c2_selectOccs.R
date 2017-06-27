


remSelLocs <- function(remLocID) {
  isolate({
    if (!(remLocID %in% values$df$origID)) {
      writeLog('<font color="red"><b>! ERROR</b></font> : Entered ID not found.')
      return()
    }
    
    # find row number relating to ID
    remo <- which(remLocID == values$df$origID)  # find which row name corresponds to user selection for removal
    # Remove the offending row
    values$removed <- rbind(values$removed, values$df[remo, ])
    values$removedAll <- c(values$removedAll, values$df[remo,]$origID)  # keep vector of all removed pts
    values$df <- values$df[-remo, ]
    values$origOccs <- values$origOccs[-remo, ]
    
    writeLog(paste0("> Removed locality with ID = ", remLocID, ". Localities data has n = ", nrow(values$df), " records."))
    
    proxy %>% 
      clearMarkers() %>% 
      map_plotLocs(values$origOccs) %>%
      zoom2Occs(values$origOccs)
  })
}