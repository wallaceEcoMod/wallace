

c2_removeByID <- function(occs, removeID, shinyLogs = NULL) {
  if (is.null(occs)) {
    shinyLogs %>% writeLog(type = 'error', "Before processing occurrences, 
                      obtain the data in component 1.")
    return()
  }
  
  if (!(removeID %in% occs$occID)) {
    shinyLogs %>% writeLog(type = 'error','Entered occID not found.')
    return()
  }
  
  # find which occID corresponds to row for removal
  i <- which(removeID == occs$occID)  
  # remove the row
  occs.remID <- occs[-i,]
  
  shinyLogs %>% writeLog("Removed occurrence from", em(spName(occs)), "with occID = ", removeID, 
                    ". Updated data has n = ", nrow(occs.remID), " records.")
  return(occs.remID)
}