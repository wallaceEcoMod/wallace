

c2_removeByID <- function(occs, removeID, logs = NULL, shiny = FALSE) {
  if (is.null(occs)) {
    logs %>% writeLog(type = 'error', "Before processing occurrences, 
                      obtain the data in component 1.")
    return()
  }
  
  if (!(removeID %in% occs$occID)) {
    logs %>% writeLog(type = 'error','Entered occID not found.')
    return()
  }
  
  # find which occID corresponds to row for removal
  i <- which(removeID == occs$occID)  
  # remove the row
  occs.remID <- occs[-i,]
  print(occs.remID %>% dplyr::select(longitude, latitude, occID))
  
  logs %>% writeLog("Removed occurrence with occID = ", removeID, 
                    ". Updated data has n = ", nrow(occs.remID), " records.")
  return(occs.remID)
}