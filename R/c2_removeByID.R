

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
  
  logs %>% writeLog("Removed occurrence from", occs$taxon_name[1], "with occID = ", removeID, 
                    ". Updated data has n = ", nrow(occs.remID), " records.")
  return(occs.remID)
}