
removeByID_UI <- function(id) {
  ns <- NS(id)
  tagList(
    numericInput(ns("removeID"), label="Enter the record ID to be removed", value = 0)
  )
}

removeByID_MOD <- function(input, output, session, rvs) {
  
  reactive({
    req(rvs$occs)
    print('GO')
    if (!(input$removeID %in% rvs$occs$occID)) {
      rvs %>% writeLog(type = 'error','Entered ID not found.')
      return()
    }
    
    # find row number relating to ID
    i <- which(input$removeID == rvs$occs$occID)  # find which row name corresponds to user selection for removal
    # remove the row
    rvs$occs <- rvs$occs[-i,]
    # record all removed ids
    rvs$removedIDs <- c(rvs$removedIDs, input$removeID)
    
    rvs %>% writeLog("Removed occurrence with ID = ", input$removeID, 
                     ". Updated data has n = ", nrow(rvs$occs), " records.")
  })
}
