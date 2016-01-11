comp1_mod_userOcc <- function(csvPath) {
  source("functions.R")
  inFile <- read.csv(csvPath, header = TRUE)  # read user csv
  if (all(names(inFile) %in% c('species', 'longitude', 'latitude'))) {  # throw error if these columns are not included
    writeLog('* ERROR: Please input CSV file with columns "species", "longitude", "latitude".')
    return()
  }
  values$inFile <- inFile  # store original table in values list
  
  #     # IN DEV: make dynamic field selections for ui user-defined kfold groups
  #     output$occgrpSel <- renderUI({
  #       selectInput('occgrp', 'Occurrence Group Field', names(inFile))
  #     })
  #     output$bggrpSel <- renderUI({
  #       selectInput('bggrp', 'Background Group Field', names(inFile))
  #     })
  
  # subset to only occs, not backg, and just fields that match df
  values$spname <- inFile[1,1]  # get species name
  inFile.occs <- inFile[inFile[,1] == values$spname,]  # limit to records with this name
  for (col in c("institutionCode", "country", "stateProvince", 
                "locality", "elevation", "basisOfRecord")) {  # add all cols to match gbifoccs if not already there
    if (!(col %in% names(inFile.occs))) inFile.occs[,col] <- NA
  }
  
  inFile.occs$origID <- row.names(inFile.occs)  # add col for IDs
  inFile.occs$pop <- unlist(apply(inFile.occs, 1, popUpContent))  # add col for map marker popup text
  
  # add user locs to existing gbifoccs and df, and remove duplicate records
  values$gbifoccs <- isolate({
    values$gbifoccs <- rbind(values$gbifoccs, inFile.occs)
    values$gbifoccs <- remDups(values$gbifoccs)
  })
  values$df <- isolate({
    values$df <- rbind(values$df, inFile.occs)
    values$df <- remDups(values$df)
  })
  isolate(writeLog("* User-specified CSV input."))
  # this makes an infinite loop. not sure why...
  #     x <- paste0("User input ", input$userCSV$name, " with [", nrow(values$df), "[ records.")
  #     values$log <- paste(values$log, x, sep='<br>')
}