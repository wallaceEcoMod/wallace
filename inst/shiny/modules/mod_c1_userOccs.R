
userOccs_UI <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(ns("userCSV"), label = "Upload Occurrence CSV")
  )
}

userOccs_MOD <- function(input, output, session, rvs) {
  reactive({
    # FUNCTION CALL ####
    occsList <- c1_userOccs(input$userCSV$datapath, input$userCSV$name, logs, shiny=TRUE)
    
    if (is.null(occsList)) return()
    
    # LOAD INTO SPP ####
    # if species name is already in list, overwrite it
    for(n in names(occsList)) {
      if(!is.null(spp[[n]])) {
        spp[[n]] <- NULL
      }
      # make occsOrig copy
      occsList[[n]]$occsOrig <- occsList[[n]]$occs
      # RMD VALUES ####
      dbOccsRMD <- list(spName = n, userCSV = input$userCSV) 
      occsList[[n]]$rmd <- list(c1 = dbOccsRMD)
      # append all new species lists to spp
      spp[[n]] <- occsList[[n]]
    }
    
    # # METADATA ####
    # rmm$metadata$data$occurrence$taxaVector <- occs$taxon_name[1]
    # rmm$metadata$data$occurrence$occurrenceDataType <- "presence only"
    # rmm$metadata$data$occurrence$presenceSampleSize <- nrow(occs)
    
    # RETURN ####
    return(occsList)
  })
}
