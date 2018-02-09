
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

      # append all new species lists to spp
      spp[[n]] <- occsList[[n]]
    }
    
    # METADATA ####
    spp[[n]]$rmm$data$occurrence$taxa <- occs$taxon_name[1]
    spp[[n]]$rmm$data$occurrence$dataType <- "presence only"
    spp[[n]]$rmm$data$occurrence$presenceSampleSize <- nrow(occs)
    spp[[n]]$rmm$data$occurrence$sources <- "user"
    
    # RETURN ####
    return(occsList)
  })
}
