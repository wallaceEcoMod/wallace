
userOccs_UI <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(ns("userCSV"), label = "Upload Occurrence CSV")
  )
}

userOccs_MOD <- function(input, output, session) {
  reactive({
    # FUNCTION CALL ####
    occsList <- c1_userOccs(input$userCSV$datapath, input$userCSV$name, logs)
    
    if (is.null(occsList)) return()
    
    # LOAD INTO SPP ####
    # if species name is already in list, overwrite it
    for(n in names(occsList)) {
      occs <- occsList[[n]]$occs
      if(!is.null(spp[[n]])) spp[[n]] <- NULL
      spp[[n]] <- list(occs = occs, occData = list(occsCleaned = occs),
                       rmm = rangeModelMetadata::rangeModelMetadataTemplate())
      if(!is.null(occsList[[n]]$bg)) spp[[n]]$bg <- occsList[[n]]$bg
      
      # METADATA ####
      spp[[n]]$rmm$data$occurrence$taxa <- occs$taxon_name[1]
      spp[[n]]$rmm$data$occurrence$dataType <- "presence only"
      spp[[n]]$rmm$data$occurrence$presenceSampleSize <- nrow(occs)
      spp[[n]]$rmm$data$occurrence$sources <- "user"
      spp[[n]]$rmm$code$wallaceSettings$userCSV <- input$userCSV$name
    }
    # RETURN ####
    return(occsList)
  })
}

userOccs_INFO <- infoGenerator(modName = "User-specified Occurrences",
                              modAuts = "Jamie M. Kass, Bruno Vilela, Robert P. Anderson",
                              pkgName = NULL)
