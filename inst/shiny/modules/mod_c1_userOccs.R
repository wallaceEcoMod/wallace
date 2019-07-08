
userOccs_UI <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(ns("userCSV"), label = "Upload Occurrence CSV")
  )
}

userOccs_MOD <- function(input, output, session) {
  reactive({
    # FUNCTION CALL ####
    occsList <- c1_userOccs(input$userCSV$datapath, input$userCSV$name, shinyLogs)
    
    if (is.null(occsList)) return()
    
    # LOAD INTO SPP ####
    # if species name is already in list, overwrite it
    for(sp in names(occsList)) {
      occs <- occsList[[sp]]$cleaned
      occsOrig <- occsList[[sp]]$orig
      if(!is.null(spp[[sp]])) spp[[sp]] <- NULL
      spp[[sp]] <- list(occs = occs, 
                       occData = list(occsOrig = occsOrig, occsCleaned = occs),
                       rmm = rangeModelMetadata::rmmTemplate())
      if(!is.null(occsList[[sp]]$bg)) spp[[sp]]$bg <- occsList[[sp]]$bg
      
      # METADATA ####
      spp[[sp]]$rmm$data$occurrence$taxa <- occs$scientific_name[1]
      spp[[sp]]$rmm$data$occurrence$dataType <- "presence only"
      spp[[sp]]$rmm$data$occurrence$presenceSampleSize <- nrow(occs)
      spp[[sp]]$rmm$data$occurrence$sources <- "user"
      spp[[sp]]$rmm$code$wallaceSettings$userCSV <- input$userCSV$name
    }
    # RETURN ####
    return(occsList)
  })
}

userOccs_MAP <- function(map, session) {
  occs <- spp[[curSp()]]$occData$occsCleaned
  map %>% clearAll() %>%
    addCircleMarkers(data = occs, lat = ~latitude, lng = ~longitude, 
                     radius = 5, color = 'red', fill = TRUE, fillColor = "red", 
                     fillOpacity = 0.2, weight = 2, popup = ~pop) %>%
    zoom2Occs(occs)
}

userOccs_INFO <- infoGenerator(modName = "User-specified Occurrences",
                              modAuts = "Jamie M. Kass, Bruno Vilela, Gonzalo E. 
                                        Pinilla-Buitrago, Robert P. Anderson",
                              pkgName = NULL)

userOccs_RMD <- function(sp) {
  list(userOccsCsvName = spp[[sp]]$rmm$code$wallaceSettings$userCSV)
}
