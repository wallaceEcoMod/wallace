
userOccs_UI <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(ns("userCSV"), label = "Upload Occurrence CSV")
  )
}

userOccs_MOD <- function(input, output, session, rvs) {
  reactive({
    # FUNCTION CALL ####
    occs <- c1_userOccs(input$userCSV$datapath, input$userCSV$name, logs, shiny=TRUE)
    
    if (is.null(occs)) return()
    
    # RMD VALUES ####
    rmd$c1$userCSV <- input$userCSV
    rmd$c1$spName <- occs$taxon_name[1]
    # rmd$c1$timeInterval<- "Present"
    
    # METADATA ####
    rmm$metadata$data$occurrence$taxaVector <- occs$taxon_name[1]
    rmm$metadata$data$occurrence$occurrenceDataType <- "presence only"
    rmm$metadata$data$occurrence$presenceSampleSize <- nrow(occs)
    
    # MAPPING ####
    map %>%
      clearMarkers() %>%
      clearShapes() %>%
      clearImages() %>%
      addCircleMarkers(data = occs, lat = ~latitude, lng = ~longitude, radius = 5, 
                       color = 'red', fill = TRUE, fillColor = 'red', 
                       fillOpacity = 0.2, weight = 2, popup = ~pop) %>%
      zoom2Occs(occs)
    
    # RETURN ####
    return(occs)
  })
}
