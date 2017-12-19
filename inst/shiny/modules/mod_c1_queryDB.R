
queryDb_UI <- function(id) {
  ns <- NS(id)
  tagList(
    radioButtons(ns("occDb"), "Choose Database",
                 choices = list("GBIF" = 'gbif',
                                "VertNet" = 'vertnet',
                                "BISON" = 'bison'), inline = TRUE),
    tags$div(title='Examples: Felis catus, Canis lupus, Nyctereutes procyonoides',
             textInput(ns("spName"), label = "Enter species scientific name", placeholder = 'format: Genus species')),
    tags$div(title='Maximum number of occurrences recovered from databases. Downloaded records are not sorted randomly: rows are always consistent between downloads.',
             sliderInput(ns("occNum"), "Set maximum number of occurrences", min = 1, max = 500, value = 100))
  )
}

queryDb_MOD <- function(input, output, session) {
  reactive({
    # FUNCTION CALL ####
    occs <- c1_queryDb(input$spName, input$occDb, input$occNum, logs, shiny=TRUE)
    
    if (is.null(occs)) return()
    
    # RMD VALUES ####
    rmd$c1$occDb <- input$occDb
    rmd$c1$spName <- input$spName
    rmd$c1$occNum <- input$occNum
    rmd$c1$timeInterval<- "Present"
    
    # METADATA ####
    rmm$metadata$data$occurrence$taxaVector <- input$spName
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
