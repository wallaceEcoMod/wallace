
queryDb_UI <- function(id) {
  ns <- NS(id)
  tagList(
    radioButtons(ns("occDb"), "Choose Database",
                 choices = list("GBIF" = 'gbif',
                                "VertNet" = 'vertnet',
                                "BISON" = 'bison'), inline = TRUE),
    tags$div(title='Examples: Felis catus, Canis lupus, Nyctereutes procyonoides',
             textInput(ns("spName"), label = "Enter species scientific name", placeholder = 'format: Genus species')),
    tags$div(title='Maximum number of occurrences recovered from databases. 
             Downloaded records are not sorted randomly: rows are always consistent between downloads.',
             sliderInput(ns("occNum"), "Set maximum number of occurrences", min = 1, max = 500, value = 100))
  )
}

queryDb_MOD <- function(input, output, session) {
  reactive({
    # FUNCTION CALL ####
    occsTbl <- c1_queryDb(input$spName, input$occDb, input$occNum, logs, shiny=TRUE)
    
    if (is.null(occsTbl)) return()
    
    # LOAD INTO SPP ####
    n <- formatSpName(occsTbl$taxon_name)
    # if species name is already in list, overwrite it
    if(!is.null(spp[[n]])) spp[[n]] <- NULL
    # add two copies of occs dataset -- "occs" will be altered during session,
    # while "occsOrig" will be preserved in this state
    # rmm is the range model metadata object
    spp[[n]] <- list(occs = occsTbl, occData = list(occsOrig = occsTbl),
                     rmm = rangeModelMetadata::rangeModelMetadataTemplate())
    
    # METADATA ####
    spp[[n]]$rmm$data$occurrence$taxa <- n
    spp[[n]]$rmm$data$occurrence$dataType <- "presence only"
    spp[[n]]$rmm$data$occurrence$presenceSampleSize <- nrow(occsTbl)
    spp[[n]]$rmm$data$occurrence$sources <- input$occDb
    spp[[n]]$rmm$code$wallaceSettings$occNum <- input$occNum
    
    # RETURN ####
    # output the table
    return(occsTbl)
  })
}
