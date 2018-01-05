
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
    
    # load into spp superlist
    n <- formatSpName(occs$taxon_name)
    # if species name is already in list, overwrite it
    if(!is.null(spp[[n]])) spp[[n]] <- NULL
    spp[[n]] <- list(occs = occs)
    spp[[n]]$occsOrig <- occs
    
    updateSelectInput(session, "sppSel", selected = n)
    
    # RMD VALUES ####
    c1 <- list(occDb = input$occDb, occNum = input$occNum,
               timeInterval = "Present")
    if(is.null(spp[[n]]$rmd)) {
      spp[[n]]$rmd <- list(c1 = c1)  
    }else{
      spp[[n]]$rmd <- c(spp[[n]]$rmd, c1)
    }
    
    # METADATA ####
    # rmm$metadata$data$occurrence$taxaVector <- input$spName
    # rmm$metadata$data$occurrence$occurrenceDataType <- "presence only"
    # rmm$metadata$data$occurrence$presenceSampleSize <- nrow(occs)
    
    # RETURN ####
    return(occs)
  })
}
