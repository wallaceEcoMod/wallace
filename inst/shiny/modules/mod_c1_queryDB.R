queryDb_UI <- function(id) {
  ns <- NS(id)
  tagList(
    radioButtons(ns("occsDb"), "Choose Database",
                 choices = list("GBIF" = 'gbif',
                                "VertNet" = 'vertnet',
                                "BISON" = 'bison'), inline = TRUE),
    tags$div(title='Examples: Felis catus, Canis lupus, Nyctereutes procyonoides',
             textInput(ns("spName"), label = "Enter species scientific name", placeholder = 'format: Genus species')),
    tags$div(title='Maximum number of occurrences recovered from databases. 
             Downloaded records are not sorted randomly: rows are always consistent between downloads.',
             numericInput(ns("occsNum"), "Set maximum number of occurrences", value = 100, min = 1))
  )
}

queryDb_MOD <- function(input, output, session) {
  reactive({
    # FUNCTION CALL ####
    occsTbls <- c1_queryDb(input$spName, 
                           input$occsDb, 
                           input$occsNum, 
                           logs, shiny=TRUE)
    print(occsTbls$cleaned$taxon_name)
    
    req(occsTbls)
    
    # LOAD INTO SPP ####
    occsOrig <- occsTbls$orig
    occs <- occsTbls$cleaned
    n <- formatSpName(occs$taxon_name)
    # if species name is already in list, overwrite it
    if(!is.null(spp[[n]])) spp[[n]] <- NULL
    # add two copies of occs dataset -- higher level occs will be
    # altered during session, while occData$occsCleaned is preserved in the
    # post-download cleaned state; occsOrig is the raw download
    # rmm is the range model metadata object
    spp[[n]] <- list(occs = occs, occData = list(occsOrig = occsOrig, occsCleaned = occs),
                     rmm = rangeModelMetadata::rangeModelMetadataTemplate())
    
    # METADATA ####
    spp[[n]]$rmm$data$occurrence$taxa <- n
    spp[[n]]$rmm$data$occurrence$dataType <- "presence only"
    spp[[n]]$rmm$data$occurrence$presenceSampleSize <- nrow(occs)
    spp[[n]]$rmm$data$occurrence$sources <- input$occsDb
    spp[[n]]$rmm$code$wallaceSettings$occsNum <- input$occsNum
    
    # RETURN ####
    # output the table
    return(occs)
  })
}

queryDb_INFO <- infoGenerator(modName = "Query Database (Present)",
                              modAuts = "Jamie M. Kass, Bruno Vilela, Robert P. Anderson",
                              pkgName = "spocc")

