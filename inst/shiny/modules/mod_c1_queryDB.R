queryDb_UI <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    tags$div(title = "text",
             radioButtons(ns("occsDb"), label = "Choose Database",
                          choices = c("GBIF" = 'gbif', 
                                      "VertNet" = 'vertnet', 
                                      "BISON" = 'bison',
                                      "BIEN" = 'bien'), 
                          inline = TRUE)),
    conditionalPanel(sprintf("input['%s'] == 'gbif'", ns("occsDb")),
                     checkboxInput(ns("doCitations"), 
                                   label = 'Include Data Source Citations', 
                                   value = FALSE),
                     conditionalPanel(sprintf("input['%1$s'] == 'gbif' & 
                                              input['%2$s'] == true",
                                              ns("occsDb"), ns("doCitations")),
                                      splitLayout(textInput(ns('gbifUser'),
                                                            'GBIF User ID',
                                                            value=NULL),
                                                  textInput(ns('gbifEmail'),
                                                            'GBIF email',
                                                            value=NULL),
                                                  textInput(ns('gbifPW'),
                                                            'GBIF password',
                                                            value=NULL)))),
    tags$div(title = 'Examples: Felis catus, Canis lupus, Nyctereutes procyonoides',
             textInput(ns("spNames"), label = "Enter species scientific name", 
                       placeholder = 'format: Genus species', value="meles meles, martes martes")),
    tags$div(title = 'Maximum number of occurrences recovered from databases. 
             Downloaded records are not sorted randomly: 
             rows are always consistent between downloads.',
             numericInput(ns("occsNum"), "Set maximum number of occurrences", 
                          value = 100, min = 0))
  )
}

queryDb_MOD <- function(input, output, session, targetGroup = FALSE) {
  reactive({
    # WARNING ####
    if (input$occsNum < 1) {
      shinyLogs %>% writeLog(type = 'warning', "Enter a non-zero number of occurrences.")
      return()
    }
    
    # FUNCTION CALL ####
    occsList <- c1_queryDb(input$spNames, input$occsDb, input$occsNum, 
                           input$doCitations, input$gbifUser, input$gbifEmail,
                           input$gbifPW, shinyLogs)
    req(occsList)
    
    if(targetGroup == TRUE) {
      return(occsList)
    }
    else{
      for(sp in names(occsList)) {
        # LOAD INTO SPP ####
        # if species name is already in list, overwrite it
        if(!is.null(spp[[sp]])) spp[[sp]] <- NULL
        # add two copies of occs dataset -- higher level occs will be
        # altered during session, while occData$occsCleaned is preserved in the
        # post-download cleaned state; occsOrig is the raw download
        # rmm is the range model metadata object
        spp[[sp]] <- list(occs = occsList[[sp]]$cleaned, 
                          occData = list(occsOrig = occsList[[sp]]$orig, 
                                         occsCleaned = occsList[[sp]]$cleaned),
                          rmm = rangeModelMetadata::rmmTemplate())  
        
        # METADATA ####
        spp[[sp]]$rmm$data$occurrence$taxa <- sp
        spp[[sp]]$rmm$data$occurrence$dataType <- "presence only"
        spp[[sp]]$rmm$data$occurrence$presenceSampleSize <- nrow(occs)
        spp[[sp]]$rmm$code$wallaceSettings$occsNum <- input$occsNum
        spp[[sp]]$rmm$code$wallaceSettings$occsRemoved <- input$occsNum - nrow(occsList[[sp]]$cleaned)
        # store citations with occCite, or just report the database if users are 
        # too lame to use bridgetree
        if(input$doCitations){
          # DOUBLE CHECK THIS DOESN"T NEED TO BE VECTORIZED!!
          spp[[sp]]$rmm$data$occurrence$sources <- occsList[[sp]]$citations
        } else {
          spp[[sp]]$rmm$data$occurrence$sources <- input$occsDb
        }  
      }
      return(occsList)
    }
    
    
    # RETURN ####
    # output the table
    # return(occsList[[sp]]$cleaned)
  })
}

queryDb_MAP <- function(map, session) {
  occs <- spp[[curSp()]]$occData$occsCleaned
  map %>% clearAll() %>%
    addCircleMarkers(data = occs, lat = ~latitude, lng = ~longitude, 
                     radius = 5, color = 'red', fill = TRUE, fillColor = "red", 
                     fillOpacity = 0.2, weight = 2, popup = ~pop) %>%
    zoom2Occs(occs)
}

queryDb_INFO <- infoGenerator(modName = "Query Database (Present)",
                              modAuts = "Jamie M. Kass, Bruno Vilela, Gonzalo E. 
                                Pinilla-Buitrago, Hannah Owens, Cory Merow, Robert P.
                                Anderson",
                              pkgName = "spocc")

queryDb_RMD <- function(sp) {
  list(occDb = spp[[sp]]$rmm$data$occurrence$sources,
       occNum = spp[[sp]]$rmm$code$wallaceSettings$occsNum)
}
  
