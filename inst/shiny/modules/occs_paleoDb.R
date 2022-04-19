occs_paleoDb_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    shinyWidgets::pickerInput(
      ns("timeInterval"),
      label = "Select interval",
      choices = setNames(as.list(c('Quaternary', 'Holocene', 'Pleistocene',
                                   'Late Pleistocene', 'Middle Pleistocene',
                                   'Calabrian', 'Gelasian')),
                         c('Quaternary [0 - 2.588]',
                           '-- Holocene [0 - 0.0117]',
                           '-- Pleistocene [0.0117 - 2.588]',
                           '---- Late Pleistocene [0.0117 - 0.126]',
                           '---- Middle Pleistocene [0.126 - 0.781]',
                           '---- Calabrian [0.781 - 1.806]',
                           '---- Gelasian [1.806 - 2.588]')),
      multiple = FALSE),
    tags$div(title = 'Examples: Canis lupus, Crocuta crocuta',
             textInput(ns("spNamePB"), label = "Enter species scientific name",
                       placeholder = 'format: Genus species')),
    tags$div(title = paste0('Maximum number of occurrences recovered from',
                            ' databases. Downloaded records are not sorted',
                            ' randomly: rows are always consistent between',
                            ' downloads.'),
             numericInput(ns("occsNumPB"), "Set maximum number of occurrences",
                          value = 0, min = 0, max = 500)),
    actionButton(ns("goPaleoDbOccs"), "Query Database")
  )
}

occs_paleoDb_module_server <- function(input, output, session, common) {
  logger <- common$logger
  spp <- common$spp

  observeEvent(input$goPaleoDbOccs, {
    # WARNING ####
    if (input$occsNumPB < 1) {
      logger %>% writeLog(type = 'warning', "Enter a non-zero number of ocurrences.")
      return()
    }

    # FUNCTION CALL ####
    occsTbls <- occs_paleoDb(input$spNamePB, input$occsNumPB, input$timeInterval,
                             logger)

    req(occsTbls)

    # LOAD INTO SPP ####
    occsOrig <- occsTbls$orig
    occs <- occsTbls$cleaned
    sp <- fmtSpN(input$spNamePB)
    sp <- paste0(toupper(substring(sp, 1, 1)), substring(sp, 2, nchar(sp)))
    # if species name is already in list, overwrite it
    if (!is.null(spp[[sp]])) spp[[sp]] <- NULL
    # add two copies of occs dataset -- "occs" will be altered during session,
    # while "occsOrig" will be preserved in this state
    # rmm is the range model metadata object
    spp[[sp]] <- list(occs = occs,
                      occData = list(occsOrig = occsOrig,
                                     occsCleaned = occs),
                      rmm = rangeModelMetadata::rmmTemplate(),
                      rmd = list())

    # REFERENCES ####
    knitcitations::citep(citation("paleobioDB"))

    # METADATA ####
    spp[[sp]]$rmm$data$occurrence$taxon <- sp
    spp[[sp]]$rmm$data$occurrence$dataType <- "presence only"
    spp[[sp]]$rmm$data$occurrence$presenceSampleSize <- nrow(occs)
    spp[[sp]]$rmm$data$occurrence$yearMin <- paste(min(occs$late_age), "mya")
    spp[[sp]]$rmm$data$occurrence$yearMax <- paste(max(occs$early_age), "mya")
    spp[[sp]]$rmm$code$wallace$occsNum <- input$occsNumPB
    spp[[sp]]$rmm$code$wallace$occsRemoved <- nrow(occsOrig) - nrow(occs)
    spp[[sp]]$rmm$data$occurrence$sources <- "paleobioDb"
    spp[[sp]]$rmm$code$wallace$timeInterval <- input$timeInterval

    common$update_component(tab = "Map")
  })

  return(list(
    save = function() {
      list(
        spNamePB = input$spNamePB,
        occsNumPB = input$occsNumPB,
        timeInterval = input$timeInterval
      )
    },
    load = function(state) {
      updateTextInput(session, "spNamePB", value = state$spNamePB)
      updateNumericInput(session, "occsNumPB", value = state$occsNumPB)
      shinyWidgets::updatePickerInput(session, "timeInterval",
                                      selected = input$timeInterval)
    }
  ))

}

occs_paleoDb_module_map <- function(map, common) {
  spp <- common$spp
  curSp <- common$curSp
  occs <- spp[[curSp()]]$occData$occsCleaned
  map %>% clearAll() %>%
    addCircleMarkers(data = occs, lat = ~latitude, lng = ~longitude,
                     radius = 5, color = 'red', fill = TRUE, fillColor = "red",
                     fillOpacity = 0.2, weight = 2, popup = ~pop) %>%
    zoom2Occs(occs)
}

occs_paleoDb_module_rmd <- function(species) {
  # Variables used in the module's Rmd code
  list(
    occs_paleoDb_knit = species$rmm$data$occurrence$sources == "paleobioDb",
    occsNumPB_rmd = species$rmm$code$wallace$occsNum,
    timeInterval_rmd = species$rmm$code$wallace$timeInterval
  )
}

