occs_queryDb_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    tags$div(title = "text",
             radioButtons(ns("occsDb"), label = "Choose Database",
                          choices = c("GBIF" = 'gbif',
                                      "VertNet" = 'vertnet',
                                      # "BISON" = 'bison',
                                      "BIEN" = 'bien'),
                          inline = TRUE)),
    tags$div(
      title = "Check to get only occurrences with uncertainty information",
      checkboxInput(ns("uncertainty"),
                    label = strong("Keep only occurrences with uncertainty values"),
                    value = FALSE)
    ),
    conditionalPanel(
      sprintf("input['%s'] == 'gbif'", ns("occsDb")),

      checkboxInput(ns("doCitations"),
                    label = 'Include Data Source Citations',
                    value = FALSE),
      conditionalPanel(
        sprintf("input['%1$s'] == 'gbif' &
                               input['%2$s'] == true",
                ns("occsDb"), ns("doCitations")),
        splitLayout(textInput(ns('gbifUser'),
                              'GBIF User ID',
                              value = NULL),
                    textInput(ns('gbifEmail'),
                              'GBIF email',
                              value = NULL),
                    passwordInput(ns('gbifPW'),
                                  'GBIF password',
                                  value = NULL))
      )
    ),
    tags$div(title = 'Examples: Felis catus, Canis lupus, Nyctereutes procyonoides',
             textInput(ns("spNames"), label = "Enter species scientific name",
                       placeholder = 'format: Genus species',
                       value = "")), # Check default
    conditionalPanel(
      sprintf(
        "(input['%1$s'] == 'gbif' & input['%2$s'] == false) | input['%1$s'] == 'vertnet'" ,
        ns("occsDb"), ns("doCitations")),
      tags$div(
        title = paste0('Maximum number of occurrences recovered from ',
                       'databases. Downloaded records are not sorted randomly: ',
                       'rows are always consistent between downloads.'),
        numericInput(ns("occsNum"), "Set maximum number of occurrences",
                     value = 0, min = 0))
    ),
    actionButton(ns("goDbOccs"), "Query Database")
  )
}

occs_queryDb_module_server <- function(input, output, session, common) {
  logger <- common$logger
  spp <- common$spp

  observeEvent(input$goDbOccs, {
    # WARNING ####
    if (input$occsDb != "bien") {
      if (input$occsDb != "gbif" & input$occsNum < 1) {
        logger %>% writeLog(type = 'warning',
                            "Enter a non-zero number of occurrences.")
        return()
      }
      if (input$occsDb == "gbif" & input$occsNum < 1 &
          input$doCitations == FALSE) {
        logger %>% writeLog(type = 'warning',
                            "Enter a non-zero number of occurrences.")
        return()
      }
    }

    # FUNCTION CALL ####
    occsList <- occs_queryDb(input$spNames, input$occsDb, input$occsNum,
                             input$doCitations, input$gbifUser, input$gbifEmail,
                             input$gbifPW, input$uncertainty, logger)
    req(occsList)

    for (sp in names(occsList)) {
      # LOAD INTO SPP ####
      # if species name is already in list, overwrite it
      if (!is.null(spp[[sp]])) spp[[sp]] <- NULL
      # add two copies of occs dataset -- higher level occs will be
      # altered during session, while occData$occsCleaned is preserved in the
      # post-download cleaned state; occsOrig is the raw download
      # rmm is the range model metadata object
      spp[[sp]] <- list(occs = occsList[[sp]]$cleaned,
                        occData = list(occsOrig = occsList[[sp]]$orig,
                                       occsCleaned = occsList[[sp]]$cleaned),
                        rmm = rangeModelMetadata::rmmTemplate(),
                        rmd = list())

      # REFERENCES ####
      if (input$occsDb == "bien") {
        knitcitations::citep(citation("BIEN"))
      } else {
        if (input$doCitations) {
          knitcitations::citep(citation("occCite"))
        } else {
          knitcitations::citep(citation("spocc"))
        }
      }

      # METADATA ####
      spp[[sp]]$rmm$data$occurrence$taxon <- sp
      spp[[sp]]$rmm$data$occurrence$dataType <- "presence only"
      spp[[sp]]$rmm$data$occurrence$presenceSampleSize <- nrow(occsList[[sp]]$cleaned)
      spp[[sp]]$rmm$data$occurrence$yearMin <- min(occsList[[sp]]$cleaned$year)
      spp[[sp]]$rmm$data$occurrence$yearMax <- max(occsList[[sp]]$cleaned$year)
      spp[[sp]]$rmm$code$wallace$occsNum <- input$occsNum
      spp[[sp]]$rmm$code$wallace$uncertainty <- input$uncertainty
      if (input$doCitations | input$occsDb == 'bien') {
        spp[[sp]]$rmm$code$wallace$occsNum <- nrow(occsList[[sp]]$orig)
      }
      spp[[sp]]$rmm$code$wallace$occsRemoved <- input$occsNum - nrow(occsList[[sp]]$cleaned)

      # Store DOI citations
      if (input$doCitations) {
        spp[[sp]]$rmm$data$occurrence$sources <- input$occsDb
        spp[[sp]]$rmm$code$wallace$gbifDOI <- occsList[[sp]]$citation
        spp[[sp]]$rmm$code$wallace$gbifUser <- input$gbifUser
        spp[[sp]]$rmm$code$wallace$gbifEmail <- input$gbifEmail
        spp[[sp]]$rmm$code$wallace$doCitations <- input$doCitations
      } else {
        spp[[sp]]$rmm$data$occurrence$sources <- input$occsDb
      }
    }

    common$update_component(tab = "Map")
  })

  return(list(
    save = function() {
      list(
        spNames = input$spNames,
        occsNum = input$occsNum
      )
    },
    load = function(state) {
      updateTextInput(session, "spNames", value = state$spNames)
      updateNumericInput(session, "occsNum", value = state$occsNum)
    }
  ))
}

occs_queryDb_module_map <- function(map, common) {
  spp <- common$spp
  curSp <- common$curSp
  occs <- spp[[curSp()]]$occData$occsCleaned
  map %>% clearAll() %>%
    addCircleMarkers(data = occs, lat = ~latitude, lng = ~longitude,
                     radius = 5, color = 'red', fill = TRUE, fillColor = "red",
                     fillOpacity = 0.2, weight = 2, popup = ~pop) %>%
    zoom2Occs(occs)
}

occs_queryDb_module_rmd <- function(species) {
  list(
    occs_queryDb_knit = species$rmm$data$occurrence$sources == 'gbif' |
      species$rmm$data$occurrence$sources == 'vernet' |
      # species$rmm$data$occurrence$sources == 'bison' |
      species$rmm$data$occurrence$sources == 'bien',
    occs_citation_knit = !is.null(species$rmm$code$wallace$gbifDOI),
    occDb_rmd = species$rmm$data$occurrence$sources,
    occNum_rmd = species$rmm$code$wallace$occsNum,
    doCitations_rmd = species$rmm$code$wallace$doCitations,
    gbifUser_rmd = species$rmm$code$wallace$gbifUser,
    gbifEmail_rmd = species$rmm$code$wallace$gbifEmail,
    uncertainty_rmd = species$rmm$code$wallace$uncertainty
  )
}
