rep_biomodelos_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    uiOutput(ns("bioSpUI")),
    passwordInput(ns("keyPost"), label = "Enter API Key", value = ""),
    textInput(ns("userBio"), "User biomodelos e-mail", value = NULL),
    selectInput(ns("selLicense"),
                label = "CC License",
                choices = list("Select License" = "",
                               "by" = "by",
                               "by-sa" = "by-sa",
                               "by-nc" = "by-nc",
                               "by-nc-sa" = "by-nc-sa",
                               "cc-zero" = "cc-zero")),
    checkboxInput(ns("atlas"), "Atlas agreement", value = TRUE),
    actionButton(ns("pushBiomod"), "Push Payload (**)")
  )
}

rep_biomodelos_module_server <- function(input, output, session, common) {

  spp <- common$spp
  bioSp <- common$bioSp

  output$bioSpUI <- renderUI({
    # check that a species is in the list already -- if not, don't proceed
    req(length(reactiveValuesToList(spp)) > 0)
    # get the species names
    n <- names(spp)[order(names(spp))]
    # remove multispecies names from list
    n <- n[!grepl(".", n, fixed = TRUE)]
    sppNameList <- c(list("Current species" = ""), setNames(as.list(n), n))
    # generate a selectInput ui that lists the available species
    selectizeInput('bioSp', label = "Select species" , choices = sppNameList,
                   multiple = TRUE, selected = bioSp(), options = list(maxItems = 1))
  })

  observeEvent(input$pushBiomod, {
    if (spp[[bioSp()]]$rmm$data$occurrence$sources != "Biomodelos") {
      shinyalert::shinyalert(
        "You must submit a model built with occurrences from biomodelos (**)",
        type = "error")
      return()
    }
    if (is.null(spp[[bioSp()]]$biomodelos$prediction)) {
      shinyalert::shinyalert(
        "You need a map prediction before pushing to biomodelos (**).",
        type = "error")
      return()
    }

    URL <- 'https://api-biomodelos.humboldt.org.co/v2/models'


    manifest <- list(
      edited_occurrences = paste0(bioSp(), '_processed_occs.csv'),
      raster_model_prediction = paste0(bioSp(), '_pred.tif'),
      # raster_model_continuous = paste0(bioSp(), '_cloglog.tif'),
      raster_model_threshold = paste0(bioSp(), '_thr.csv'),
      shape_file_mask = paste0(bioSp(), '_projectionExtentShp.zip'),
      model_metadata = paste0(bioSp(), '_metadata.csv'),
      wallace_session = paste0(bioSp(), '_session.Rmd')
    )

    ### Create files
    tmpdir <- tempdir()
    # Create occs
    # add req occs
    tmpOccs <- file.path(tmpdir, paste0(bioSp(), '_processed_occs.csv'))
    write.csv(spp[[bioSp()]]$occs, tmpOccs, row.names = FALSE)

    # Create mapPrediction
    # add req pred
    tmpPred <- file.path(tmpdir, paste0(bioSp(), '_pred.tif'))
    raster::writeRaster(spp[[bioSp()]]$biomodelos$prediction, tmpPred,
                        overwrite = TRUE)

    # Create thr
    # add req occs
    tmpThrs <- file.path(tmpdir, paste0(bioSp(), '_thr.csv'))
    write.csv(spp[[bioSp()]]$biomodelos$thrs, tmpThrs, row.names = FALSE)

    # Create extent shapefile
    # add req ext
    tmpExt <- file.path(tmpdir, paste0(bioSp(), '_projectionExtentShp.zip'))
    rgdal::writeOGR(obj = spp[[bioSp()]]$procEnvs$bgExt,
                    dsn = tmpdir,
                    layer = paste0(bioSp(), '_bgShp'),
                    driver = "ESRI Shapefile",
                    overwrite_layer = TRUE)
    exts <- c('dbf', 'shp', 'shx')
    fsExt <- file.path(tmpdir, paste0(bioSp(), '_bgShp.', exts))
    zip::zipr(zipfile = tmpExt, files = fsExt)

    # Create extent shapefile
    # add req metadata
    tmpRMM <- file.path(tmpdir, paste0(bioSp(), '_metadata.csv'))
    rangeModelMetadata::rmmToCSV(spp[[bioSp()]]$rmm, filename = tmpRMM)

    # Create RMD
    tmpRMD <- file.path(tmpdir, paste0(bioSp(), '_session.Rmd'))
    md_files <- c()
    md_intro_file <- tempfile(pattern = "intro_", fileext = ".md")
    rmarkdown::render("Rmd/userReport_intro.Rmd",
                      output_format = rmarkdown::github_document(html_preview = FALSE),
                      output_file = md_intro_file,
                      clean = TRUE,
                      encoding = "UTF-8")
    md_files <- c(md_files, md_intro_file)
    # Abbreviation for one species
    spAbr <- abbreviate(stringr::str_replace(bioSp(), "_", " "), minlength = 2)
    names(spAbr) <- bioSp()

    for (sp in bioSp()) {
      species_rmds <- NULL
      for (component in names(COMPONENT_MODULES[names(COMPONENT_MODULES) != c("espace", "rep")])) {
        for (module in COMPONENT_MODULES[[component]]) {
          rmd_file <- module$rmd_file
          rmd_function <- module$rmd_function
          if (is.null(rmd_file)) next

          if (is.null(rmd_function)) {
            rmd_vars <- list()
          } else {
            rmd_vars <- do.call(rmd_function, list(species = spp[[sp]]))
          }
          knit_params <- c(
            file = rmd_file,
            spName = spName(sp),
            sp = sp,
            spAbr = spAbr[[sp]],
            rmd_vars
          )
          module_rmd <- do.call(knitr::knit_expand, knit_params)

          module_rmd_file <- tempfile(pattern = paste0(module$id, "_"),
                                      fileext = ".Rmd")
          writeLines(module_rmd, module_rmd_file)
          species_rmds <- c(species_rmds, module_rmd_file)
        }
      }

      species_md_file <- tempfile(pattern = paste0(sp, "_"),
                                  fileext = ".md")
      rmarkdown::render(input = "Rmd/userReport_species.Rmd",
                        params = list(child_rmds = species_rmds,
                                      spName = spName(sp),
                                      spAbr = spAbr[[sp]]),
                        output_format = rmarkdown::github_document(html_preview = FALSE),
                        output_file = species_md_file,
                        clean = TRUE,
                        encoding = "UTF-8")
      md_files <- c(md_files, species_md_file)
    }

    combined_md <-
      md_files %>%
      lapply(readLines) %>%
      # lapply(readLines, encoding = "UTF-8") %>%
      lapply(paste, collapse = "\n") %>%
      paste(collapse = "\n\n")

    combined_rmd <- gsub('``` r', '```{r}', combined_md)
    writeLines(combined_rmd, tmpRMD, useBytes = TRUE)

    # Create ZIP file
    tmpZIP <- file.path(tmpdir, paste0(spp[[bioSp()]]$rmm$code$wallace$biomodelosTaxID, '.zip'))
    zip::zipr(zipfile = tmpZIP,
              files = c(tmpOccs, tmpPred, tmpExt, tmpRMM, tmpRMD))

    PAYLOAD <- list(
      taxID = spp[[bioSp()]]$rmm$code$wallace$biomodelosTaxID,
      biomodelos_user = input$userBio,
      cc_license = input$selLicense,
      atlas_agreement = FALSE,
      manifest = jsonlite::toJSON(manifest, auto_unbox = TRUE),
      model = httr::upload_file(tmpZIP,
                               type = "application/zip")
    )
    response <- content(POST(URL, body = PAYLOAD, encode = "multipart",
                             add_headers(host = 'api-biomodelos.humboldt.org.co',
                                         authorization = paste0('apiKey ', input$keyPost))),
                        as = 'parsed')
    if (is.null(response)) {
      shinyalert::shinyalert(
        "Pushed to Biomodelos (**)",
        type = "success")
    } else if (response == "Unauthorized") {
      shinyalert::shinyalert(
        "API key is not working.",
        type = "error")
      return()
    }
  })
}


