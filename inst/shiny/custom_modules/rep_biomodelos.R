rep_biomodelos_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
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
  curSp <- common$curSp

  observeEvent(input$pushBiomod, {
    URL <- 'https://api-biomodelos.humboldt.org.co/v2/models'

    biomodelos <- list(
      taxID = spp[[curSp()]]$rmm$code$wallace$biomodelosTaxID,
      biomodelos_user = input$userBio,
      cc_license = input$selLicense,
      atlas_agreement = FALSE,
      model = 'attachment'
    )

    manifest <- list(
      edited_occurrences = paste0(curSp(), '_processed_occs.csv'),
      raster_model_prediction = paste0(curSp(), '_pred.tif'),
      # raster_model_continuous = paste0(curSp(), '_cloglog.tif'),
      # raster_model_thresholded = paste0(curSp(), '_mtp.tif'),
      shape_file_mask = paste0(curSp(), '_projectionExtentShp.zip'),
      model_metadata = paste0(curSp(), '_metadata.csv'),
      wallace_session = paste0(curSp(), '_session.Rmd')
    )

    ### Create files
    tmpdir <- tempdir()
    # Create occs
    # add req occs
    tmpOccs <- file.path(tmpdir, paste0(curSp(), '_processed_occs.csv'))
    write.csv(spp[[curSp()]]$occs, tmpOccs, row.names = FALSE)

    # Create mapPrediction
    # add req pred
    tmpPred <- file.path(tmpdir, paste0(curSp(), '_pred.tif'))
    raster::writeRaster(spp[[curSp()]]$visualization$mapPred, tmpPred,
                        overwrite = TRUE)

    # Create extent shapefile
    # add req ext
    tmpExt <- file.path(tmpdir, paste0(curSp(), '_projectionExtentShp.zip'))
    rgdal::writeOGR(obj = spp[[curSp()]]$procEnvs$bgExt,
                    dsn = tmpdir,
                    layer = paste0(curSp(), '_bgShp'),
                    driver = "ESRI Shapefile",
                    overwrite_layer = TRUE)
    exts <- c('dbf', 'shp', 'shx')
    fsExt <- file.path(tmpdir, paste0(curSp(), '_bgShp.', exts))
    zip::zipr(zipfile = tmpExt, files = fsExt)

    # Create extent shapefile
    # add req metadata
    tmpRMM <- file.path(tmpdir, paste0(curSp(), '_metadata.csv'))
    rangeModelMetadata::rmmToCSV(spp[[curSp()]]$rmm, filename = tmpRMM)

    # Create RMD
    tmpRMD <- file.path(tmpdir, paste0(curSp(), '_session.Rmd'))
    md_files <- c()
    md_intro_file <- tempfile(pattern = "intro_", fileext = ".md")
    rmarkdown::render("Rmd/userReport_intro.Rmd",
                      output_format = rmarkdown::github_document(html_preview = FALSE),
                      output_file = md_intro_file,
                      clean = TRUE,
                      encoding = "UTF-8")
    md_files <- c(md_files, md_intro_file)
    # Abbreviation for one species
    spAbr <- abbreviate(stringr::str_replace(curSp(), "_", " "), minlength = 2)
    names(spAbr) <- curSp()

    for (sp in curSp()) {
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
    zip::zipr(zipfile = 'C:/Users/Gonzalo/Desktop/data/biomodelos/payload/files.zip',
              files = c(tmpOccs, tmpPred, tmpExt, tmpRMM, tmpRMD))

    PAYLOAD <- list(
      biomodelos = jsonlite::toJSON(biomodelos, auto_unbox = TRUE),
      manifest = jsonlite::toJSON(manifest, auto_unbox = TRUE),
      attachment = httr::upload_file('C:/Users/Gonzalo/Desktop/data/biomodelos/files.zip',
                               type = "application/zip")
    )

    print(PAYLOAD)
  })
}


