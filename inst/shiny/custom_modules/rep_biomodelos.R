# Wallace EcoMod: a flexible platform for reproducible modeling of
# species niches and distributions.
#
# rep_biomodelos.R
# File author: Wallace EcoMod Dev Team. 2023.
# --------------------------------------------------------------------------
# This file is part of the Wallace EcoMod application
# (hereafter “Wallace”).
#
# Wallace is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License,
# or (at your option) any later version.
#
# Wallace is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Wallace. If not, see <http://www.gnu.org/licenses/>.
# --------------------------------------------------------------------------
#
rep_biomodelos_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    uiOutput(ns("bioSpUI")),
    passwordInput(ns("keyPost"), label = "Enter API Key", value = ""),
    textInput(ns("userBio"), "User BioModelos e-mail", value = NULL),
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
  curSp <- common$curSp

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
    if (is.null(spp[[curSp()]]$visualization$mapPred)) {
      #add warning
      shinyalert::shinyalert(
        "You need a map prediction built on Wallace in order to push payload to BioModelos. ",
        type = "error")
      return()
    } else {

    if (spp[[bioSp()]]$rmm$data$occurrence$sources != "Biomodelos") {
      shinyalert::shinyalert(
        "You must submit a model built with occurrences from BioModelos. ",
        type = "error")
      return()
    }
    if (is.null(spp[[bioSp()]]$biomodelos$prediction)) {
      shinyalert::shinyalert(
        "You need a map prediction built on Wallace before pushing to BioModelos. ",
        type = "error")
      return()
    }

    URL <- 'https://api-biomodelos.humboldt.org.co/v2/models'


    manifest <- list(
      edited_occurrences = paste0(bioSp(), '_occs.csv'),
      raster_model_prediction = paste0(bioSp(), '_pred.tif'),
      raster_model_threshold = paste0(bioSp(), '_thr.csv'),
      raster_model_modified_expert =
        if (is.null(spp[[bioSp()]]$biomodelos$predExpert)) {
          "No model prediction modified by expert."
        } else {
          paste0(bioSp(), '_pred_expert.tif')
        },
      shape_file_expert =
        if (is.null(spp[[bioSp()]]$mask$expertPoly)) {
        "No model prediction modified by expert."
      } else {
        paste0(bioSp(), '_expertPolygonsShp.zip')
      },
      shape_file_extent = paste0(bioSp(), '_projectionExtentShp.zip'),
      biomodelos_metadata = paste0(bioSp(), '_metadata.csv'),
      model_metadata = paste0(bioSp(), '_rmms.csv'),
      wallace_session = paste0(bioSp(), '_session.Rmd')
    )

    ### Create files
    tmpdir <- tempdir()
    # Create occs
    # add req occs
    tmpOccs <- file.path(tmpdir, paste0(bioSp(), '_occs.csv'))
    occsBio <- spp[[bioSp()]]$occData$occsOrig
    usedOccs <- occsBio$occID %in% spp[[bioSp()]]$occs$occID
    occsBio <- cbind(occsBio, use = usedOccs)
    write.csv(occsBio, tmpOccs, row.names = FALSE)

    # Create mapPrediction
    # add req pred
    tmpPred <- file.path(tmpdir, paste0(bioSp(), '_pred.tif'))
    raster::writeRaster(spp[[bioSp()]]$biomodelos$prediction, tmpPred,
                        overwrite = TRUE)

    # Create thr
    # add req occs
    tmpThrs <- file.path(tmpdir, paste0(bioSp(), '_thr.csv'))
    write.csv(spp[[bioSp()]]$biomodelos$thrs, tmpThrs, row.names = TRUE)

    # Create raster model modified by expert
    if (!is.null(spp[[bioSp()]]$biomodelos$predExpert)) {
      tmpPredExp <- file.path(tmpdir, paste0(bioSp(), '_pred_expert.tif'))
      raster::writeRaster(spp[[bioSp()]]$biomodelos$predExpert, tmpPredExp,
                          overwrite = TRUE)
    }

    # Create shapefile of expert polygons
    if (!is.null(spp[[bioSp()]]$mask$expertPoly)) {
      tmpExpPoly <- file.path(tmpdir, paste0(bioSp(), '_expertPolygonsShp.zip'))
      expertPolys <- do.call("rbind",
                             c(args = lapply(seq_along(spp[[bioSp()]]$mask$expertPoly),
                                             function(i){spp[[bioSp()]]$mask$expertPoly[i][[1]]}),
                               makeUniqueIDs = TRUE))
      sf::st_write(obj = sf::st_as_sf(expertPolys),
                      dsn = tmpdir,
                      layer = paste0(bioSp(), '_expertPolygonsShp'),
                      driver = "ESRI Shapefile",
                      append = FALSE)
      extsExpPoly <- c('dbf', 'shp', 'shx')
      fsExpPoly <- file.path(tmpdir, paste0(bioSp(), '_expertPolygonsShp.', extsExpPoly))
      zip::zipr(zipfile = tmpExpPoly , files = fsExpPoly)
    }

    # Create extent shapefile
    # add req ext
    tmpExt <- file.path(tmpdir, paste0(bioSp(), '_projectionExtentShp.zip'))
    sf::st_write(obj = sf::st_as_sf(spp[[bioSp()]]$procEnvs$bgExt),
                    dsn = tmpdir,
                    layer = paste0(bioSp(), '_bgShp'),
                    driver = "ESRI Shapefile",
                    append = FALSE)
    exts <- c('dbf', 'shp', 'shx')
    fsExt <- file.path(tmpdir, paste0(bioSp(), '_bgShp.', exts))
    zip::zipr(zipfile = tmpExt, files = fsExt)

    # Create Metadata
    evalTbl <- spp[[bioSp()]]$evalOut@results
    if (grepl("Maxent", spp[[bioSp()]]$biomodelos$modelingMethod, fixed = TRUE)) {
      evalTbl <- evalTbl[evalTbl$tune.args == spp[[bioSp()]]$rmd$vis_curModel, ]
    }
    tmpBioMeta <- file.path(tmpdir, paste0(bioSp(), '_metadata.csv'))
    bioMet <- data.frame()
    bioMet[1, 1:2] <- c("acceptedNameUsage",
                        unique(spp[[bioSp()]]$occs$scientific_name)[1])
    bioMet[2, 1:2] <- c("consensusMethod",
                        "all")
    bioMet[3, 1:2] <- c("dd",
                        format(Sys.Date(), "%d"))
    bioMet[4, 1:2] <- c("license",
                        input$selLicense)
    bioMet[5, 1:2] <- c("methodFile",
                        paste0(bioSp(), '_session.Rmd'))
    bioMet[6, 1:2] <- c("mm",
                        format(Sys.Date(), "%m"))
    bioMet[7, 1:2] <- c("modelingMethod",
                        spp[[bioSp()]]$biomodelos$modelingMethod)
    bioMet[8, 1:2] <- c("omission",
                        spp[[bioSp()]]$biomodelos$occsOmitted)
    bioMet[9, 1:2] <- c("perfStatSD",
                        paste(NA,  # 'auc.train',
                              NA,  # 'cbi.train',
                              evalTbl[1, 'auc.diff.sd'],
                              evalTbl[1, 'auc.val.sd'],
                              evalTbl[1, 'cbi.val.sd'],
                              evalTbl[1, 'or.10p.sd'],
                              evalTbl[1, 'or.mtp.sd'],
                              sep = "; "))
    bioMet[10, 1:2] <- c("perfStatType",
                         paste0('auc.train; cbi.train; auc.diff.avg; ',
                                'auc.val.avg; cbi.val.avg; or.10p.avg; ',
                                'or.mtp.avg'))
    bioMet[11, 1:2] <- c("perfStatValue",
                         paste(evalTbl[1, 'auc.train'],
                               evalTbl[1, 'cbi.train'],
                               evalTbl[1, 'auc.diff.avg'],
                               evalTbl[1, 'auc.val.avg'],
                               evalTbl[1, 'cbi.val.avg'],
                               evalTbl[1, 'or.10p.avg'],
                               evalTbl[1, 'or.mtp.avg'],
                               sep = "; "))
    bioMet[12, 1:2] <- c("recsUsed",
                         nrow(spp[[bioSp()]]$occs))
    bioMet[13, 1:2] <- c("taxID",
                         spp[[bioSp()]]$rmm$code$wallace$biomodelosTaxID)
    bioMet[14, 1:2] <- c("thresholdType",
                         switch(spp[[bioSp()]]$rmm$prediction$binary$thresholdRule,
                                none = "Continuous",
                                mtp = "0",
                                p10 = "10",
                                qtp = as.character(spp[[bioSp()]]$rmm$code$wallace$trainPresQuantile * 100)))
    bioMet[15, 1:2] <- c("thresholdValue",
                         spp[[bioSp()]]$rmm$prediction$binary$thresholdSet)
    bioMet[16, 1:2] <- c("validationType",
                         spp[[bioSp()]]$rmm$model$partition$partitionRule)
    bioMet[17, 1:2] <- c("yyyy",
                         format(Sys.Date(), "%Y"))
    bioMet[18, 1:2] <- c("zip",
                         paste0(spp[[bioSp()]]$rmm$code$wallace$biomodelosTaxID,
                                '.zip'))
    names(bioMet) <- c("name", "value")
    write.csv(bioMet, tmpBioMeta, row.names = FALSE)

    # add req metadata
    tmpRMM <- file.path(tmpdir, paste0(bioSp(), '_rmms.csv'))
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
      compNames <- names(COMPONENT_MODULES) %in% c("espace", "mask", "indic", "diver", "rep")
      for (component in names(COMPONENT_MODULES[!compNames])) {
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
    filesZIP <- c(tmpOccs, tmpBioMeta, tmpPred, tmpThrs, tmpExt, tmpRMM, tmpRMD)
    if (!is.null(spp[[bioSp()]]$biomodelos$predExpert)) {
      filesZIP <- c(filesZIP, tmpPredExp)
    }
    if (!is.null(spp[[bioSp()]]$mask$expertPolyt)) {
      filesZIP <- c(filesZIP, tmpExpPoly)
    }

    zip::zipr(zipfile = tmpZIP,
              files = filesZIP)

    PAYLOAD <- list(
      taxID = spp[[bioSp()]]$rmm$code$wallace$biomodelosTaxID,
      biomodelos_user = input$userBio,
      cc_license = input$selLicense,
      atlas_agreement = FALSE,
      manifest = jsonlite::toJSON(manifest, auto_unbox = TRUE),
      model = httr::upload_file(tmpZIP,
                               type = "application/zip")
    )
    response <- httr::content(httr::POST(URL, body = PAYLOAD, encode = "multipart",
                                         httr::add_headers(host = 'api-biomodelos.humboldt.org.co',
                                         authorization = paste0('apiKey ', input$keyPost))),
                        as = 'parsed')
    if (is.null(response)) {
      shinyalert::shinyalert(
        "Pushed to BioModelos (**)",
        type = "success")
    } else if (response == "Unauthorized") {
      shinyalert::shinyalert(
        "API key is not working.",
        type = "error")
      return()
    }
    }
  })
}


