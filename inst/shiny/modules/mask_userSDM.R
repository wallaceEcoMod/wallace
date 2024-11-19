# Wallace EcoMod: a flexible platform for reproducible modeling of
# species niches and distributions.
#
# mask_userSDM.R
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
mask_userSDM_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    fileInput(ns("sdmFile"), label = "Upload distribution map (**)",
              multiple = TRUE, accept = c(".tif", ".asc")),
    actionButton(ns("goUserSDM"), "Load SDM")
  )
}

mask_userSDM_module_server <- function(input, output, session, common) {
  spp <- common$spp
  logger <- common$logger

  observeEvent(input$goUserSDM, {
    # WARNING ####
    if (is.null(input$sdmFile)) {
      logger %>% writeLog(type = 'error', "Raster files not uploaded (**)")
      return()
    }
    for (i in 1:length(input$sdmFile$name)) {
      sppName <- fileNameNoExt(input$sdmFile$name[i])
      sppName <- paste0(toupper(substring(sppName, 1, 1)),
                        substring(sppName, 2, nchar(sppName)))
      sppName <- stringr::str_replace(sppName, " " , "_")
      # Check genus and species name on file
      if (length(strsplit(sppName, "_")[[1]]) != 2) {
        logger %>%
          writeLog(
            type = 'warning', "'", sppName,
            "' file without genus and species name format (e.g. Canis_lupus.tif).")
        return()
      }
      if (sppName %in% names(spp)) {
        logger %>% writeLog(hlSpp(sppName),
                            type = 'warning',
                            "SDM uploaded matches previously loaded occurrence data.")
      }
    }

    # FUNCTION CALL ####
    for (i in 1:length(input$sdmFile$name)) {
      # Create species object if does not exists
      sppName <- fileNameNoExt(fmtSpN(input$sdmFile$name[i]))
      userSDMs <- mask_userSDM(rasPath = input$sdmFile$datapath[i],
                               rasName = input$sdmFile$name[i],
                               logger, spN = sppName)
      req(userSDMs)
      # Check if prediction is threshold
      userThr <- terra::spatSample(x = terra::rast(userSDMs$sdm),
                                   size = 100, na.rm = TRUE)[, 1]
      userThr <- !any(userThr > 0 & userThr < 1)

      if (!(sppName %in% names(spp))) {
        spp[[sppName]] <- list()
        logger %>% writeLog(hlSpp(sppName),
                            "New species registered (**).")
      }
      # LOAD INTO SPP ####
      spp[[sppName]]$mask$userThr <- userThr
      spp[[sppName]]$mask$userSDM <- userSDMs$sdm * 1
      spp[[sppName]]$mask$userPolyExt <- userSDMs$extSdm
      logger %>% writeLog(hlSpp(sppName), "User SDM prediction loaded")
      # For biomodelos
      spp[[sppName]]$biomodelos$mask$userSDM <- userSDMs$sdm * 1
      spp[[sppName]]$biomodelos$mask$userPolyExt <- userSDMs$extSdm

      # REFERENCES ####
      knitcitations::citep(citation("raster"))

      # METADATA ####
      spp[[sppName]]$rmm$code$wallace$userSDM <- TRUE
      spp[[sppName]]$rmm$code$wallace$rasPath <- input$sdmFile$datapath
      spp[[sppName]]$rmm$code$wallace$rasName <- input$sdmFile$name
    }
    common$update_component(tab = "Map")
  })
}

mask_userSDM_module_map <- function(map, common) {

  curSp <- common$curSp
  spp <- common$spp

  # Map logic
  req(spp[[curSp()]]$mask$userSDM)
  # Zoom
  userRaster <- spp[[curSp()]]$mask$userSDM
  userValues <- terra::spatSample(x = terra::rast(userRaster),
                                  size = 100, na.rm = TRUE)[, 1]
  userPolyExt <- spp[[curSp()]]$mask$userPolyExt
  polys <- userPolyExt@polygons[[1]]@Polygons
  if (length(polys) == 1) {
    xy <- list(polys[[1]]@coords)
  } else{
    xy <- lapply(polys, function(x) x@coords)
  }
  zoomExt <- raster::extent(userRaster)
  map %>% fitBounds(lng1 = zoomExt[1], lng2 = zoomExt[2],
                    lat1 = zoomExt[3], lat2 = zoomExt[4])

  map %>%
    clearAll() %>%
    # add background polygon
    mapBgPolys(xy,
               color = 'green', group = 'mask')

  # Define raster colors and shiny legend
  rasCols <- c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c")
  # if it is threshold specified
  if (!any(userValues > 0 & userValues < 1)) {
    map %>%
      addLegend("bottomright", colors = c('gray', 'darkgreen'),
                title = "Distribution<br>map",
                labels = c("Unsuitable", "Suitable"),
                opacity = 1, layerId = 'expert') %>%
      leafem::addGeoRaster(spp[[curSp()]]$mask$userSDM,
                           colorOptions = leafem::colorOptions(
                             palette = colorRampPalette(colors = c('gray', 'darkgreen'))),
                           opacity = 0.7, group = 'mask', layerId = 'maskPred')
  } else {
    # if threshold specified
    quanRas <- quantile(c(raster::minValue(userRaster),
                          raster::maxValue(userRaster)),
                        probs = seq(0, 1, 0.1))
    legendPal <- colorNumeric(rev(rasCols), quanRas, na.color = 'transparent')
    map %>%
      addLegend("bottomright", pal = legendPal, title = "Suitability<br>(User) (**)",
                values = quanRas, layerId = "expert",
                labFormat = reverseLabel(2, reverse_order = TRUE)) %>%
      leafem::addGeoRaster(spp[[curSp()]]$mask$userSDM,
                           colorOptions = leafem::colorOptions(
                             palette = colorRampPalette(colors = rasCols)),
                           opacity = 0.7, group = 'mask', layerId = 'maskPred')
  }
}

mask_userSDM_module_rmd <- function(species) {
  # Variables used in the module's Rmd code
  list(
    mask_userSDM_knit = species$rmm$code$wallace$userSDM == TRUE,
    rasPath_rmd = species$rmm$code$wallace$rasPath,
    rasName_rmd = species$rmm$code$wallace$rasName,
    )
}

