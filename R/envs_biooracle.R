# Wallace EcoMod: a flexible platform for reproducible modeling of
# species niches and distributions.
#
# envs_biooracle.R
# File author: Wallace EcoMod Dev Team. 2026.
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
#' @title envs_biooracle_layers Catalogue of Bio-ORACLE v3 variables
#' @description Return the catalogue of Bio-ORACLE v3 "baseline" (2000-2020)
#'   variables that Wallace can download from the Bio-ORACLE ERDDAP server
#'   (https://erddap.bio-oracle.org/erddap/).
#'
#' @details Each row is one variable. Ocean variables (\code{type = "ocean"})
#'   live in one ERDDAP dataset per depth layer, named
#'   \code{<var>_baseline_<years>_depth<surf|mean|min|max>}, each holding the
#'   fields \code{<var>_mean}, \code{<var>_min}, \code{<var>_max},
#'   \code{<var>_range}, \code{<var>_ltmin} and \code{<var>_ltmax}. Variables
#'   with \code{surf_only = TRUE} (air temperature, sea ice, mixed-layer depth)
#'   exist only at the surface. Terrain variables (\code{type = "terrain"},
#'   e.g. bathymetry) come from the static \code{terrain_characteristics}
#'   dataset and ignore the depth and statistic choices.
#'
#' @return data frame with columns \code{var} (Bio-ORACLE variable code),
#'   \code{label} (human readable name), \code{units}, \code{type}
#'   ("ocean" or "terrain"), \code{years} (baseline period in the dataset id)
#'   and \code{surf_only}.
#' @author Ben Best <ben@@ecoquants.com>
#' @seealso \code{\link{envs_biooracle}}, \code{\link{envs_biooracle_select}}
#' @export
envs_biooracle_layers <- function() {
  ocean <- data.frame(
    var   = c("thetao", "so", "chl", "o2", "no3", "po4", "si", "ph", "phyc",
              "dfe", "sws", "swd", "mlotst", "siconc", "sithick", "tas"),
    label = c("Sea water temperature", "Sea water salinity",
              "Chlorophyll concentration", "Dissolved oxygen",
              "Nitrate", "Phosphate", "Silicate", "pH",
              "Phytoplankton (carbon)", "Dissolved iron",
              "Sea water speed", "Sea water direction",
              "Mixed layer depth", "Sea ice concentration",
              "Sea ice thickness", "Air temperature (2 m)"),
    units = c("degree_C", "PSU", "mg/m^3", "mmol/m^3", "mmol/m^3", "mmol/m^3",
              "mmol/m^3", "1", "mmol/m^3", "mmol/m^3", "m/s", "degree",
              "m", "fraction", "m", "degree_C"),
    type  = "ocean",
    years = c("2000_2019", "2000_2019", "2000_2018", "2000_2018", "2000_2018",
              "2000_2018", "2000_2018", "2000_2018", "2000_2020", "2000_2018",
              "2000_2019", "2000_2019", "2000_2019", "2000_2020", "2000_2020",
              "2000_2020"),
    surf_only = c(rep(FALSE, 12), rep(TRUE, 4)),
    stringsAsFactors = FALSE)
  terrain <- data.frame(
    var   = c("bathymetry_mean", "bathymetry_min", "bathymetry_max", "slope",
              "terrain_ruggedness_index", "topographic_position_index"),
    label = c("Bathymetry (mean)", "Bathymetry (min)", "Bathymetry (max)",
              "Seafloor slope", "Terrain ruggedness index",
              "Topographic position index"),
    units = c("m", "m", "m", "degree", "1", "1"),
    type  = "terrain",
    years = NA_character_,
    surf_only = FALSE,
    stringsAsFactors = FALSE)
  rbind(ocean, terrain)
}

#' @title envs_biooracle_select Resolve Bio-ORACLE selections to ERDDAP requests
#' @description Translate the user's choice of variables, depth layer and
#'   statistic into the ERDDAP dataset ids, field names and Wallace layer
#'   names that \code{\link{envs_biooracle}} downloads. Pure function (no
#'   network), so the naming rules can be unit tested.
#'
#' @details Naming rules:
#' \itemize{
#'   \item ocean variable: dataset \code{<var>_baseline_<years>_depth<boDepth>},
#'     field \code{<var>_<boStat>}, layer name \code{<var>_<boStat>_<boDepth>}
#'     (e.g. \code{thetao_mean_surf}).
#'   \item surface-only variable with a benthic depth requested: silently
#'     resolved to the surface dataset (layer name ends in \code{_surf}).
#'   \item terrain variable: dataset \code{terrain_characteristics}, field and
#'     layer name equal to the variable code (e.g. \code{bathymetry_mean}).
#'   \item unknown variable codes raise an error.
#' }
#'
#' @param boVars character. Bio-ORACLE variable codes, see
#'   \code{\link{envs_biooracle_layers}}.
#' @param boDepth character. One of "surf" (sea surface), "mean", "min", "max"
#'   (benthic layer at the mean, minimum or maximum depth of each cell).
#' @param boStat character. One of "mean", "min", "max", "range", "ltmin",
#'   "ltmax" (long-term statistics over the baseline decades).
#' @return data frame with one row per layer and columns \code{var},
#'   \code{dataset_id}, \code{field}, \code{layer}, \code{label}, \code{units}.
#' @author Ben Best <ben@@ecoquants.com>
#' @examples
#' envs_biooracle_select(c("thetao", "so", "bathymetry_mean"), "surf", "mean")
#' @export
envs_biooracle_select <- function(boVars, boDepth = "surf", boStat = "mean") {
  stopifnot(length(boDepth) == 1, boDepth %in% c("surf", "mean", "min", "max"))
  stopifnot(length(boStat) == 1,
            boStat %in% c("mean", "min", "max", "range", "ltmin", "ltmax"))
  cat_bo <- envs_biooracle_layers()
  unknown <- setdiff(boVars, cat_bo$var)
  if (length(unknown) > 0) {
    stop("Unknown Bio-ORACLE variable(s): ", paste(unknown, collapse = ", "),
         call. = FALSE)
  }
  sel <- cat_bo[match(boVars, cat_bo$var), ]
  depth <- ifelse(sel$surf_only, "surf", boDepth)
  is_terrain <- sel$type == "terrain"
  data.frame(
    var        = sel$var,
    dataset_id = ifelse(is_terrain, "terrain_characteristics",
                        paste0(sel$var, "_baseline_", sel$years, "_depth", depth)),
    field      = ifelse(is_terrain, sel$var, paste0(sel$var, "_", boStat)),
    layer      = ifelse(is_terrain, sel$var,
                        paste0(sel$var, "_", boStat, "_", depth)),
    label      = sel$label,
    units      = sel$units,
    stringsAsFactors = FALSE)
}

#' @title envs_biooracle Obtain Bio-ORACLE marine variables
#' @description Download Bio-ORACLE v3 marine environmental layers from the
#'   Bio-ORACLE ERDDAP server (https://erddap.bio-oracle.org/erddap/) using
#'   the \code{rerddap} package. See https://www.bio-oracle.org.
#'
#' @details This function is called by the module envs_biooracle to download
#'   Bio-ORACLE v3 "baseline" (2000-2020 decadal) layers. The user selects the
#'   variables (\code{boVars}), the depth layer (sea surface or benthic;
#'   \code{boDepth}), the long-term statistic (\code{boStat}) and the
#'   resolution (\code{boRes}). Layers are global (-180 to 180, -90 to 90) and
#'   are \code{NA} over land, so downstream steps mask to the ocean.
#'
#'   Bio-ORACLE v3 is served at 0.05 degree (3 arcmin). Coarser resolutions
#'   are obtained server-side with an ERDDAP stride (every n-th cell is
#'   sampled, no averaging), which keeps downloads small; the resulting grid
#'   is snapped to whole-degree boundaries. Each layer is written as a GeoTIFF
#'   to \code{tempdir()} and returned as a \code{RasterStack}, matching
#'   \code{\link{envs_worldclim}}.
#'
#' @param boVars character. Bio-ORACLE variable codes, see
#'   \code{\link{envs_biooracle_layers}} (e.g. "thetao", "so", "chl",
#'   "bathymetry_mean").
#' @param boDepth character. Depth layer: "surf" (default), or benthic "mean",
#'   "min", "max". Ignored for terrain variables and surface-only variables.
#' @param boStat character. Statistic: "mean" (default), "min", "max",
#'   "range", "ltmin", "ltmax". Ignored for terrain variables.
#' @param boRes numeric. Resolution in decimal degrees; one of 0.05 (native,
#'   3 arcmin), 0.1, 0.25 (default), 0.5, 1.
#' @param doBrick logical. Converts downloaded rasters to brick for faster
#'   processing.
#' @param logger Stores all notification messages to be displayed in the
#'   Log Window of Wallace GUI. Insert the logger reactive list here for
#'   running in shiny, otherwise leave the default NULL.
#'
#' @examples
#' \dontrun{
#' sst_sss <- envs_biooracle(boVars = c("thetao", "so", "bathymetry_mean"),
#'                           boDepth = "surf", boStat = "mean", boRes = 1)
#' }
#'
#' @return A rasterStack or a rasterBrick (if doBrick=TRUE) of downloaded
#'   Bio-ORACLE rasters at the requested resolution, with layer names from
#'   \code{\link{envs_biooracle_select}}.
#'
#' @author Ben Best <ben@@ecoquants.com>
#' @seealso \code{\link[rerddap]{griddap}}, \code{\link{envs_biooracle_layers}}
#' @export
envs_biooracle <- function(boVars, boDepth = "surf", boStat = "mean",
                           boRes = 0.25, doBrick = FALSE, logger = NULL) {
  if (!requireNamespace("rerddap", quietly = TRUE)) {
    logger %>%
      writeLog(
        type = "warning",
        "This option is available if you install the 'rerddap' package ",
        "(which is a suggested package for Wallace, not a required dependency). If you ",
        "want to install it, close Wallace and run the following line in the ",
        "R Console: ", em("install.packages('rerddap')"))
    return()
  }
  if (is.null(boVars) || length(boVars) == 0) {
    logger %>% writeLog(type = 'error', 'Select at least one Bio-ORACLE variable.')
    return()
  }
  boRes <- as.numeric(boRes)
  if (is.na(boRes) || !boRes %in% c(0.05, 0.1, 0.25, 0.5, 1)) {
    logger %>% writeLog(type = 'error', 'Select a raster resolution.')
    return()
  }

  sel <- envs_biooracle_select(boVars, boDepth, boStat)
  # native grid is 0.05 deg with cell centers at -179.975 ... 179.975; sample
  # every `stride`-th cell starting at the native cell nearest the center of
  # the first coarse cell, so the strided grid can be snapped to whole degrees
  native <- 0.05
  stride <- as.integer(round(boRes / native))
  lon0 <- -180 + boRes / 2 - native / 2
  lat0 <- -90 + boRes / 2 - native / 2
  lon <- c(lon0, lon0 + (360 / boRes - 1) * boRes)
  lat <- c(lat0, lat0 + (180 / boRes - 1) * boRes)
  bo_url <- "https://erddap.bio-oracle.org/erddap/"

  smartProgress(logger, message = "Retrieving Bio-ORACLE data...", {
    files <- character(0)
    for (i in seq_len(nrow(sel))) {
      out_tif <- file.path(tempdir(), paste0("biooracle_", sel$layer[i], "_",
                                             gsub("\\.", "p", boRes), ".tif"))
      if (!file.exists(out_tif)) {
        r <- tryCatch({
          inf <- rerddap::info(sel$dataset_id[i], url = bo_url)
          g <- rerddap::griddap(
            inf, time = c("last", "last"), latitude = lat, longitude = lon,
            stride = c(1L, stride, stride), fields = sel$field[i],
            url = bo_url, fmt = "nc", read = FALSE)
          r <- terra::rast(g$summary$filename)
          # snap to whole-degree global extent (shift <= half a native cell)
          terra::ext(r) <- terra::ext(-180, 180, -90, 90)
          terra::crs(r) <- "EPSG:4326"
          names(r) <- sel$layer[i]
          terra::writeRaster(r, out_tif, overwrite = TRUE)
          r
        }, error = function(e) NULL)
        if (is.null(r)) {
          logger %>% writeLog(
            type = "error",
            paste0("Unable to retrieve ", sel$layer[i], " from the Bio-ORACLE ",
                   "ERDDAP server. Server may be down. Please use ",
                   "User-Specified module instead."))
          return()
        }
      }
      files <- c(files, out_tif)
    }
  })
  if (length(files) != nrow(sel)) return()

  bo <- raster::stack(files)
  names(bo) <- sel$layer

  # convert to brick for faster processing
  if (doBrick == TRUE) {
    smartProgress(logger,
                  message = "Converting to RasterBrick for faster processing...", {
      bo <- raster::brick(bo)
    })
  }

  logger %>% writeLog("Bio-ORACLE v3 marine variables ",
                      paste(names(bo), collapse = ", "), " at ",
                      boRes, " degree resolution.")
  return(bo)
}
