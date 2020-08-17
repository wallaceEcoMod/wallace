#' @title penvs_userBgExtent: user provided background extent
#' @description This function generates a background area according to a user provided polygon and buffer
#'
#' @details
#' This function is used in the select study region component. Here, the user provides either a shapefile or a csv with vertex coordinates
#' with the desired shape for the background extent, the user may include a buffer to the given polygon. The buffered poylgon must include all occurrences (occs) or function will return an error.
#' The function returns a SpatialPolygons object of the desired extent (+ buffer).
#'
#' @param bgShp_path Path to the user provided shapefile or csv with vertex coordinates. M
#' @param bgShp_name Name of the user porvided shapefile or csv with vertex coordinates.
#' @param userBgBuf Buffer to be used in creating the background extent must be >=0
#' @param occs data frame of cleaned or processed occurrences obtained from components occs: Obtain occurrence data or, poccs: Process occurrence data.
#' @param logger stores all notification messages to be displayed in the Log Window of Wallace GUI. insert the logger reactive list here for running in shiny,
#' otherwise leave the default NULL
#' @param spN data frame of cleaned occurrences obtained from component occs: Obtain occurrence data. Used to obtain species name for logger messages
# @keywords

# @keywords
#'
#' @examples
#' "espeletia argentea" endemic to Colombia (provided shapefile in test folder)
#' occs <-  occs_queryDb(spName = "espeletia argentea", occDb = "gbif", occNum = 100)
#' occs <- as.data.frame(occs[[1]]$cleaned)
#' Path <- list.files(path='./tests/testthat/shapefile', pattern = "COL_adm0.", full.names = TRUE)
#' Name <- list.files(path='./tests/testthat/shapefile', pattern = "COL_adm0.", full.names = FALSE)
#' userBgbf <- penvs_userBgExtent(bgShp_path = Path, bgShp_name = Name, userBgBuf = 0.5,occs=occs)
#' @return This function returns a SpatialPolygons object with the user provided shape (+ a buffer is userBgBuf >0).
#' The polygon will be at least large enough to contain all occurrences.
#' @author Jamie Kass < jamie.m.kass@@gmail.com >
#' @author Gonzalo E. Pinilla-Buitrago < gpinillabuitrago@@gradcenter.cuny.edu>
#' @author Andrea Paz < paz.andreita@@gmail.com>
# @note

#' @seealso \code{\link{penvs_drawBgExtent}}, \code{\link{penvs_bgExtent}}, \code{\link{penvs_bgMask}} , \code{\link{penvs_bgSample}}
# @references
# @aliases - a list of additional topic names that will be mapped to
# this documentation when the user looks them up from the command
# line.
# @family - a family name. All functions that have the same family tag will be linked in the documentation.
#' @export

penvs_userBgExtent <- function(bgShp_path, bgShp_name, userBgBuf, occs,
                               logger = NULL, spN = NULL) {
    pathdir <- dirname(bgShp_path)
    pathfile <- basename(bgShp_path)
    # get extensions of all input files
    exts <- sapply(strsplit(bgShp_name, '\\.'), FUN = function(x) x[2])
    if (length(exts) == 1 & exts[1] == 'csv') {
      f <- read.csv(bgShp_path, header = TRUE)
      bgExt <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(f)), 1)))
    } else if ('shp' %in% exts) {
      if (length(exts) < 3) {
        logger %>%
          writeLog(type = 'error',
                   paste0('If entering a shapefile, please select all the ',
                          'following files: .shp, .shx, .dbf.'))
        return()
      }
      if (!file.exists(file.path(pathdir, bgShp_name)[1])) {
        file.rename(bgShp_path, file.path(pathdir, bgShp_name))
      }
      # get index of .shp
      i <- which(exts == 'shp')
      shpName <- strsplit(bgShp_name[i], '\\.')[[1]][1]
      # read in shapefile and extract coords
      bgExt <- rgdal::readOGR(pathdir[i], shpName)
    } else {
      logger %>%
        writeLog(type = 'error',
                 paste0('Please enter either a CSV file of vertex coordinates ',
                        'or shapefile (.shp, .shx, .dbf).'))
      return()
    }

    if (userBgBuf >= 0) {
      bgExt <- rgeos::gBuffer(bgExt, width = userBgBuf)
    }

    ### Points outside polygon

    occs.xy <- occs[c('longitude', 'latitude')]
    # make spatial pts object of original occs and preserve origID
    pts <- sp::SpatialPointsDataFrame(occs.xy, data = occs['occID'])
    intersect <- sp::over(pts, bgExt)
    ptRem <- ifelse(all(!is.na(intersect)), 0, as.numeric(which(is.na(intersect))))

    if (ptRem == 0) {
      if (userBgBuf > 0) {
        logger %>% writeLog(hlSpp(spN), 'Study extent user-defined polygon buffered by ',
                            userBgBuf, ' degrees.')
      } else {
        logger %>% writeLog(hlSpp(spN), "Study extent: user-defined polygon.")
      }
      return(bgExt)
    } else if (ptRem > 0) {
      logger %>%
        writeLog(type = 'error', hlSpp(spN),
                 "The polygon did not include all localities(**). ",
                 "You can remove localities in Process Occs component")
      return()
    }
}
