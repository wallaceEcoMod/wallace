
#' @title penvs_userBgExtent
#' @description ..
#'
#' @details
#' See Examples.
#'
#' @param bgShp_path x
#' @param bgShp_name x
#' @param userBgBuf x
#' @param logger x
# @keywords
#'
# @examples
#'
#'
# @return
#' @author Jamie Kass <jkass@@gradcenter.cuny.edu>
# @note

# @seealso
# @references
# @aliases - a list of additional topic names that will be mapped to
# this documentation when the user looks them up from the command
# line.
# @family - a family name. All functions that have the same family tag will be linked in the documentation.
#' @export

penvs_userBgExtent <- function(bgShp_path, bgShp_name, userBgBuf, logger=NULL) {

    pathdir <- dirname(bgShp_path)
    pathfile <- basename(bgShp_path)
    # get extensions of all input files
    exts <- sapply(strsplit(bgShp_name, '\\.'), FUN=function(x) x[2])

    if (length(exts) == 1 & exts == 'csv') {
      f <- read.csv(bgShp_path, header = TRUE)
      bgExt <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(f)), 1)))
    } else if ('shp' %in% exts) {
      if (length(exts) < 3) {
        logger %>% writeLog(type = 'error', 'If entering a shapefile, please
                               select all the following files: .shp, .shx, .dbf.')
        return()
      }
      file.rename(bgShp_path, file.path(pathdir, bgShp_name))
      # get index of .shp
      i <- which(exts == 'shp')
      shpName <- strsplit(bgShp_name[i], '\\.')[[1]][1]
      # read in shapefile and extract coords
      bgExt <- rgdal::readOGR(pathdir[i], shpName)
    } else {
      logger %>% writeLog(type = 'error', 'Please enter either a CSV file of
                             vertex coordinates or shapefile (.shp, .shx, .dbf).')
      return()
    }
    logger %>% writeLog("Study extent: user-defined polygon.")

    if (userBgBuf > 0) {
      bgExt <- rgeos::gBuffer(bgExt, width = userBgBuf)
      logger %>% writeLog('Study extent buffered by ', userBgBuf, ' degrees.')
    }

    return(bgExt)
}
