" This script contains 7 functions. Created to be SOURCED in the main MaxentENMEval function
    Main function:   create_hull()
    Other functions: as_list(), run before create_hull() so its output is fed into create_hull
                     buffer_pts(), alpha_select() (ashape_to_SPLDF and ahull_to_SPLDF within it), convex_hull_pm_split() are run within  create_hull()"


library(alphahull)
library(CoordinateCleaner)
library(raster)
library(rgl)
library(maptools) 

# Select input parameters _________________________________________________________________________________________
# chull_pt_thresh = 3 # threshold of number of points needed to create convex hull
                      # < threshold: points buffered. 
                      # => threshold: alpha and convex hulls plotted.
# alpha_val       = 30  # alpha value for alpha shape creation. not exactly sure what the unit is here, but related to radius of circle used to construct hull
                      # the lower the value, the greater the concavities, and the more holes (ie gaps) and outliers (which are buffered)
# buffer_val      = 2   # unit: km (need to check). Amount to buffer hulls and pts.
# thresh          = 320  # this value is hard-coded
                         # max amount between highest and lowest lon to tolerate for creating convex hull
# alpha_type      = "shape" or "hull"

# _____________________________________________ M A I N  F U N C T I O N : CREATE_HULL _____________________________________________________
" Determines hull type, creates a SPATIALPOLYGON and saves that as .RDA in a directory it creates for each species
  
  Returns:
    Creates a FOLDER (directory) per species, and saves convex+alpha or buffer points (class: SPATIAL POLYGON) as a .RDA"

create_hull <- function(occs, chull_pt_thresh, alpha_val, buffer_val, crs, alpha_type, iter) {
    
    if(nrow(occs) >= chull_pt_thresh) {
      out.i <- alpha_select(occ.pts.i, alpha_val, buffer_val, alpha_type)
      save(out.i, file = paste(out.fold.i, "alpha.rda", sep = "/"))
      rm(out.i)
      out.i <- convex_hull_pm_split(occ.pts.i, buffer_val)
      save(out.i, file = paste(out.fold.i, "convex.rda", sep = "/"))
    }
    
    if(n.i > 0 & n.i < chull_pt_thresh) {
      out.i <- buffer_pts(occ.pts.i, crs, buffer_val)
      save(out.i, file = paste(out.fold.i, "buff_pts.rda", sep = "/"))
    }
  } # end for
} # end create_hull

# Execute function
  # create_hull(in_csv_list, chull_pt_thresh, alpha_val, buffer_val, crs, alpha_type, iter)

# AS_LIST ____________________________________________________________________________________________________________
" in_csv is made into a list BY species name and duplicate coordinates are removed. "
as_list <- function(in_csv) {
  "
  Args:
    in_csv = 3 column data.table returned by clean_database function
    
  Returns:
    a LIST of coordinates by species
  "
  
  all_sp <- unique(in_csv$valid_species_name)
  in_csv_list <- list()
  
  for(sp_name in all_sp) {
    # Subset by species and ensure duplicates are removed 
    this_spec <- in_csv[in_csv$valid_species_name == sp_name]
    
    this_spec <- this_spec[!duplicated(this_spec)]
    this_spec <- cc_dupl(this_spec, lon = colnames(this_spec[,3]),lat = colnames(this_spec[,2]),
                        species = "valid_species_name", value = "clean", verbose = F)
    
    in_csv_list[[sp_name]] <- this_spec 
    
  } # end for
  
  return(in_csv_list)
  
} # end as_list

# BUFFER_PTS _________________________________________________________________________________________________________
" Creates buffer around individual pts "
buffer_pts <- function(occs, crs, buffer_val) {
  " 
  Args:
    occ.pts.i: occurence points of a species (subset of the in_csv_list)
    crs: crs object with projected coordinates
    buffer_val: amount to buffer the points. UNIT: km (or degrees.. unlikely but need to check!)
    
  Returns: 
    SpatialPolygon
  "
  
  sp_pts <- SpatialPoints(occs, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  sp_pts <- spTransform(sp_pts, crs)
  
  half_buffer_val <- buffer_val/2
  
  buff_poly <- gBuffer(sp_pts, width = half_buffer_val)
  
  return(buff_poly) #SpatialPolygons
  
} # end buffer_pts

#' @description Creates alpha shape OR hull of pts. Points outside hull are buffered.
#' NOTE: currently DOES NOT plot/save hulls that produce error. 
#' Need to investigate nature of error and how to keep track of failed species.
#' @param occs occurence points of a species
#' @param buffer_val distance in km to buffer the points
#' @param alpha_val alpha value for hull creation
#' @param alpha_type hull or shape
#' @return SpatialPolygons

alpha_select <- function(occs, alpha_val, buffer_val, alpha_type = c("hull","shape")) {

  sp_pts <- sp::SpatialPoints(occs, proj4string = sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  sp_pts <- sp::spTransform(sp_pts, CRSobj = crs)
  
  # create hull without tryCatch ...................................................
  # hull <- alphahull::ahull(occs, alpha = alpha_val)
  # ahull_spldf <- ahull_to_SPLDF(hull) #spatialLINESdataframe
  # ...............................................................................
  
  if(alpha_type == "hull"){
    hull <- tryCatch({ # need this because of tri.mesh(x) + other warnings/errors
      ahull(occs, alpha = alpha_val)
    },
    error <- function(e){
    },
    warning <- function(e){
    }
    ) # end try catch
  } # end if
  
  if(alpha_type == "shape"){
    hull <- tryCatch({
      ashape(occs, alpha = alpha_val)
    },
    error <- function(e){
    },
    warning <- function(e){
    }
    ) # end try catch
  } # end if
  
  if(!is.null(hull)){ # currently skips sp with ahull warning/errors
    if(alpha_type == "hull"){
      ahull_spldf <- ahull_to_SPLDF(hull)} 
    if(alpha_type == "shape"){
      ahull_spldf <- ashape_to_SPLDF(hull)}
  } else {
    print_this <- paste(paste(occ.pts.i[1,1]), "(Alpha Hull = NULL due to ahull() error/warning)")
    print(print_this)
    return(NULL)}
  
  ahull_sp <- tryCatch({ # need this bc gpolygonize sometimes returns NULL 
    rgeos::gPolygonize(ahull_spldf) # PROJECTED
  },
  error <- function(e){
  }
  ) # end try catch. this returns NULL for errors
  
  if (is.null(ahull_sp)) { # currently skips species with gpolygonize errors
    
    print_this <- paste(paste(occ.pts.i[1,1]), "(Alpha Hull = NULL due to gPolygonize error)")
    print(print_this)
    return(NULL)
  }
  
  # add buffer to hull
  ahull_sp <- spTransform(ahull_sp, crs)
  ahull_buffer <- gBuffer(ahull_sp, width = buffer_val) # SpatialPolygons 
  
  ahull_buffer1 <- as(ahull_buffer, "SpatialPolygonsDataFrame") # need to do this for MERGE()
  
  # CHECK IF ALL POINTS ARE IN the buffered hull. IF NOT, subset it, buffer it and merge it with the hull  
  in_hull <- over(ahull_buffer, sp_pts, bbox = NULL, returnList = T) 
  
  # to list out all pts within ALL hulls (not just in one hull as multiple hulls are often plotted and assigned to a list)
  hull_list <- names(in_hull)
  pts_in_hull <- vector()
  
  for(l in 1:length(hull_list)){
    name.l <- hull_list[l]
    list_nm <- in_hull[[name.l]]
    pts_in_hull <- c(pts_in_hull, list_nm)
  } # end for
  
  row_nm <- setdiff(1:nrow(occ.pts.i), pts_in_hull) # list of row numbers of points not in the hull # class: integer
  row_nm <- as.numeric(as.character(row_nm)) # changed class to feed into subsetting. class: numeric
  
  # buffer around points not in hull IF there are pts not inside hull. if not, do nothing
  if(length(row_nm) != 0){
    print_this <- paste(paste(occ.pts.i[1,1]), ": # of pts outside ahull =", length(row_nm), "/", nrow(occs))
    print(print_this)
    pts_buffer <- occ.pts.i[row_nm,]
    occs1 <- cbind(pts_buffer[,3], pts_buffer[,2]) #LON, LAT
    pts_buffer <- SpatialPoints(coords = occs1, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
    pts_buffer <- spTransform(pts_buffer, crs)
    half_buff_val <- buffer_val/2
    pts_buffer <- gBuffer(pts_buffer, width = half_buff_val) # class: SpatialPolygons
    pts_buffer <- as(pts_buffer, "SpatialPolygonsDataFrame")}
  
  # for testing the function (and see plots) ...........................................................
  # if(exists("pts_buffer")){
  # test <- bind(ahull_buffer1, pts_buffer)} else {test <- ahull_buffer1
  #   print("all pts inside hull")}
  # 
  # plot(sp_pts)
  # plot(ahull_buffer1, add = T)
  # plot(ahull_sp, add = T)
  
  # if(exists("pts_buffer")){plot(pts_buffer, add = T, col = "blue")} ................................
  
  
  # for actual function ...............................................................................
  if(exists("pts_buffer")){
    alpha_poly <- aggregate(bind(ahull_buffer1, pts_buffer))
  } else {alpha_poly <- ahull_buffer1}
  
  alpha_poly <- as(alpha_poly, "SpatialPolygons")
  
  return(alpha_poly) # SpatialPolygons
  # ....................................................................................................
  
} # end alpha_select

"Ashape to SpatialLinesDataFrame. x: ashape object (Source: online, not my function)"
ashape_to_SPLDF <- function(x, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")) {
  if(class(x) != 'ashape')
    stop('this function only works with `ashape` class objects')
  
  # convert ashape edges to DF
  x.as.df <- as.data.frame(x$edges)
  
  # convert each edge to a line segment
  l.list <- list()
  for(i in 1:nrow(x.as.df))
  {
    # extract line start and end points as 1x2 matrices
    p1 <- cbind(x.as.df$x1[i], x.as.df$y1[i])
    p2 <- cbind(x.as.df$x2[i], x.as.df$y2[i])
    # row-bind into 2x3 matrix
    l.list[[i]] <- Line(rbind(p1, p2))
  }
  
  # promote to Lines class, then to SpatialLines class
  l <- Lines(l.list, ID=1)
  
  # copy over CRS data from original point data
  # l.spl <- SpatialLines(list(l), proj4string= crs)
  l.spl <- SpatialLines(list(l), proj4string= CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  
  # promote to SpatialLinesDataFrame, required for export to GRASS / OGR
  l.spldf <- SpatialLinesDataFrame(l.spl, data=data.frame(id=1), match.ID=FALSE)
  
  return(l.spldf)
} # end ashape_to_SPLDF

"Ahull to SpatialLinesDataFrame.  x: ahull object (Source: online, not my function)"
ahull_to_SPLDF <- function(x, proj4string= CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")) {
  
  if(class(x) != 'ahull')
    stop('this function only works with `ahull` class objects')
  
  x.ah.df <- as.data.frame(x$arcs) # convert ashape edges to DF
  
  l.list <- list()   # convert each arc to a line segment
  for(i in 1:nrow(x.ah.df)){
    row_i <- x.ah.df[i,]
    
    v <- c(row_i$v.x, row_i$v.y)   # extract elements for arc()
    theta <- row_i$theta
    r <- row_i$r
    cc <- c(row_i$c1, row_i$c2)
    angles <- anglesArc(v, theta)  # from arc()
    seqang <- seq(angles[1], angles[2], length = 100)
    x <- cc[1] + r * cos(seqang)
    y <- cc[2] + r * sin(seqang)
    
    # convert to line segment
    l.list[[i]] <- Line(cbind(x,y)) } # end for 
  
  l <- Lines(l.list, ID=1)   # promote to Lines class, then to SpatialLines class
  # l.spl <- SpatialLines(list(l), proj4string= crs)   # copy over CRS data from original point data
  l.spl <- SpatialLines(list(l), proj4string= CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))   # copy over CRS data from original point data
  
  l.spldf <- SpatialLinesDataFrame(l.spl, data=data.frame(id=1), match.ID=FALSE)  # promote to SpatialLinesDataFrame, required for export to GRASS / OGR
  
  return(l.spldf)
  
} # end ahull_to_SPLDF

# CONVEX_HULL_PM_SPLIT  _______________________________________________________________________________________________
" Creates a convex hull. Splits hull into two if the difference between max and min lon exceeds a given thresh to prevent prime meridian issue"
" NOTE: lon_opt, lat_opt hard coded so NEED TO CHANGE if using different occs"
convex_hull_pm_split <- function(occs, buffer_val) {
  " 
    Args:
      occ.pts.i: occurence points of a species (subset of the in_csv_list)
      buffer_val: amount to buffer the points. UNIT: km
      
    Returns: 
      SpatialPolygon
  "
  
  if(max(occs$lon_opt) -  min(occs$lon_opt) < 320) {
    
    sp_pts <- SpatialPoints(occs, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
    sp_pts <- spTransform(sp_pts, CRSobj = crs)
    
    hull <- gConvexHull(sp_pts, byid = F) # id arg: character. id label for resulting geometries
    conv_hull <- gBuffer(hull, width = buffer_val)
  
  } else {
    for(i in 1:nrow(occs)) {
      if(occs[i,1] > 0) {occs[i,1] <- occs[i,1] - 180 # still in longlat
      } else { 
        occs[i,1] <- (occs[i,1] + 180)
      }
    } # end for
    
    sp_pts <- SpatialPoints(occs, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
    sp_pts <- spTransform(sp_pts, CRSobj = crs)
    
    hull <- gConvexHull(sp_pts, byid = F) # SpatialPolygons
    hull <- gBuffer(hull, width = buffer_val)
    lon <- c(0, 0, 0)
    lat <- c(-7342.23, 0, 7342.23)
    line <- SpatialLines(list(Lines(Line(cbind(lon,lat)), ID="PM")), proj4string = crs)
    intrsct <-  gIntersection(hull, line)
    intrsct <- gBuffer(intrsct, width = 0.1)
    splt <- gDifference(hull, intrsct)

    if(splt@polygons[[1]]@Polygons[[1]]@coords[1,1] < 0) { # need this because poly #1 and #2 varies- as in which side of the PM it is
      
      # OK this is repetitive. Simplify if there's time
      
      poly_1 <- SpatialPolygons(list(Polygons(list(splt@polygons[[1]]@Polygons[[1]]), "1")), proj4string = crs)
      poly_1 <- spTransform(poly_1, CRSobj = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
      poly_1 <- elide(poly_1, shift = c(180,0))
      proj4string(poly_1) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
      poly_1=spTransform(poly_1, CRSobj = crs)
      
      poly_2 <- SpatialPolygons(list(Polygons(list(splt@polygons[[1]]@Polygons[[2]]), "2")), proj4string = crs)
      poly_2 <- spTransform(poly_2, CRSobj = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
      poly_2 <- elide(poly_2, shift = c(-180,0))
      proj4string(poly_2) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
      poly_2=spTransform(poly_2, CRSobj = crs) } else {
        
        poly_1 <- SpatialPolygons(list(Polygons(list(splt@polygons[[1]]@Polygons[[1]]), "1")), proj4string = crs)
        poly_1 <- spTransform(poly_1, CRSobj = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
        poly_1 <- elide(poly_1, shift = c(-180,0))
        proj4string(poly_1) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
        poly_1=spTransform(poly_1, CRSobj = crs)
        
        poly_2 <- SpatialPolygons(list(Polygons(list(splt@polygons[[1]]@Polygons[[2]]), "2")), proj4string = crs)
        poly_2 <- spTransform(poly_2, CRSobj = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
        poly_2 <- elide(poly_2, shift = c(180,0))
        proj4string(poly_2) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
        poly_2=spTransform(poly_2, CRSobj = crs)}
    
    conv_hull <- bind(poly_1, poly_2)

    print_this <- paste(paste(occ.pts.i[1,1]), "(Convex Hull: PM issue resolved)")
    print(print_this)
  } # end else
  
  return(conv_hull) #SpatialPolygons   

} # end convex_hull_pm_split