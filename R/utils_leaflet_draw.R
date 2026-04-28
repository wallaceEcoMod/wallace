# ------------------------------------------------------------------------------
#
# This file contains code adapted from the 'leaflet.extras' package (GPL-3)
# by Sebastian Gatscha, Bhaskar Karambelkar, Barret Schloerke, et al.
# Original Source: https://github.com/trafficonese/leaflet.extras
#
# This code was included because 'leaflet.extras' was archived on CRAN
# (on 2026-02-19), and its functionality represents a hard dependency for
# this package.
#
# It provides the Leaflet Draw features required to enable polygon drawing
# and shape editing on the map.
#
# -----------------------------------------------------------------------------

# Draw dependencies
drawDependencies <- function(drag = TRUE) {
  draw_dep <- htmltools::htmlDependency(
    "lfx-draw",
    version = "1.0.4",
    system.file(file.path("htmlwidgets", "build", "lfx-draw"), package = "wallace"),
    script = c("lfx-draw-prod.js", "lfx-draw-bindings.js"),
    stylesheet = "lfx-draw-prod.css",
    all_files = TRUE
  )

  if (drag) {
    drag_dep <- htmltools::htmlDependency(
      "lfx-draw-drag",
      version = "0.4.8",
      system.file(file.path("htmlwidgets", "build", "lfx-draw-drag"), package = "wallace"),
      script = "lfx-draw-drag-prod.js",
      all_files = TRUE
    )
    list(draw_dep, drag_dep)
  } else {
    list(draw_dep)
  }
}

#' Options for drawn shapes
#' @param stroke Whether to draw stroke along the path. Set it to false to disable borders on polygons or circles.
#' @param color Stroke color.
#' @param weight Stroke width in pixels.
#' @param opacity Stroke opacity.
#' @param fill Whether to fill the path with color. Set it to false to disable filling on polygons or circles.
#' @param fillColor Same as color. Fill color.
#' @param fillOpacity Fill opacity.
#' @param dashArray A string that defines the stroke dash pattern. Doesn't work on canvas-powered layers (e.g. Android 2).
#' @param lineCap A string that defines shape to be used at the end of the stroke.
#' @param lineJoin A string that defines shape to be used at the corners of the stroke.
#' @param clickable If false, the vector will not emit mouse events and will act as a part of the underlying map.
#' @param pointerEvents Sets the pointer-events attribute on the path if SVG backend is used.
#' @param smoothFactor How much to simplify the polyline on each zoom level. More means better performance and smoother look, and less means more accurate representation.
#' @param noClip Disabled polyline clipping.
#' @noRd
drawShapeOptions <- function(
    stroke = TRUE,
    color = "#03f",
    weight = 1,
    opacity = 1,
    fill = TRUE,
    fillColor = "#03f",
    fillOpacity = 0.4,
    dashArray = NULL,
    lineCap = NULL,
    lineJoin = NULL,
    clickable = TRUE,
    pointerEvents = NULL,
    smoothFactor = 1.0,
    noClip = TRUE
) {
  leaflet::filterNULL(list(
    stroke = stroke,
    color = color,
    weight = weight,
    opacity = opacity,
    fill = fill,
    fillColor = fillColor,
    fillOpacity = fillOpacity,
    dashArray = dashArray,
    lineCap = lineCap,
    lineJoin = lineJoin,
    clickable = clickable,
    pointerEvents = pointerEvents,
    smoothFactor = smoothFactor,
    noClip = noClip
  ))
}

#' Options for drawing polylines
#' @param allowIntersection Determines if line segments can cross.
#' @param drawError Configuration options for the error that displays if an intersection is detected.
#' @param guidelineDistance Distance in pixels between each guide dash.
#' @param maxGuideLineLength Maximum length of the guide lines.
#' @param showLength Whether to display the distance in the tooltip.
#' @param metric Determines which measurement system (metric or imperial) is used.
#' @param feet When not metric, use feet instead of yards for display.
#' @param nautic When not metric, not feet, use nautic mile for display.
#' @param zIndexOffset This should be a high number to ensure that you can draw over all other layers on the map.
#' @param shapeOptions Leaflet Polyline options. See \code{drawShapeOptions()}.
#' @param repeatMode Determines if the draw tool remains enabled after drawing a shape.
#' @noRd
drawPolylineOptions <- function(
    allowIntersection = TRUE,
    drawError = list(color = "#b00b00", timeout = 2500),
    guidelineDistance = 20,
    maxGuideLineLength = 4000,
    showLength = TRUE,
    metric = TRUE,
    feet = TRUE,
    nautic = FALSE,
    zIndexOffset = 2000,
    shapeOptions = drawShapeOptions(fill = FALSE),
    repeatMode = FALSE
) {
  leaflet::filterNULL(list(
    allowIntersection = allowIntersection,
    drawError = drawError,
    guidelineDistance = guidelineDistance,
    maxGuideLineLength = maxGuideLineLength,
    showLength = showLength,
    metric = metric,
    feet = feet,
    nautic = nautic,
    zIndexOffset = zIndexOffset,
    shapeOptions = shapeOptions,
    repeatMode = repeatMode
  ))
}

#' Options for drawing polygons
#' @param showArea Show the area of the drawn polygon in m², ha or km². The area is only approximate and become less accurate the larger the polygon is.
#' @param metric Determines which measurement system (metric or imperial) is used.
#' @param shapeOptions Shape options. See \code{drawShapeOptions()}.
#' @param repeatMode Determines if the draw tool remains enabled after drawing a shape.
#' @noRd
drawPolygonOptions <- function(
    showArea = FALSE,
    metric = TRUE,
    shapeOptions = drawShapeOptions(),
    repeatMode = FALSE
) {
  leaflet::filterNULL(list(
    showArea = showArea,
    metric = metric,
    shapeOptions = shapeOptions,
    repeatMode = repeatMode
  ))
}

#' Options for drawing rectangles
#' @param showArea Show the area of the drawn rectangle in m², ha or km².
#' @param metric Determines which measurement system (metric or imperial) is used.
#' @param shapeOptions Shape options. See \code{drawShapeOptions()}.
#' @param repeatMode Determines if the draw tool remains enabled after drawing a shape.
#' @noRd
drawRectangleOptions <- function(
    showArea = TRUE,
    metric = TRUE,
    shapeOptions = drawShapeOptions(),
    repeatMode = FALSE
) {
  leaflet::filterNULL(list(
    showArea = showArea,
    metric = metric,
    shapeOptions = shapeOptions,
    repeatMode = repeatMode
  ))
}

#' Options for drawing Circles
#' @param showRadius Show the radius of the drawn circle in m, km, ft (feet), or nm (nautical mile).
#' @param metric Determines which measurement system (metric or imperial) is used.
#' @param feet When not metric, use feet instead of yards for display.
#' @param nautic When not metric, not feet, use nautic mile for display.
#' @param shapeOptions Shape options. See \code{drawShapeOptions()}.
#' @param repeatMode Determines if the draw tool remains enabled after drawing a shape.
#' @noRd
drawCircleOptions <- function(
    showRadius = TRUE,
    metric = TRUE,
    feet = TRUE,
    nautic = FALSE,
    shapeOptions = drawShapeOptions(),
    repeatMode = FALSE
) {
  leaflet::filterNULL(list(
    shapeOptions = shapeOptions,
    repeatMode = repeatMode,
    showRadius = showRadius,
    metric = metric,
    feet = feet,
    nautic = nautic
  ))
}

#' Options for drawing markers
#' @param markerIcon Can be either \code{\link[leaflet]{makeIcon}}() OR \code{\link[leaflet]{makeAwesomeIcon}}()
#' @param zIndexOffset This should be a high number to ensure that you can draw over all other layers on the map.
#' @param repeatMode Determines if the draw tool remains enabled after drawing a shape.
#' @noRd
drawMarkerOptions <- function(
    markerIcon = NULL,
    zIndexOffset = 2000,
    repeatMode = FALSE
) {
  leaflet::filterNULL(list(
    markerIcon = markerIcon,
    zIndexOffset = zIndexOffset,
    repeatMode = repeatMode
  ))
}

#' Options for drawing circle markers
#' @param stroke Whether to draw stroke along the path.
#' @param color Stroke color.
#' @param weight Stroke width in pixels.
#' @param opacity Stroke opacity.
#' @param fill Whether to fill the path with color.
#' @param fillColor Fill color.
#' @param fillOpacity Fill opacity.
#' @param clickable If false, the vector will not emit mouse events.
#' @param zIndexOffset This should be a high number to ensure that you can draw over all other layers on the map.
#' @param repeatMode Determines if the draw tool remains enabled after drawing a shape.
#' @noRd
drawCircleMarkerOptions <- function(
    stroke = TRUE,
    color = "#3388ff",
    weight = 4,
    opacity = 0.5,
    fill = TRUE,
    fillColor = NULL,
    fillOpacity = 0.2,
    clickable = TRUE,
    zIndexOffset = 2000,
    repeatMode = FALSE
) {
  leaflet::filterNULL(list(
    stroke = stroke,
    color = color,
    weight = weight,
    opacity = opacity,
    fill = fill,
    fillColor = fillColor,
    fillOpacity = fillOpacity,
    clickable = clickable,
    zIndexOffset = zIndexOffset,
    repeatMode = repeatMode
  ))
}

#' Options for path when in editMode
#' @param dashArray A string that defines the stroke dash pattern.
#' @param weight Stroke width in pixels.
#' @param color Stroke color.
#' @param fill Whether to fill the path with color.
#' @param fillColor Fill color.
#' @param fillOpacity Fill opacity.
#' @param maintainColor Whether to maintain shape's original color.
#' @noRd
selectedPathOptions <- function(
    dashArray = c("10, 10"),
    weight = 2,
    color = "black",
    fill = TRUE,
    fillColor = "black",
    fillOpacity = 0.6,
    maintainColor = FALSE
) {
  leaflet::filterNULL(list(
    dashArray = dashArray,
    weight = weight,
    color = color,
    fill = fill,
    fillColor = fillColor,
    fillOpacity = fillOpacity,
    maintainColor = maintainColor
  ))
}

#' Options for editing shapes
#' @param edit Editing enabled by default. Set to false do disable editing.
#' @param remove Set to false to disable removing.
#' @param selectedPathOptions To customize shapes in editing mode pass \code{selectedPathOptions()}.
#' @param allowIntersection Determines if line segments can cross.
#' @export
#' @keywords internal
editToolbarOptions <- function(
    edit = TRUE,
    remove = TRUE,
    selectedPathOptions = NULL,
    allowIntersection = TRUE
) {
  leaflet::filterNULL(list(
    edit = edit,
    remove = remove,
    selectedPathOptions = selectedPathOptions,
    allowIntersection = allowIntersection
  ))
}

#' Options for editing handlers
#' @description Customize tooltips for \code{addDrawToolbar()}
#' @param polyline List of options for polyline tooltips.
#' @param polygon List of options for polygon tooltips.
#' @param rectangle List of options for rectangle tooltips.
#' @param circle List of options for circle tooltips.
#' @param marker List of options for marker tooltips.
#' @param circlemarker List of options for circlemarker tooltips.
#' @param simpleshape List of options for simpleshape tooltips.
#' @noRd
handlersOptions <- function(
    polyline = list(
      error = "<strong>Error:</strong> shape edges cannot cross!",
      tooltipStart = "Click to start drawing line.",
      tooltipCont = "Click to start drawing line.",
      tooltipEnd = "Click to start drawing line."
    ),
    polygon = list(
      tooltipStart = "Click to start drawing shape.",
      tooltipCont = "Click to start drawing shape.",
      tooltipEnd = "Click to start drawing shape."
    ),
    rectangle = list(
      tooltipStart = "Click and drag to draw rectangle."
    ),
    circle = list(
      tooltipStart = "Click map to place circle marker.",
      radius = "Radius"
    ),
    marker = list(
      tooltipStart = "Click map to place marker."
    ),
    circlemarker = list(
      tooltipStart = "Click and drag to draw circle."
    ),
    simpleshape = list(
      tooltipEnd = "Release mouse to finish drawing."
    )
) {
  leaflet::filterNULL(list(
    polyline = list(
      error = polyline$error,
      tooltip = list(
        start = polyline$tooltipStart,
        cont = polyline$tooltipCont,
        end = polyline$tooltipEnd
      )
    ),
    polygon = list(
      tooltip = list(
        start = polygon$tooltipStart,
        cont = polygon$tooltipCont,
        end = polygon$tooltipEnd
      )
    ),
    rectangle = list(tooltip = list(start = rectangle$tooltipStart)),
    circle = list(
      radius = circle$radius,
      tooltip = list(start = circle$tooltipStart)
    ),
    marker = list(tooltip = list(start = marker$tooltipStart)),
    circlemarker = list(tooltip = list(start = circlemarker$tooltipStart)),
    simpleshape = list(tooltip = list(end = simpleshape$tooltipEnd))
  ))
}

#' Options for editing the toolbar
#' @description Customize the toolbar for \code{addDrawToolbar()}
#' @param actions List of options for actions toolbar button.
#' @param finish List of options for finish toolbar button.
#' @param undo List of options for undo toolbar button.
#' @param buttons List of options for buttons toolbar button.
#' @noRd
toolbarOptions <- function(
    actions = list(
      title = "Cancel drawing",
      text = "Cancel"
    ),
    finish = list(
      title = "Finish drawing",
      text = "Finish"
    ),
    undo = list(
      title = "Delete last point drawn",
      text = "Delete last point"
    ),
    buttons = list(
      polyline = "Draw a polyline",
      polygon = "Draw a polygon",
      rectangle = "Draw a rectangle",
      circle = "Draw a circle",
      marker = "Draw a marker",
      circlemarker = "Draw a circlemarker"
    )
) {
  leaflet::filterNULL(list(
    actions = list(
      title = actions$title,
      text = actions$text
    ),
    finish = list(
      title = finish$title,
      text = finish$text
    ),
    undo = list(
      title = undo$title,
      text = undo$text
    ),
    buttons = list(
      polyline = buttons$polyline,
      polygon = buttons$polygon,
      rectangle = buttons$rectangle,
      circle = buttons$circle,
      marker = buttons$marker,
      circlemarker = buttons$circlemarker
    )
  ))
}

#' Options for editing edit handlers
#' @description Customize edit handlers for \code{addDrawToolbar()}
#' @param edit List of options for editing tooltips.
#' @param remove List of options for removing tooltips.
#' @noRd
edithandlersOptions <- function(
    edit = list(
      tooltipText = "Drag handles or markers to edit features.",
      tooltipSubtext = "Click cancel to undo changes."
    ),
    remove = list(
      tooltipText = "Click on a feature to remove."
    )
) {
  leaflet::filterNULL(list(
    edit = list(
      tooltip = list(
        text = edit$tooltipText,
        subtext = edit$tooltipSubtext
      )
    ),
    remove = list(
      tooltip = list(
        text = remove$tooltipText
      )
    )
  ))
}

#' Options for editing the toolbar
#' @description Customize the edit toolbar for \code{addDrawToolbar()}
#' @param actions List of options for edit action tooltips.
#' @param buttons List of options for edit button tooltips.
#' @noRd
edittoolbarOptions <- function(
    actions = list(
      save = list(
        title = "Save changes",
        text = "Save"
      ),
      cancel = list(
        title = "Cancel editing, discards all changes",
        text = "Cancel"
      ),
      clearAll = list(
        title = "Clear all layers",
        text = "Clear All"
      )
    ),
    buttons = list(
      edit = "Edit layers",
      editDisabled = "No layers to edit",
      remove = "Delete layers",
      removeDisabled = "No layers to delete"
    )
) {
  leaflet::filterNULL(list(
    actions = actions,
    buttons = buttons
  ))
}

#' Adds a Toolbar to draw shapes/points on the map
#' @param map The map widget.
#' @param targetLayerId An optional layerId of a GeoJSON/TopoJSON layer whose features need to be editable.
#'  Used for adding a GeoJSON/TopoJSON layer and then editing the features using the draw plugin.
#' @param targetGroup An optional group name of a Feature Group whose features need to be editable.
#'  Used for adding shapes(markers, lines, polygons) and then editing them using the draw plugin.
#'  You can either set layerId or group or none but not both.
#' @param position The position where the toolbar should appear.
#' @param polylineOptions See \code{drawPolylineOptions()}. Set to FALSE to disable polyline drawing.
#' @param polygonOptions See \code{drawPolygonOptions()}. Set to FALSE to disable polygon drawing.
#' @param circleOptions See \code{drawCircleOptions()}. Set to FALSE to disable circle drawing.
#' @param rectangleOptions See \code{drawRectangleOptions()}. Set to FALSE to disable rectangle drawing.
#' @param markerOptions See \code{drawMarkerOptions()}. Set to FALSE to disable marker drawing.
#' @param circleMarkerOptions See \code{drawCircleMarkerOptions()}. Set to FALSE to disable circle marker drawing.
#' @param editOptions By default editing is disable. To enable editing pass \code{editToolbarOptions()}.
#' @param singleFeature When set to TRUE, only one feature can be drawn at a time, the previous ones being removed.
#' @param toolbar See \code{toolbarOptions()}. Set to \code{NULL} to take Leaflets default values.
#' @param handlers See \code{handlersOptions()}. Set to \code{NULL} to take Leaflets default values.
#' @param edittoolbar See \code{edittoolbarOptions()}. Set to \code{NULL} to take Leaflets default values.
#' @param edithandlers See \code{edithandlersOptions()}. Set to \code{NULL} to take Leaflets default values.
#' @param drag When set to \code{TRUE}, the drawn features will be draggable during editing, utilizing
#'    the \code{Leaflet.Draw.Drag} plugin. Otherwise, this library will not be included.
#'
#' @details
#' The drawn features emit events upon mouse interaction.
#' Event names follow the pattern: \code{input$MAPID_LAYERCATEGORY_EVENTNAME},
#' where \code{LAYERCATEGORY} can be one of:
#' \itemize{
#'   \item \code{marker}
#'   \item \code{shape}
#'   \item \code{polyline}
#' }
#'
#' Similarly, for \code{EVENTNAME}, valid values are:
#' \itemize{
#'   \item \code{click}
#'   \item \code{mouseover}
#'   \item \code{mouseout}
#' }
#' @export
#' @keywords internal
addDrawToolbar <- function(
    map, targetLayerId = NULL, targetGroup = NULL,
    position = c("topleft", "topright", "bottomleft", "bottomright"),
    polylineOptions = drawPolylineOptions(),
    polygonOptions = drawPolygonOptions(),
    circleOptions = drawCircleOptions(),
    rectangleOptions = drawRectangleOptions(),
    markerOptions = drawMarkerOptions(),
    circleMarkerOptions = drawCircleMarkerOptions(),
    editOptions = FALSE,
    singleFeature = FALSE,
    toolbar = NULL,
    handlers = NULL,
    edittoolbar = NULL,
    edithandlers = NULL,
    drag = TRUE
) {
  if (!is.null(targetGroup) && !is.null(targetLayerId)) {
    stop("To edit existing features either specify a targetGroup or a targetLayerId, but not both")
  }

  if (!inherits(toolbar, "list")) toolbar <- NULL
  if (!inherits(handlers, "list")) handlers <- NULL
  if (!inherits(edittoolbar, "list")) edittoolbar <- NULL
  if (!inherits(edithandlers, "list")) edithandlers <- NULL

  map$dependencies <- c(map$dependencies, drawDependencies(drag))

  markerIconFunction <- NULL
  if (inherits(markerOptions, "list") && !is.null(markerOptions$markerIcon)) {
    stop("markerIcon is not supported in this version. Please use standard leaflet markers instead.")
  }

  position <- match.arg(position)

  options <- list(
    position = position,
    draw = leaflet::filterNULL(list(
      polyline = polylineOptions,
      polygon = polygonOptions,
      circle = circleOptions,
      rectangle = rectangleOptions,
      marker = markerOptions,
      circlemarker = circleMarkerOptions,
      singleFeature = singleFeature
    )),
    edit = editOptions,
    toolbar = toolbar,
    handlers = handlers,
    edittoolbar = edittoolbar,
    edithandlers = edithandlers
  )

  leaflet::invokeMethod(
    map, leaflet::getMapData(map), "addDrawToolbar",
    targetLayerId, targetGroup, options
  )
}

#' Removes the draw toolbar
#' @param map The map widget.
#' @param clearFeatures Whether to clear the map of drawn features.
#' @export
#' @keywords internal
removeDrawToolbar <- function(map, clearFeatures = FALSE) {
  leaflet::invokeMethod(map, leaflet::getMapData(map), "removeDrawToolbar", clearFeatures)
}
