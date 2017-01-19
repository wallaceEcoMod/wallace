comp5_setPartitions <- function(partSelect2, kfolds, aggFact, proxy) {
  occs <- values$df[,2:3]

  if (partSelect2 == 'block') {
    pt <- 'block'
    group.data <- ENMeval::get.block(occs, values$bg.coords)
    writeLog("> Data partition by block method.")
  }
  if (partSelect2 == 'cb1') {
    pt <- "checkerboard 1"
    group.data <- ENMeval::get.checkerboard1(occs, values$predsMsk, values$bg.coords, aggFact)
    writeLog("> Data partition by checkerboard 1 method.")
  }
  if (partSelect2 == 'cb2') {
    pt <- "checkerboard 2"
    group.data <- ENMeval::get.checkerboard2(occs, values$predsMsk, values$bg.coords, aggFact)
    writeLog("> Data partition by checkerboard 2 method.")
  }
  if (partSelect2 == 'jack') {
    pt <- "jackknife"
    group.data <- ENMeval::get.jackknife(occs, values$bg.coords)
    writeLog("> Data partition by jackknife method.")
  }
  if (partSelect2 == 'random') {
    pt <- paste0("random k-fold (k = ", kfolds, ")")
    group.data <- ENMeval::get.randomkfold(occs, values$bg.coords, kfolds)
    writeLog(paste("> Data partition by", paste0("random k-fold (k = ", kfolds, ")"), ":"))
  }

  values$modParams <- list(occ.pts=occs, bg.pts=values$bg.coords, occ.grp=group.data[[1]], bg.grp=group.data[[2]])
  #newColors <- brewer.pal(max(group.data[[1]]), 'Accent')
  #     values$df$parts <- factor(group.data[[1]])
  #     newColors <- colorFactor(rainbow(max(group.data[[1]])), values$df$parts)
  #     fillColor = ~newColors(parts)
  newColors <- gsub("FF$", "", rainbow(max(group.data[[1]])))  # colors for partition symbology
  values$partFill <- newColors[group.data[[1]]]
  #newColors <- sample(colors(), max(group.data[[1]]))
  proxy %>% map_plotLocs(values$df, fillColor = values$partFill, fillOpacity = 1)
    # addCircleMarkers(data = values$df, lat = ~latitude, lng = ~longitude,
    #                          radius = 5, color = 'red', fill = TRUE, fillColor = values$partFill,
    #                          weight = 2, popup = ~pop, fillOpacity = 1, group = 'comp5')
}

# comp5_userPartitions <- function(partSelect, occ.grp, bg.grp) {
#   # if user kfold, get groups and assign occs and backg from inFile,
#   # and if not, make backg pts and assign user kfold groups to NULL
#   occs <- values$inFile[values$inFile[,1] == values$spname,]
#   bg.coords <- values$inFile[values$inFile[,1] != values$spname,]
#   group.data <- list()
#   group.data[[1]] <- as.numeric(occs[,occ.grp])
#   group.data[[2]] <- as.numeric(backg_pts[,bg.grp])
#   occs <- occs[,2:3]
#   values$bg.coords <- backg_pts[,2:3]
# }
