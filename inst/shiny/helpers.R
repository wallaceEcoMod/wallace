uiTop <- function(mod_INFO) {
  modName <- mod_INFO$modName
  pkgName <- mod_INFO$pkgName
  pkgTitl <- mod_INFO$pkgTitl

  ls <- list(div(paste("Module: ", modName), id="mod"))

  if(!is.null(pkgName)) {
    for(i in 1:length(pkgName)) {
      ls <- c(ls, list(span(pkgName[i], id="rpkg"),
                       span(paste(':', pkgTitl[i]), id="pkgDes"),
                       br()))
    }
  }

  ls <- c(ls, list(HTML('<hr>')))
  return(ls)
}

uiBottom <- function(mod_INFO) {
  modAuts <- mod_INFO$modAuts
  pkgName <- mod_INFO$pkgName
  pkgAuts <- mod_INFO$pkgAuts

  ls <- list(div(paste('Module Developers:', modAuts), id="pkgDes"))

  if(!is.null(pkgName)) {
    for(i in 1:length(pkgName)) {
      ls <- c(ls, list(span(pkgName[i], id = "rpkg"), "references", br(),
                       div(paste('Package Developers:', pkgAuts[i]), id="pkgDes"),
                       a("CRAN", href = file.path("http://cran.r-project.org/web/packages",
                                                  pkgName[i], "index.html"), target = "_blank"), " | ",
                       a("documentation", href = file.path("https://cran.r-project.org/web/packages",
                                                           pkgName[i], paste0(pkgName[i], ".pdf")), target = "_blank"), br()))
    }
  }
  return(ls)
}

infoGenerator <- function(pkgName, modName , modAuts) {
  pkgInfo <- sapply(pkgName, packageDescription, simplify = FALSE)
  pkgTitl <- sapply(pkgInfo, function(x) x$Title)
  # remove square brackets and spaces before commas
  pkgAuts <- sapply(pkgInfo, function(x) gsub("\\s+,", ",", gsub("\n|\\[.*?\\]", "", x$Author)))
  # remove parens and spaces before commas
  pkgAuts <- sapply(pkgAuts, function(x) gsub("\\s+,", ",", gsub("\\(.*?\\)", "", x)))
  list(modName = modName,
       modAuts = modAuts,
       pkgName = pkgName,
       pkgTitl = pkgTitl,
       pkgAuts = pkgAuts)
}
