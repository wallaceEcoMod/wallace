uiTop <- function(mod_INFO) {
  modName <- mod_INFO$modName
  pkgName <- mod_INFO$pkgName
  pkgTitl <- mod_INFO$pkgTitl

  ls <- list(div(paste("Module: ", modName), class = "mod"))

  for (i in seq_along(pkgName)) {
    ls <- c(ls, list(span(pkgName[i], class = "rpkg"),
                     span(paste(':', pkgTitl[i]), class = "pkgDes"),
                     br()))
  }

  ls <- c(ls, list(HTML('<hr>')))
  ls
}

uiBottom <- function(mod_INFO) {
  modAuts <- mod_INFO$modAuts
  pkgName <- mod_INFO$pkgName
  pkgAuts <- mod_INFO$pkgAuts

  ls <- list(div(paste('Module Developers:', modAuts), class = "pkgDes"))

  for (i in seq_along(pkgName)) {
    ls <- c(ls, list(
      span(pkgName[i], class = "rpkg"), "references", br(),
      div(paste('Package Developers:', pkgAuts[i]), class = "pkgDes"),
      a("CRAN", href = file.path("http://cran.r-project.org/web/packages",
                                 pkgName[i], "index.html"), target = "_blank"), " | ",
      a("documentation", href = file.path("https://cran.r-project.org/web/packages",
                                          pkgName[i], paste0(pkgName[i], ".pdf")), target = "_blank"), br()
    ))
  }
  ls
}

ui_top <- function(pkgName, modName , modAuts) {
  uiTop(infoGenerator(pkgName, modName , modAuts))
}
ui_bottom <- function(pkgName, modName , modAuts) {
  uiBottom(infoGenerator(pkgName, modName , modAuts))
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

# Join a string vector into a single string separated by commas
join <- function(v) paste(v, collapse = ", ")

# Add radio buttons for all modules in a component
insert_modules_options <- function(component) {
  unlist(setNames(
    lapply(COMPONENT_MODULES[[component]], `[[`, "id"),
    lapply(COMPONENT_MODULES[[component]], `[[`, "short_name")
  ))
}

# Add the UI for a module
insert_modules_ui <- function(component) {
  lapply(COMPONENT_MODULES[[component]], function(module) {
    conditionalPanel(
      glue("input.{component}Sel == '{module$id}'"),
      ui_top(
        modName = module$long_name,
        modAuts = module$authors,
        pkgName = module$package
      ),
      do.call(module$ui_function, list(module$id)),
      tags$hr(),
      ui_bottom(
        modName = module$long_name,
        modAuts = module$authors,
        pkgName = module$package
      )
    )
  })
}

# Add the results section UI of all modules in a component
insert_modules_results <- function(component) {
  lapply(COMPONENT_MODULES[[component]], function(module) {
    if (is.null(module$result_function)) return()
    conditionalPanel(
      glue("input.{component}Sel == '{module$id}'"),
      do.call(module$result_function, list(module$id))
    )
  })
}
