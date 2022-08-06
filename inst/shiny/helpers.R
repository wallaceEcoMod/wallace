uiTop <- function(mod_INFO) {
  modID <- mod_INFO$modID
  modName <- mod_INFO$modName
  pkgName <- mod_INFO$pkgName

  ls <- list(div(paste("Module: ", modName), class = "mod"),
             actionLink(paste0(modID, "Help"),
                        label = "", icon = icon("circle-question"),
                        class = "modHelpButton"),
             br())

  ls <- c(ls, list(span("R packages:", class = "rpkg"),
                   span(paste(pkgName, collapse = ", "), class = "pkgDes"),
                   br()))
  # for (i in seq_along(pkgName)) {
  #   ls <- c(ls, list(span(pkgName[i], class = "rpkg"),
  #                    span(paste(':', pkgTitl[i]), class = "pkgDes"),
  #                    br()))
  # }

  ls <- c(ls, list(HTML('<hr>')))
  ls
}

uiBottom <- function(mod_INFO) {
  modAuts <- mod_INFO$modAuts
  pkgName <- mod_INFO$pkgName
  pkgAuts <- mod_INFO$pkgAuts
  pkgTitl <- mod_INFO$pkgTitl

  ls <- list(span('Module Developers:', class = "rpkg"),
             span(modAuts, class = "pkgDes"), br(), br())

  for (i in seq_along(pkgName)) {
    ls <- c(ls, list(
      span(pkgName[i], class = "rpkg"),
      "references", br(),
      div(paste(pkgTitl[i]), class = "pkgTitl"),
      div(paste('Package Developers:', pkgAuts[i]), class = "pkgDes"),
      a("CRAN", href = file.path("http://cran.r-project.org/web/packages",
                                 pkgName[i], "index.html"), target = "_blank"), " | ",
      a("documentation", href = file.path("https://cran.r-project.org/web/packages",
                                          pkgName[i], paste0(pkgName[i], ".pdf")), target = "_blank"), br()
    ))
  }
  ls
}

ui_top <- function(pkgName, modName, modAuts, modID) {
  uiTop(infoGenerator(pkgName, modName, modAuts, modID))
}
ui_bottom <- function(pkgName, modName, modAuts, modID) {
  uiBottom(infoGenerator(pkgName, modName, modAuts, modID))
}

infoGenerator <- function(pkgName, modName, modAuts, modID) {
  # Use installed package only (some packages are Suggested)
  pkgName <- pkgName[vapply(pkgName, requireNamespace, TRUE, quietly = TRUE)]

  pkgInfo <- sapply(pkgName, packageDescription, simplify = FALSE)
  pkgTitl <- sapply(pkgInfo, function(x) x$Title)
  # remove square brackets and spaces before commas
  pkgAuts <- sapply(pkgInfo, function(x) gsub("\\s+,", ",", gsub("\n|\\[.*?\\]", "", x$Author)))
  # remove parens and spaces before commas
  pkgAuts <- sapply(pkgAuts, function(x) gsub("\\s+,", ",", gsub("\\(.*?\\)", "", x)))
  list(modID = modID,
       modName = modName,
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
        modID = module$id,
        modName = module$long_name,
        modAuts = module$authors,
        pkgName = module$package
      ),
      do.call(module$ui_function, list(module$id)),
      tags$hr(),
      ui_bottom(
        modID = module$id,
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

# Add helper button for component
help_comp_ui <- function(name) {
  actionLink(name, label = "", icon = icon("circle-question"),
             class = "compHelpButton")
}
