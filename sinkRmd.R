# Functions used for the Rmd files.


# Function to generate the file and title of the .Rmd file
sinkRmdTitle <- function(title = "Code description") {
  sink(file = "temp.Rmd")
  cat("---\n")
  cat(paste("title:", paste("\'", title, "\'", sep ="")))
  cat("\n---")
  cat("\n\n## About")
  cat("\n\nThis is an R Markdown document (more information on http://rmarkdown.rstudio.com/). Here all R code history used in the web app Wallace is registered. With this document users can track their own analysis and recreate it in R itself.")
  cat("\n\n## Packages installation")
  cat("\n\nWallace uses the following R packages that need to be installed before starting:")
  cat("\n```{r, eval = FALSE}\n")
  cat("install.packages(devtools)\n")
  cat("install.packages(rgbif)\n")
  cat("install.packages(maptools)\n")
  cat("install.packages(spThin)\n")
  cat("install.packages(ENMeval)\n")
  cat("install.packages(dismo)\n")
  cat("install.packages(rgeos)\n")
  cat("install.packages(repmis)")
  cat("\n```")
  cat("\n\nOnce installed, load them:")
  cat("\n```{r, message = FALSE}\n")
  cat("library(devtools)\n")
  cat("library(rgbif)\n")
  cat("library(maptools)\n")
  cat("library(spThin)\n")
  cat("library(ENMeval)\n")
  cat("library(dismo)\n")
  cat("library(rgeos)\n")
  cat("library(repmis)")
  cat("\n```")
  cat("\n\nWallace also includes several functions developed to help integrate different packages and
some additional small functionalities.  For this reason, it is necessary to load the file 'functions.R', which can be found on Wallace's GitHub page (https://github.com/ndimhypervol/wallace). Download the file, place it in your working directory ( use `getwd()`), and then load it:")
  cat("\n```{r}\n")
  cat("source('functions.R')")
  cat("\n```")
  sink()
}


# Function to apply for the code of Wallace to generate the .Rmd file
sinkRmd <- function(x, comment) {
  x
  call.line <- as.character(substitute(x))
  call.line[1:2] <- call.line[2:1]
  call.line <- gsub("input\\$", "", call.line)
  call.line <- gsub("values\\$", "", call.line)
  if (sum(nchar(call.line)) > 80) {
    call.line[3] <- (gsub(", ", ",\n   ", call.line[3]))
  }
  sink(file = "temp.Rmd", append = TRUE)
  cat("\n\n")
  cat(comment)
  cat("\n```{r, warning = FALSE, message = FALSE}\n")
  cat(call.line)
  cat("\n```")
  sink()
}


# Multiple
sinkRmdmult <- function(x, comment) {
  x
  call.line <- as.character(substitute(x))
  call.line <- call.line[-1]
  call.line <- gsub("input\\$", "", call.line)
  call.line <- gsub("values\\$", "", call.line)
  rmdprint <- paste0(call.line, "\n")
  for(i in 1:length(rmdprint)) {
    if (sum(nchar(rmdprint[i])) > 80) {
      rmdprint[i] <- (gsub(", ", ",\n     ", rmdprint[i]))
    }
  }
  sink(file = "temp.Rmd", append = TRUE)
  cat("\n\n")
  cat(comment, sep = "")
  cat("\n```{r, warning = FALSE, message = FALSE, fig.keep = 'none', results = 'hide'}\n ")
  cat(rmdprint)
  cat("\n```")
  sink()
}


# Function for variables
sinkRmdob <- function(x, comment) {
  char <- is.character(x)
  call.line <- as.character(substitute(x))
  if (length(call.line) > 1) {
    call.line <- call.line[3]
  }
  if (char) {
    x <- paste("'", x, "'", sep = "")
  }
  if (length(x) > 1) {
    x <- paste0("c(", paste(x, collapse = ", "), ")")
  }
  sink(file = "temp.Rmd", append = TRUE)
  cat("\n\n")
  cat(comment)
  cat("\n```{r}\n")
  cat(paste(call.line, "<-", x))
  cat("\n```")
  sink()
}


# False variables
sinkFalse <- function(x, comment) {
  sink(file = "temp.Rmd", append = TRUE)
  cat("\n\n")
  cat(comment, sep = "")
  cat("\n```{r, warning = FALSE, message = FALSE, fig.keep = 'none', results = 'hide'}\n")
  cat(x)
  cat("\n```")
  sink()
}

# Sink subtitles
sinkSub <- function(comment) {
  sink(file = "temp.Rmd", append = TRUE)
  cat("\n\n")
  cat(comment)
  sink()
}
