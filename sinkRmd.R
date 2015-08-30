# Functions used for the Rmd files.


# Function to generate the file and title of the .Rmd file
sinkRmdTitle <- function(title = "Code description") {
  sink(file = "temp.Rmd")
  cat("---\n")
  cat(paste("title:", paste("\'", title, "\'", sep ="")))
  cat("\noutput:")
  cat("\n html_document:")
  cat("\n  toc: true")
  cat("\n---")
  cat("\n\n## About")
  cat("\n\nThis document describes the R code used in Wallace.")
  cat("\n\n## Packages instalation")
  cat("\n\nBefore start, install the following packages:")
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
  cat("\n\nNow load them:")
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
  cat("\n\nLoad the functions necessary to run the analysis:")
  cat("\n```{r}\n")
  cat("source('functions.R')")
  cat("\n```")
  sink()
}


# Function to apply for the code of Wallace to generate the .Rmd file
sinkRmd <- function(x, coment) {
  x
  call.line <- as.character(substitute(x))
  call.line[1:2] <- call.line[2:1]
  call.line <- gsub("input\\$", "", call.line)
  call.line <- gsub("values\\$", "", call.line)
  sink(file = "temp.Rmd", append = TRUE)
  cat("\n\n")
  cat(coment)
  cat("\n```{r, warning = FALSE, message = FALSE}\n")
  cat(call.line)
  cat("\n```")
  sink()
}


# Multiple
sinkRmdmult <- function(x, coment) {
  x
  call.line <- as.character(substitute(x))
  call.line <- call.line[-1]
  call.line <- gsub("input\\$", "", call.line)
  call.line <- gsub("values\\$", "", call.line)
  rmdprint <- paste0(call.line, "\n")
  sink(file = "temp.Rmd", append = TRUE)
  cat("\n\n")
  cat(coment, sep = "")
  cat("\n```{r, warning = FALSE, message = FALSE}\n ")
  cat(rmdprint)
  cat("\n```")
  sink()
}


# Function for variables
sinkRmdob <- function(x, coment) {
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
  cat(coment)
  cat("\n```{r}\n")
  cat(paste(call.line, "<-", x))
  cat("\n```")
  sink()
}


# False variables
sinkFalse <- function(x, coment) {
  sink(file = "temp.Rmd", append = TRUE)
  cat("\n\n")
  cat(coment, sep = "")
  cat("\n```{r, warning = FALSE, message = FALSE}\n")
  cat(x)
  cat("\n```")
  sink()
}

# Sink subtitles
sinkSub <- function(coment) {
  sink(file = "temp.Rmd", append = TRUE)
  cat("\n\n")
  cat(coment)
  sink()
}

