[![Build Status](https://travis-ci.org/wallaceEcoMod/wallace.svg?branch=master)](https://travis-ci.org/wallaceEcoMod/wallace) [![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0) [![CRAN version](http://www.r-pkg.org/badges/version/wallace)](https://CRAN.R-project.org/package=wallace) [![downloads](http://cranlogs.r-pkg.org/badges/grand-total/wallace?color=orange)](http://cranlogs.r-pkg.org/badges/grand-total/wallace?color=orange)

# Wallace (v1.0.1)

*Wallace* is a modular platform for reproducible modeling of species niches and distributions, written in R. The application guides users through a complete analysis, from the acquisition of data to visualizing model predictions on an interactive map, thus bundling complex workflows into a single, streamlined interface.

Install *Wallace* via CRAN and run the application with the following R code.

```R
install.packages("wallace")
library(wallace)
run_wallace()
```

In the future, development versions can be downloaded from Github with the following R code. For now, please install via CRAN.

```R
install.packages("devtools")
devtools::install_github("wallaceEcoMod/wallace")
library(wallace)
run_wallace()
```

### Before using *Wallace*

#### Update R and RStudio versions
Please make sure you have installed the latest versions of both R (<a href= "https://cran.r-project.org/bin/macosx/" target="_blank">Mac OS</a>, <a href= "https://cran.r-project.org/bin/windows/base/" target="_blank">Windows</a>) and RStudio (<a href= "https://www.rstudio.com/products/rstudio/download3/" target="_blank">Mac OS /  Windows</a>: choose the free version).

#### Download maxent.jar
*Wallace* uses the `maxent()` function in the package `dismo`. This function requires the user to place the `maxent.jar` file in the `/java` directory of the `dismo` package root folder. You can download Maxent <a href="https://www.cs.princeton.edu/~schapire/maxent/" target="_blank">here</a>, and locate `maxent.jar`, which is the Maxent program itself, in the downloaded folder. You can find the directory path to `dismo/java` by running `system.file('java', package="dismo")` at the R console. Simply copy `maxent.jar` and paste it into this folder. If you try to run Maxent in *Wallace* without the file in place, you will get a warning message in the log window and Maxent will not run.

### Potential Issues

#### rJava and Java versions
*Wallace* uses the `rJava` package to run the program `maxent.jar`. The package `rJava` will not load properly if the version of Java on your computer (32-bit or 64-bit) does not match that of the R installation you are using. For example, if you are running 64-bit R, please make sure your Java is also 64-bit, or else `rJava` will be unable to load. Install the latest version of Java <a href="https://java.com/en/download/manual.jsp" target="_blank">here</a>, and 64-bit Windows users should make sure to select "Windows Offline (64-bit)". There is currently only a 64-bit download for Mac OS. For Mac users running OSX Yosemite and above with problems, see <a href="http://stackoverflow.com/questions/30738974/rjava-load-error-in-rstudio-r-after-upgrading-to-osx-yosemite" target="_blank">this StackOverflow post</a> for some tips on how to get `rJava` working again.

#### Problems viewing tables
If for some reason you are unable to view the tables in *Wallace*, please install (force if necessary) the development version of `htmlwidgets` by running this code: `devtools::install_github("ramnathv/htmlwidgets")`. You should be able to view tables now.

#### Windows Users: PDF download of session code
If PDF downloading of session code is not working for you, please follow the following instructions, taken from <a href="https://github.com/rstudio/shiny-examples/issues/34" target="_blank">here</a>:
     - Step 1: Download and Install MiKTeX from http://miktex.org/2.9/setup
     - Step 2: Run `Sys.getenv("PATH")` in R studio. This command returns the path where Rstudio is trying to find pdflatex.exe. In Windows (64-bit), it should return "C:\Program Files\MiKTeX 2.9\miktex\bin\x64\pdflatex.exe". If pdflatex.exe is not located in this location Rstudio gives this error code 41.
     - Step 3: To set this path variable run: `Sys.setenv(PATH=paste(Sys.getenv("PATH"),"C:/Program Files/MiKTeX 2.9/miktex/bin/x64/",sep=";"))`.

#### Windows Users: Only for Github installation
 If you are using Windows, please download and install <a href="https://cran.r-project.org/bin/windows/Rtools/" target="_blank">RTools</a> before installing the `devtools` package. After you install RTools, please make sure you add "C:\Rtools\bin" to your PATH variable (instructions <a href="http://stackoverflow.com/a/29480538/1274346" target="_blank">here</a>). Additionally, when using `devtools` on Windows machines, there is a known <a href="https://github.com/hadley/devtools/issues/1298" target="_blank">bug</a> that sometimes results in the inability to download all package dependencies. If this happens to you, please run [this script](wallace_pkgs.R) to install the packages and their dependencies directly from CRAN, and then run the code below.

#### Any other problems with install_github()
Although the recommended way to install is through CRAN, if you are trying to install the Github version and are having problems, follow these steps.
 1. Download the zip file from the repository page.
 2. Unzip and open the wallace.Rproj file in RStudio.
 3. In the right-hand pane, click Build, then Build & Reload.
 4. Type `run_wallace()` in the console and press Enter.
