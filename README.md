[![Open Source Love](https://badges.frapsoft.com/os/v2/open-source.svg?v=103)](https://github.com/ellerbrock/open-source-badge/)   [![GPL Licence](https://badges.frapsoft.com/os/gpl/gpl.svg?v=103)](https://opensource.org/licenses/GPL-3.0/)  

# Wallace (v0.6)

*Wallace* is a modular platform for reproducible modeling of species niches and distributions, written in R. The application guides users through a complete analysis, from the acquisition of data to visualizing model predictions on an interactive map, thus bundling complex workflows into a single, streamlined interface.

*Wallace* can be installed via Github by executing the following R code. Please note the following:

1. Before installing, to ensure a smooth Wallace experience, please make sure you have installed the latest versions of both R and RStudio.

2. ***WINDOWS USERS***: 
 2. If you are using a Windows machine, please download and install [RTools](https://cran.r-project.org/bin/windows/Rtools/) before installing the `devtools` package. After you install RTools, please make sure you add "C:\Rtools\bin" to your PATH variable (instructions <a href="http://stackoverflow.com/a/29480538/1274346" target="_blank">here</a>). 
  2. When using `devtools` on Windows machines, there is a known <a href="https://github.com/hadley/devtools/issues/1298" target="_blank">bug</a> that sometimes results in the inability to download all package dependencies. If this happens to you, please run the file "wallace_pkgs.R" in the main directory of the package to install the packages and their dependencies directly from CRAN, and then run the code below.
   2. If PDF downloading of session code is not working for you, please follow <a href="https://github.com/rstudio/shiny-examples/issues/34" target="_blank">these</a> instructions to install Miktex and set up the PATH variable, then restart your computer and give it another shot.

3. As *Wallace* uses the Java program `maxent.jar` to run Maxent via the `maxent()` function in `dismo`, there are a couple of hoops to jump through. 
 3. *Wallace* uses the package `rJava`, which requires that the version of Java on your computer (32-bit or 64-bit) match that of the R you are using. For example, if you are running 64-bit R, please make sure your Java is also 64-bit, or else `rJava` will be unable to load.
 3. *Wallace* uses the `maxent()` function in the package `dismo`. This function requires the user to place the `maxent.jar` file in the `/java` directory of the `dismo` package root folder. You can download Maxent <a href="https://www.cs.princeton.edu/~schapire/maxent/" target="_blank">here</a>, and you can find `maxent.jar`, which is the program itself, in the downloaded folder. You can find the directory path to `dismo/java` by running `system.file('java', package="dismo")` at the command line. Simply copy `maxent.jar` and paste it into this folder. If you try to run Maxent in *Wallace* without the file in place, you will get a warning message in the log window and Maxent will not run.

Once this is complete, please run the following code to install.

```R
# if you do not have devtools installed, install it first
install.packages(devtools)
# install wallace from github
devtools::install_github("wallaceEcoMod/wallace")
# load wallace
library(wallace)
# run the user interface
wallace()
```

