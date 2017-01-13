[![Open Source Love](https://badges.frapsoft.com/os/v2/open-source.svg?v=103)](https://github.com/ellerbrock/open-source-badge/)   [![GPL Licence](https://badges.frapsoft.com/os/gpl/gpl.svg?v=103)](https://opensource.org/licenses/GPL-3.0/)  

# Wallace (v0.6)

*Wallace* is a modular platform for reproducible modeling of species niches and distributions, written in R. The application guides users through a complete analysis, from the acquisition of data to visualizing model predictions on an interactive map, thus bundling complex workflows into a single, streamlined interface.

*Wallace* can be installed via Github by executing the following R code. Please note the following pre-install:

1. Before installing, to ensure a smooth Wallace experience, please make sure you have installed the latest versions of both R (<a href= "https://cran.r-project.org/bin/macosx/" target="_blank">Mac OS</a>, <a href= "https://cran.r-project.org/bin/windows/base/" target="_blank">Windows</a>) and RStudio (<a href= "https://www.rstudio.com/products/rstudio/download3/" target="_blank">Mac OS /  Windows</a>: choose the free version).

2. As *Wallace* uses the Java program `maxent.jar` to run Maxent via the `maxent()` function in `dismo`, there are a couple of hoops to jump through. 
 2. *Wallace* uses the package `rJava`, which requires that the version of Java on your computer (32-bit or 64-bit) match that of the R you are using. For example, if you are running 64-bit R, please make sure your Java is also 64-bit, or else `rJava` will be unable to load. Install the latest version of Java <a href="https://java.com/en/download/manual.jsp" target="_blank">here</a>, and 64-bit Windows users should make sure to select "Windows Offline (64-bit)". There is currently only a 64-bit download for Mac OS.

3. ***WINDOWS USERS***: 
 3. If you are using a Windows machine, please download and install <a href="https://cran.r-project.org/bin/windows/Rtools/" target="_blank">RTools</a> before installing the `devtools` package. After you install RTools, please make sure you add "C:\Rtools\bin" to your PATH variable (instructions <a href="http://stackoverflow.com/a/29480538/1274346" target="_blank">here</a>). 
  3. When using `devtools` on Windows machines, there is a known <a href="https://github.com/hadley/devtools/issues/1298" target="_blank">bug</a> that sometimes results in the inability to download all package dependencies. If this happens to you, please run [this script](wallace_pkgs.R) to install the packages and their dependencies directly from CRAN, and then run the code below.
   3. If PDF downloading of session code is not working for you, please follow the following instructions, taken from <a href="https://github.com/rstudio/shiny-examples/issues/34" target="_blank">here</a>:
     - Step 1: Download and Install MiKTeX from http://miktex.org/2.9/setup
     - Step 2: Run `Sys.getenv("PATH")` in R studio. This command returns the path where Rstudio is trying to find pdflatex.exe. In Windows (64-bit), it should return "C:\Program Files\MiKTeX 2.9\miktex\bin\x64\pdflatex.exe". If pdflatex.exe is not located in this location Rstudio gives this error code 41.
     - Step 3: To set this path variable run: `Sys.setenv(PATH=paste(Sys.getenv("PATH"),"C:/Program Files/MiKTeX 2.9/miktex/bin/x64/",sep=";"))`.

Because of issues with `install_github()`, please install Wallace locally this way:
 1. Download the zip file from the repository page.
 2. Unzip and open the wallace.Rproj file in RStudio.
 3. In the right-hand pane, click Build, then Build & Reload.
 4. Type `wallace()` in the console and press Enter.

~~The following code will install the package `wallace` and run the user interface.~~
!BELOW NO LONGER WORKS.
```R
# if you do not have devtools installed, install it first
install.packages("devtools")
# install wallace from github
devtools::install_github("wallaceEcoMod/wallace")
# load wallace
library(wallace)
# run the user interface
wallace()
```

Please note the following post-install:

1. *Wallace* uses the `maxent()` function in the package `dismo`. This function requires the user to place the `maxent.jar` file in the `/java` directory of the `dismo` package root folder. You can download Maxent <a href="https://www.cs.princeton.edu/~schapire/maxent/" target="_blank">here</a>, and you can find `maxent.jar`, which is the program itself, in the downloaded folder. You can find the directory path to `dismo/java` by running `system.file('java', package="dismo")` at the command line. Simply copy `maxent.jar` and paste it into this folder. If you try to run Maxent in *Wallace* without the file in place, you will get a warning message in the log window and Maxent will not run.

2. If for some reason you are unable to view the tables in *Wallace*, please install (force if necessary) the development version of `htmlwidgets` by running this code: `devtools::install_github("ramnathv/htmlwidgets")`. You should be able to view tables now.
