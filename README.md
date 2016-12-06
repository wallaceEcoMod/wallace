[![Open Source Love](https://badges.frapsoft.com/os/v2/open-source.svg?v=103)](https://github.com/ellerbrock/open-source-badge/)   [![GPL Licence](https://badges.frapsoft.com/os/gpl/gpl.svg?v=103)](https://opensource.org/licenses/GPL-3.0/)  

# Wallace (v0.6)

*Wallace* is a modular platform for reproducible modeling of species niches and distributions, written in R. The application guides users through a complete analysis, from the acquisition of data to visualizing model predictions on an interactive map, thus bundling complex workflows into a single, streamlined interface.

*Wallace* can be installed via Github by executing the following R code. If you are using a Windows machine, please download and install [RTools](https://cran.r-project.org/bin/windows/Rtools/) before installing the `devtools` package.

    # if you do not have devtools installed, install it first
    install.packages(devtools)
    # load devtools
    library(devtools)
    # install wallace from github
    install_github("wallaceEcoMod/wallace@pkg")
    # load wallace
    library(wallace)
    # run the user interface
    wallace()

When using `devtools` on Windows machines, there is a known [bug](https://github.com/hadley/devtools/issues/1298) that sometimes results in the inability to download all package dependencies. If this happens to you, please manually install any packages that cause package build errors, then reinstall `wallace`.
