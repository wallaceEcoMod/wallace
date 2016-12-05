[![Open Source Love](https://badges.frapsoft.com/os/v2/open-source.svg?v=103)](https://github.com/ellerbrock/open-source-badge/)   [![GPL Licence](https://badges.frapsoft.com/os/gpl/gpl.svg?v=103)](https://opensource.org/licenses/GPL-3.0/)  

# Wallace

*Wallace* is a modular platform for reproducible modeling of species niches and distributions, written in R. The application guides users through a complete analysis, from the acquisition of data to visualizing model predictions on an interactive map, thus bundling complex workflows into a single, streamlined interface.

*Wallace* can be installed via Github by executing the following R code.

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
