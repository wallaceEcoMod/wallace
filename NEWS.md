# wallace 0.6.2
- Removed `leaflet` version dependency, and added the Github master version to Remotes. For an unknown reason, using map functions fails with a proxy called from within a function when using version 1.0.1, the version now on CRAN. For the time being, `wallace` will use the dev version until RStudio releases a new `leaflet` version on CRAN.
