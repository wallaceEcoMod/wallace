### **Module:** ***WorldClim Bioclimatic Variables***

**BACKGROUND**

The WorldClim project provides bioclimatic variables for the Earth's land surfaces for all continents except Antarctica. Monthly data from weather stations were spatially interpolated using elevation as a covariate; subsequently, 19 bioclimatic variables were derived, reflecting various aspects of temperature and precipitation (Hijmans et al. 2005). The descriptions of each variable are listed below, taken from the <a href="https://www.worldclim.org/" target="_blank">  WorldClim</a> website, where more details can be found.

BIO1 = Annual Mean Temperature  
BIO2 = Mean Diurnal Range (Mean of monthly (max temp - min temp))  
BIO3 = Isothermality (BIO2/BIO7) (* 100)  
BIO4 = Temperature Seasonality (standard deviation *100)  
BIO5 = Max Temperature of Warmest Month  
BIO6 = Min Temperature of Coldest Month  
BIO7 = Temperature Annual Range (BIO5-BIO6)  
BIO8 = Mean Temperature of Wettest Quarter  
BIO9 = Mean Temperature of Driest Quarter  
BIO10 = Mean Temperature of Warmest Quarter  
BIO11 = Mean Temperature of Coldest Quarter  
BIO12 = Annual Precipitation  
BIO13 = Precipitation of Wettest Month  
BIO14 = Precipitation of Driest Month  
BIO15 = Precipitation Seasonality (Coefficient of Variation)  
BIO16 = Precipitation of Wettest Quarter  
BIO17 = Precipitation of Driest Quarter  
BIO18 = Precipitation of Warmest Quarter  
BIO19 = Precipitation of Coldest Quarter  

**IMPLEMENTATION**

This module relies on the R package `raster` (Hijmans et al. 2021) to download bioclimatic variables from the WorldClim server.
*Wallace* makes all four resolutions of the data available (10 arcmin ≈ 20 km, 5 arcmin ≈ 10 km, 2.5 arcmin ≈ 5 km, and 30 arcsec ≈ 1 km). Although a later version of WorldClim exists (v2; Fick and Hijmans 2017), it is not available for automated download via the `raster` package; therefore, *Wallace* provides the data from WorldClim v1 (Hijmans et al. 2005).
The finest-grain WorldClim dataset (30 arcsec) can only be downloaded by separate 30 x 30 degree tiles in the current implementation of dismo (1.1-1), so *Wallace* uses the current center of the map display as the tile reference for this resolution. This means that analyses with 30 arcsec climatic data using the current version of *Wallace* are restricted to the extent of a single tile. To visualize the tile, check the “30 arcsec tile” box in the bottom left corner of the map. This will let the user see if the occurrences are all contained within the tile or whether they extend beyond it.
When running locally, the dataset is downloaded to the *Wallace* folder (which can take substantial time), but *Wallace* will use the downloaded data for later runs when the same resolution is selected.
The “Batch” option will load the environmental variables selected at the chosen resolution for all the species that have occurrence data uploaded.


**REFERENCES**

Fick, S.E., & Hijmans, R.J. (2017). WorldClim 2: new 1-km spatial resolution climate surfaces for global land areas. *International Journal of Climatology*, 37(12), 4302-4315. <a href="https://doi.org/10.1002/joc.5086" target="_blank"> DOI: 10.1002/joc.5086</a>  

Hijmans, R.J., Cameron, S.E., Parra, J.L., Jones, P.G., & Jarvis, A. (2005). Very high resolution interpolated climate surfaces for global land areas. *International Journal of Climatology*, 25(15), 1965-1978. <a href="https://doi.org/10.1002/joc.1276" target="_blank">DOI: 10.1002/joc.1276</a> 

Hijmans, R.J., et al. (2021). raster: Geographic Data Analysis and Modeling. R package version 3.4-13. <a href="https://CRAN.R-project.org/package=raster" target="_blank">CRAN</a>
