### **Module:** ***Draw Study Region With Polygon Tool***

**BACKGROUND**

This module allows users to use the polygon-drawing tool to delineate the study region. To read about the importance of selecting a study region, see **Component: Process Environmental Data** guidance text.

**IMPLEMENTATION** 

The R package `leaflet.extras` (Karambelkar et al. 2018) provides a drawing tool plugin to the `leaflet` map (Cheng et al. 2022) for this module, which allows users to draw a polygon to specify the study region. 
The polygon-drawing tool icon is located underneath the zoom in/out buttons on the map. If a mistake is made while drawing, the last point can be deleted, or the whole drawing can be cleared. To finish, click the first point to close the shape.
Also, this module relies on `sf` (Peebesma 2018) for buffering spatial objects. A buffer can be applied (in degrees) to the drawn polygon before moving on to Step 2) Sample Background Points.
Wallace then masks the environmental grids by the resulting polygon. Users can download the masked grids in three raster grid formats (.asc, .grd, and .tif).

**REFERENCES**

Pebesma, E., 2018. Simple Features for R: Standardized Support for Spatial Vector Data. The R Journal 10 (1), 439-446, <a href="https://doi.org/10.32614/RJ-2018-009"_blank">DOI:10.32614/RJ-2018-009</a>.  

Cheng, et al. (2022). leaflet: Create Interactive Web Maps with the JavaScript 'Leaflet' Library. R package Version 2.1. 
<a href="https://rstudio.github.io/leaflet/" target="_blank">GitHub</a>  

Karambelkar, et al. (2018). leaflet.extras: Extra Functionality for 'leaflet' Package. R package Version 1.0. 
<a href="https://bhaskarvk.github.io/leaflet.extras/" target="_blank">GitHub</a> 

