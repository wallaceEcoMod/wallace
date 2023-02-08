### **Module: Calculate Richness**

**BACKGROUND**

text

**IMPLEMENTATION**

The species richness calculation is implemented in Wallace using the R package raster. Binary species distribution models projected to the area of interest are stacked and summed to obtain the number of species per pixel. The resulting raster layer is plotted in the map tab with a continuous color scale where colder colors denote less species and warmer colors more species. The resulting layer’s resolution depends on the resolution used for building individual species distribution models. 

**REFERENCES**

references
