### **Module: Species Endemism**

**BACKGROUND**

text

**IMPLEMENTATION**

The species endemism calculation is implemented in Wallace using the R package raster and the R package changeRangeR. Binary species distribution models projected to the area of interest are stacked. With the stacked models the number of species found in every pixel is divided by the total number of pixels in which they are found. The resulting raster layer is plotted in the map tab with a continuous color scale where colder colors denote less endemism and warmer colors more endemism. The resulting layer’s resolution depends on the resolution used for building individual species distribution models. 

**REFERENCES**

ChangeRangeR  in prep
SE calculation

