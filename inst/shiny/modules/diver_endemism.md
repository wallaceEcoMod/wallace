### **Module: Calculate Endemism**

**BACKGROUND**

text

**IMPLEMENTATION**

The species endemism calculation is implemented in Wallace using the R package raster and the R package changeRangeR. Binary species distribution models projected to the area of interest are stacked. With the stacked models the number of species found in every pixel is divided by the total number of pixels in which they are found. The resulting raster layer is plotted in the map tab with a continuous color scale where colder colors denote less endemism and warmer colors more endemism. The resulting layer’s resolution depends on the resolution used for building individual species distribution models. 

**REFERENCES**

Merow, C., Galante, P., Gerstner, B., Johnson, B., Kass, J.M., Paz, A., Rosauer, D., Serra, P., Anderson, R.P., Blair, M. "changeRangeR: Translating species’ distributions into conservation metrics". In prep.

Rosauer D., Laffan S.W., Crisp M.D., Donnellan S.C., & Cook L.G. (2009) Phylogenetic endemism: a new approach for identifying geographical concentrations of evolutionary history. Molecular ecology, 18, 4061–72. 

