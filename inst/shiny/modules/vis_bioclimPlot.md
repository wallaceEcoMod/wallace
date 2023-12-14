### **Module:** ***BIOCLIM Envelope Plots*** 

**BACKGROUND**  

As mentioned in **Component: Build and Evaluate Niche Model**, the algorithm BIOCLIM implements Hutchinson's *n*-dimensional hypervolume concept of the niche, also termed an environmental envelope (Hutchinson 1957; Booth et al. 2014). The model can be visualized in environmental space via what commonly are termed 'envelope plots' that indicate the density of occurrence records (and, hence, prediction strength) for any two predictor variables.

**IMPLEMENTATION** 

This module relies on plotting functionality from the R package `dismo`.

The envelope plot can be viewed in the "Results" tab. The axes of the plot represent the ranges of different predictor variables. Users can change these axes to view any combination of two environmental predictors. Furthermore, the threshold of the envelope can also be altered to trim percentiles of the tails of each variable (see Module: *BIOCLIM* for more details). Users can download the currently displayed BIOCLIM envelope plot displayed as an image file (.png).

**REFERENCES**

Booth, T.H., Nix, H.A., Busby J.R., & Hutchinson, M.F. (2014). BIOCLIM: The first species distribution modelling package, its early applications and relevance to most current MaxEnt studies. *Diversity and Distributions*, 20(1), 1-9. <a href="https://doi.org/10.1111/ddi.12144" target="_blank">DOI: 10.1111/ddi.12144</a>

Hutchinson, G.E. (1957). "Concluding remarks". *Cold Spring Harbor Symposia on Quantitative Biology*, 22, 415–427. <a href="http://dx.doi.org/10.1101/SQB.1957.022.01.039" target="_blank">DOI: 10.1101/SQB.1957.022.01.039</a> 
