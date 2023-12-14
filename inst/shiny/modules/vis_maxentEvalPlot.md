### **Module:** ***Maxent Evaluation Plots*** 

**BACKGROUND**  

As mentioned in **Component: Build and Evaluate Niche Model** guidance text, like many niche/distribution modeling algorithms, the Maxent output depends greatly on settings that affect complexity (Merow et al. 2014). Thus, *Wallace* allows users to "tune" settings related to complexity to identify those that result in the best balance of complexity and predictive ability (Radosavljevic and Anderson 2014). Plotting evaluation metrics across values of settings constitutes a helpful visualization to aid in such tuning (Muscarella et al. 2014).

**IMPLEMENTATION** 

This module relies on plotting functionality from R package `ENMeval`.

Users can select which evaluation metric plot to view in the "Results" tab. Different feature classes are symbolized by color, regularization multipliers are labeled on the x-axis, and the value of the evaluation metric is labeled on the y-axis. Variance is displayed in the error bars. Users can download the currently displayed Maxent evaluation plot as an image file (.png).

**REFERENCES**

Merow, C., Smith, M.J., Edwards, T.C., Guisan, A., McMahon, S.M., Normand, S., Thuiller, W., Wüest, R.O., Zimmermann, N.E., & Elith, J. (2014). What do we gain from simplicity versus complexity in species distribution models? *Ecography*, 37(12), 1267-1281. <a href="https://doi.org/10.1111/ecog.00845" target="_blank">DOI: 10.1111/ecog.00845</a>

Muscarella, R., Galante, P.J., Soley-Guardia, M., Boria, R.A., Kass, J.M., Uriarte, M., & Anderson, R.P. (2014). ENMeval: An R package for conducting spatially independent evaluations and estimating optimal model complexity for Maxent ecological niche models. *Methods in Ecology and Evolution*, 5(11), 1198-1205. <a href="https://doi.org/10.1111/2041-210X.12261" target="_blank">DOI: 10.1111/2041-210X.12261</a>

Radosavljevic, A., & Anderson, R.P. (2014). Making better Maxent models of species distributions: complexity, overfitting and evaluation. *Journal of Biogeography*, 41(4), 629-643. <a href="https://doi.org/10.1111/jbi.12227" target="_blank">DOI: 10.1111/jbi.12227</a>

