### **Module:** ***Map Prediction*** 

**BACKGROUND**  

Although a niche/distributional model constitutes a mathematical function of the environmental variables, most studies involve using it to make predictions in geographic space (Peterson et al. 2011). Most niche/distribution modeling algorithms provide a continuous output, which can be converted to a binary prediction by applying a threshold. Above this value, the prediction is considered strong enough to indicate sufficient suitability for the species to be present.

**IMPLEMENTATION** 

This module relies on functionality for model prediction grids from the R package `dismo` (Hijmans et al. 2020).

Users must first choose a model. Depending on the `ENMeval` (Kass et al. 2021) settings selected in **Component: Build and Evaluate Niche Model**, there may be multiple choices for Maxent. *Wallace* calculates model output based on the predictor variable values for each cell and plots the prediction on the map. Three Maxent model output scales are available for viewing in Wallace: “raw”, “logistic”, and “cloglog”. The “raw” predicted values of the background pixels sum to 1 and can represent relative abundance (not probability of presence; Phillips et al. 2017; see also Merow et al. 2013). The “logistic” and “cloglog” transformations of the raw values range from 0 to 1 and approximate probability of occurrence, with different assumptions. For logistic, the assumption is that prevalence of the species equals 0.5 (Yackulic et al. 2012; Merow et al. 2013). For cloglog, assumptions are made regarding spatial dependence and cell size (Phillips et al. 2017). Logistic and cloglog transformed values are generally similar, but cloglog tends to be higher for mid- to high-range values (Phillips et al. 2017).

Users may then choose a thresholding rule (minimum training presence or 10 percentile) or quantile of training presences to convert the continuous prediction to a binary one (0s and 1s), which can be interpreted as presence/absence or suitable/unsuitable. Users can also plot the continuous prediction by choosing the no threshold option.  Please see guidance text for **Component: Build and Evaluate Niche Model** for more details on thresholding rules. After the model and threshold selections are made, the prediction can be plotted on the map. Users can download the prediction as either raster grid types for analysis (.asc, .grd and .tif), or as an image file (.png).

**REFERENCES**

Hijmans, R. J., Phillips, S., Leathwick, J., & Elith, J. (2020). dismo: Species Distribution Modeling. R package version 1.3-3. <a href="https://CRAN.R-project.org/package=dismo" target="_blank">CRAN</a> 

Kass, J., Muscarella, R., Galante, P.J., Bohl, C.L., Pinilla-Buitrago, G.E., Boria, R.A., Soley-Guardia, M., & Anderson, R.P. (2021). ENMeval: Automated Tuning and Evaluations of Ecological Niche Models. R package version 2.0 <a href="https://CRAN.R-project.org/package=ENMeval" target="_blank">CRAN</a> 

Merow, C., Smith, M.J., & Silander, J.A. (2013). A practical guide to MaxEnt for modeling species' distributions: What it does, and why inputs and settings matter. *Ecography*, 36(10), 1058-1069. <a href="https://doi.org/10.1111/j.1600-0587.2013.07872.x" target="_blank">DOI: 10.1111/j.1600-0587.2013.07872.x</a>

Peterson, A.T., Soberón, J., Pearson, R.G., Anderson, R.P., Martinez-Meyer, E., Nakamura, M., & Araújo, M.B. (2011). *Ecological Niches and Geographic Distributions*. Princeton, New Jersey. Princeton University Press. <a href="https://press.princeton.edu/books/paperback/9780691136882/ecological-niches-and-geographic-distributions-mpb-49" target="_blank">Ecological Niches and Geographic Distributions</a> 

Phillips, S.J., Anderson, R.P., Dudík, M., Schapire, R.E., & Blair, M.E. (2017). Opening the black box: an open-source release of Maxent. *Ecography*, 40(7), 887-893. <a href="https://doi.org/10.1111/ecog.03049" target="_blank">DOI: 10.1111/ecog.03049</a>

Yackulic, C.B., Chandler, R., Zipkin, E.F., Royle, J.A., Nichols, J.D., Campbell Grant, E.H., & Veran, S. (2013). Presence-only modelling using MAXENT: when can we trust the inferences?. *Methods in Ecology and Evolution*, 4(3), 236-243. <a href="https://doi.org/10.1111/2041-210x.12004" target="_blank">DOI: 10.1111/2041-210x.12004</a>
