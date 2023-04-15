### **Module:** ***Select Study Region***

**BACKGROUND**

For niche/distribution modeling approaches that compare the environments associated with species presence data with that of some comparison dataset (including Maxent; see **Component: Build and Evaluate Niche Model**), selection of a study region is critical. This decision defines the extent of grid cells for the comparison dataset (absences provided by the user, or pseudoabsence or background points sampled by the algorithm; Anderson 2013). Various, sometimes conflicting, principles for selecting a study region have been set forth in the literature (Peterson et al. 2011 chap. 7). Many researchers emphasize the geographic or environmental domain over which to build the model, whereas others suggest identification of areas where the species is likely to be at environmental equilibrium with the predictor variables (Vanderwal et al. 2009; Anderson and Raza 2010; Barve et al. 2011; Saupe et al. 2012; Franklin 2010 chap. 4; Anderson 2013; Merow et al. 2013). For example, one critical guiding principle (when estimates of suitability are desired, especially for use in other places and times) is that the study region should not include geographic areas that the species does not inhabit due to dispersal barriers. Inclusion of such areas will send a false negative signal, biasing the model's estimated response to the environment (Anderson 2015). Despide these theoretical suggestions, operational selection of the study region usually still depends on expert opinion and available natural history information (Acevedo et al. 2012; Gerstner et al. 2018).

**IMPLEMENTATION** 

This module relies on important functions from the R packages `sp` (for defining spatial objects) and `rgeos` (for buffering spatial objects).  

In this module, *Wallace* provides three simple ways to delimit a study region, by: 1) bounding box (rectangle with most the extreme coordinates in the four cardinal directions as vertices), 2) minimum convex polygon (a convex shape drawn around localities with minimized area), or 3) buffers around occurrence points. For each of these options, users can specify a buffer distance (in degrees; i.e. not meters). *Wallace* then masks the environmental grids by the resulting polygon. Users can download the masked grids as three raster grid formats (.asc, .grd, and .tif).

**REFERENCES**

Acevedo, P., Jiménez‐Valverde, A., Lobo, J.M., & Real, R. (2012). Delimiting the geographical background in species distribution modelling. *Journal of Biogeography*, 39(8), 1383-1390. <a href="https://doi.org/10.1111/j.1365-2699.2012.02713.x" target="_blank">DOI: 10.1111/j.1365-2699.2012.02713.x</a>

Anderson, R.P., & Raza A. (2010). The effect of the extent of the study region on GIS models of species geographic distributions and estimates of niche evolution: preliminary tests with montane rodents (genus *Nephelomys*) in Venezuela. *Journal of Biogeography*, 37(7), 1378-1393. <a href="https://doi.org/10.1111/j.1365-2699.2010.02290.x" target="_blank">DOI: 10.1111/j.1365-2699.2010.02290.x</a>

Anderson, R.P. (2013). A framework for using niche models to estimate impacts of climate change on species distributions. *Annals of the New York Academy of Sciences*, 1297(1), 8-28. <a href="https://doi.org/10.1111/nyas.12264" target="_blank">DOI: 10.1111/nyas.12264</a>

Anderson, R. P. (2015). El modelado de nichos y distribuciones: no es simplemente "clic, clic, clic." [With English and French translations: Modeling niches and distributions: it's not just "click, click, click" and La modélisation de niche et de distributions: ce n'est pas juste "clic, clic, clic"]. *Biogeografía*, 8, 4-27. <a href="https://2278aec0-37af-4634-a250-8bb191f1aab7.filesusr.com/ugd/e41566_e8acb6f9c20c44fa9cd729161582857d.pdf" target="_blank">pdf</a>

Barve, N., Barve, V., Jiménez-Valverde, A., Lira-Noriega, A., Maher, S.P., Peterson, A.T., Soberón, J., & Villalobos, F. (2011). The crucial role of the accessible area in ecological niche modeling and species distribution modeling. *Ecological Modelling*, 222(11), 1810–1819. <a href="https://doi.org/10.1016/j.ecolmodel.2011.02.011" target="_blank">DOI: 10.1016/j.ecolmodel.2011.02.011</a> 

Franklin, J. (2010). *Mapping Species Distributions: Spatial Inference and Prediction*. Data for species distribution models: the biological data. Cambridge: Cambridge University Press. <a href="https://doi.org/10.1017/CBO9780511810602" target="_blank">DOI: 10.1017/CBO9780511810602</a> 

Gerstner, B.E., Kass, J.M., Kays, R., Helgen, K.M., & Anderson, R.P. (2018). Revised distributional estimates for the recently discovered olinguito (*Bassaricyon neblina*), with comments on natural and taxonomic history. *Journal of Mammalogy*, 99(2), 321-332. <a href="https://doi.org/10.1093/jmammal/gyy012" target="_blank">DOI: 10.1093/jmammal/gyy012</a>

Merow, C., Smith, M.J., & Silander, J.A. (2013). A practical guide to MaxEnt for modeling species' distributions: What it does, and why inputs and settings matter. *Ecography*, 36(10), 1058-1069. <a href="https://doi.org/10.1111/j.1600-0587.2013.07872.x" target="_blank">DOI: 10.1111/j.1600-0587.2013.07872.x</a>

Peterson, A.T., Soberón, J., Pearson, R.G., Anderson, R.P., Martinez-Meyer, E., Nakamura M., & Araújo M.B. (2011). Modeling Ecological Niches. In: *Ecological Niches and Geographic Distributions*. Princeton, New Jersey: *Monographs in Population Biology*, 49. Princeton University Press. <a href="https://doi.org/10.23943/princeton/9780691136868.003.0005" target="_blank">DOI: 10.23943/princeton/9780691136868.003.0005</a>

Saupe, E.E., Barve, V., Myers, C.E., Soberón, J., Barve, N., Hensz, C.M., Peterson, A.T., Owens, H.L., & Lira-Noriega, A. (2012). Variation in niche and distribution model performance: the need for a priori assessment of key causal factors. *Ecological Modelling*, 237-238, 11-22. <a href="https://doi.org/10.1016/j.ecolmodel.2012.04.001" target="_blank">DOI: 10.1016/j.ecolmodel.2012.04.001</a>

VanDerWal, J., Shoo, L.P., Graham, C., & Williams, S.E. (2009). Selecting pseudo-absence data for presence-only distribution modeling: How far should you stray from what you know?. *Ecological Modelling*, 220(4), 589-594. <a href="https://doi.org/10.1016/j.ecolmodel.2008.11.010" target="_blank">DOI: 10.1016/j.ecolmodel.2008.11.010</a>
