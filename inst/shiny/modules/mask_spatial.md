### **Module: Mask by Shapefile**

**BACKGROUND**

Range models may include areas where the species does not exist. Expert knowledge can improve the realism of range models towards realized distributions (Soberón 2007) by removing unsuitable regions from a model - for example, a shape file of rivers and streams can be used to mask out model predictions on land for freshwater fish.  Or, a shapefile of cloud forest extent can be used to mask out any non-cloud forest areas from the model prediction for a mammal found only in cloud forest (Gerstner et al. 2018).  Additional information not included as a predictor in the SDM can be described by masks that differentiate potential habitat from non-habitat.

**IMPLEMENTATION**

This module relies on the R package `maskRangeR` to post-process or ‘mask’ an SDM using a user-specified shapefile (Merow et al. 2022). 

These analyses require the following data:

Shapefile: With environmental information relevant to the species’ distribution. *NOTE: You must upload all three files (.shp, .shx, .dbf)

SDM: This can be a continuous or thresholded sdm. You can use a model generated in `wallace` (Wallace SDM, Module: *Map Prediction* in **Component: Visualize Model Results**), a model transferred in time or space in `wallace` (Transferred SDM, **Component: Transfer**), or use a model uploaded by the User (User uploaded SDM, this component). You can also make an additional mask on a previously Masked SDM. Note that if you wish to make more than one mask, the SDM tab will default to Masked SDM to update the mask. This means the previous mask will be lost if it is not saved before proceeding. 

---

**CONTEXTO**

Los modelos de distribución pueden incluir áreas donde la especie no existe. El conocimiento experto puede mejorar el realismo de los modelos de rango hacia las distribuciones realizadas (Soberón 2007) al eliminar las regiones inadecuadas de un modelo; por ejemplo, se puede usar un shapefile de ríos y arroyos para enmascarar las predicciones del modelo en tierra para los peces de agua dulce. También, se puede usar un shapefile de la extensión del bosque nuboso para enmascarar cualquier área que no sea bosque nuboso de la predicción del modelo para un mamífero que se encuentra solo en el bosque nuboso (Gerstner et al. 2018). La información adicional no incluida como predictor en el SDM se puede describir mediante máscaras que diferencian el hábitat potencial del no hábitat.

**IMPLEMENTACIÓN**

Este módulo se basa en el paquete de R `maskRangeR` para postprocesar o 'enmascarar' un SDM utilizando un shapefile especificado por el usuario (Merow et al. 2022).

Estos análisis requieren los siguientes datos:

Shapefile: Con información ambiental relevante para la distribución de la especie. *NOTA: Debe cargar los tres archivos (.shp, .shx, .dbf)

SDM: Puede ser un SDM continuo o con umbral. Se puede usar un modelo generado en `wallace` (Wallace SDM, Module: *Map Prediction* en **Component: Visualize Model Results**), un modelo transferido en el tiempo o en el espacio en `wallace` (Transferred SDM, **Component: Transfer**), o usar un modelo cargado por el Usuario (User uploaded SDM, este componente). También puede hacer una máscara adicional en un SDM previamente enmascarado. Tenga en cuenta que si desea crear más de una máscara, la pestaña SDM se establecerá de forma predeterminada en SDM enmascarado para actualizar la máscara. Esto significa que la máscara anterior se perderá si no se guarda antes de continuar.

---

**REFERENCES**

Gerstner, B.E., Kass, J.M., Kays, R., Helgen, K.M., & Anderson, R.P. (2018). Revised distributional estimates for the recently discovered olinguito (*Bassaricyon neblina*), with comments on natural and taxonomic history. *Journal of Mammalogy*, 99(2), 321-332. <a href="https://doi.org/10.1093/jmammal/gyy012" target="_blank">DOI:10.1093/jmammal/gyy012</a>

Merow, C., Galante, P.J., Kass, J.M., Aiello-Lammens, M., Babich Morrow, C., Gerstner, B.E., Grisales-Betancur, V., Moore, A., Noguera-Urbano, E.A., Pinilla- Buitrago, G.E., Velasquez-Tibatá, J., Anderson, R.P., Blair, M.E. (2022). Operationalizing expert knowledge in species' range estimates using diverse data types. *Frontiers of Biogeography*, 14(2), e53589. <a href="https://doi.org/10.21425/F5FBG53589" target="_blank">DOI:10.21425/F5FBG53589</a>

Soberón, J. (2007). Grinnellian and Eltonian niches and geographic distributions of species. *Ecology Letters*, 10, 1115–1123. <a href="https://doi.org/10.1111/j.1461-0248.2007.01107.x" target="_blank">DOI:10.1111/j.1461-0248.2007.01107.x</a>

