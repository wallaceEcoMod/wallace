### **Module: Calculate Ratio Overlap**

**BACKGROUND**

It is helpful for conservation planning to calculate the proportion overlap of a species’ range with other features, for example different land cover classes, protected areas, habitat types, or ecoregions, anthropogenic disturbance, or different types of threats (Velásquez-Tibatá et al. 2019). 

**IMPLEMENTATION**

Wallace calculates proportion overlap using the `changeRangeR` function 'ratioOverlap'. Shapefiles uploaded can be separated by any fields’ categories in a shapefile’s attribute table. Note:  If a continuous raster is provided for overlap then proportion of overlap is calculated for each quartile in the environmental raster.

These analyses require the following data:

Shapefile/raster: With environmental information relevant to the species’ distribution. *NOTE: You must upload all three files (.shp, .shx, .dbf) when using a shapefile. (optional) either 1) a shapefile of features or 2) a continuous raster. Must be in the same projection as Wallace or a uploaded user SDM. If there is no projection set, Wallace assumes WGS84.  If a raster, then the number of cells within each quantile are calculated. 

SDM: This can be a continuous or thresholded SDM. You can use a model generated in `wallace` (Wallace SDM, Module: *Map Prediction* in **Component: Visualize Model Results**), a model transferred in time or space in `wallace` (Transferred SDM, **Component: Transfer**), a Masked SDM (**Component: Mask Prediction**), or a model uploaded by the User (User uploaded SDM; uploaded in the **Mask Prediction** component in the *Upload User Prediction* module, see Module: *Upload User Prediction* guidance text for information on uploaded user SDMs).

Overlap calculations appear in the Results tab.

---

**CONTEXTO**

Es útil para la planificación de la conservación calcular la proporción de superposición del rango de una especie con otras características, por ejemplo, diferentes clases de cobertura terrestre, áreas protegidas, tipos de hábitat o ecorregiones, perturbaciones antropogénicas o diferentes tipos de amenazas (Velásquez-Tibatá et al. 2019).

**IMPLEMENTACIÓN**

Wallace calcula la superposición de proporciones usando la función `changeRangeR` 'ratioOverlap'. Los archivos en formato shapefile cargados se pueden separar por categorías de campos en la tabla de atributos de un shapefile. Nota: si se proporciona un ráster continuo para la superposición, la proporción de superposición se calcula para cada cuartil en el ráster ambiental.

Estos análisis requieren los siguientes datos:

Shapefile/raster: Con información ambiental relevante para la distribución de la especie. *NOTA: debe cargar los tres archivos (.shp, .shx, .dbf) cuando utilice un shapefile. (opcional) 1) un shapefile de características o 2) un ráster continuo. Debe estar en la misma proyección que Wallace o el SDM de usuario cargado. Si no se establece una proyección, Wallace asume WGS84. Si es un ráster, se calcula el número de celdas dentro de cada cuantil.

SDM: Puede ser un SDM continuo o con umbral. Se puede usar un modelo generado en `wallace` (Wallace SDM, Module: *Map Prediction* en **Component: Visualize Model Results**), un modelo transferido en el tiempo o en el espacio en `wallace` (Transferred SDM, **Component: Transfer**), un Masked SDM (**Component: Mask Prediction**) o un modelo cargado por el usuario (User uploaded SDM; cargado en el componente **Mask Prediction** en el módulo *Upload User Prediction*, consulte Módulo: *Upload User Prediction* para obtener información sobre los SDM cargados por el usuario).

Los cálculos de superposición aparecen en la pestaña Results.

---

**REFERENCES**

Galante, P.J., Chang, S., Paz, A., Aiello-Lammens, M., Gerstner, B.E., Johnson, B.A., Kass, J.M., Merow, C., Noguera-Urbano, E.A., Pinilla-Buitrago, G.E., and Blair, M.E. (2023). changeRangeR: an R package for reproducible biodiversity change metrics from species distribution estimates. *Conservation Science & Practice*, 5(1): e12863. <a href="https://doi.org/10.1111/csp2.12863" target="_blank">DOI:10.1111/csp2.12863</a> 

Velásquez-Tibatá, J., Olaya-Rodríguez, M.H., López-Lozano, D., Gutiérrez, C., González, I. & Londoño-Murcia, M.C. (2019). BioModelos: A collaborative online system to map species distributions. *PloS ONE* 14(3), e0214522. <a href="https://doi.org/10.1371/journal.pone.0214522" target="_blank">DOI:10.1371/journal.pone.0214522</a> 
