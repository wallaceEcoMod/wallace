### **Module: Mask by Drawn Polygon**

**BACKGROUND**

Range models may omit areas of presence, or include areas where the species does not exist. Expert knowledge can improve the realism of range models, either by adding suitable regions to a model or removing unsuitable ones, representing additional information that was not included as predictors in the SDM training (Velásquez-Tibatá et al. 2019). Wallace allows users to specify expert-defined areas to add or remove to the model in a transparent and reproducible way.

**IMPLEMENTATION**

This module relies on the R package `maskRangeR` to post-process an SDM by drawing a polygon or uploading a user-specified shapefile or CSV file representing a polygon  (Merow et al. 2022). 

These analyses require the following data:

Expert defined area: This may be an area to add to or remove from the species’ distribution. In Wallace, you can draw a polygon directly while in map view, or upload a polygon area shapefile to add or remove the area from the SDM.

SDM: This can be a continuous or thresholded SDM. You can use a model generated in `wallace` (Wallace SDM, Module: *Map Prediction* in **Component: Visualize Model Results**), a model transferred in time or space in `wallace` (Transferred SDM, **Component: Transfer**), or use a model uploaded by the User (User uploaded SDM, this component). You can also make an additional mask on a previously Masked SDM. Note that if you wish to make more than one mask, the SDM tab will default to Masked SDM to update the mask. This means the previous mask will be lost if it is not saved before proceeding. 

Note: You can add or remove a polygon to a thresholded model prediction. However,  you can only remove a polygon from a continuous model prediction.

---

**CONTEXTO**

Los modelos de distribución pueden omitir áreas de presencia o incluir áreas donde la especie no existe. El conocimiento experto puede mejorar el realismo de los modelos de rango, ya sea agregando regiones adecuadas a un modelo o eliminando las inadecuadas, lo que representa información adicional que no se incluyó como predictores en el entrenamiento SDM (Velásquez-Tibatá et al. 2019). Wallace permite a los usuarios especificar áreas definidas por expertos para agregar o eliminar del modelo de manera transparente y reproducible.

**IMPLEMENTACIÓN**

Este módulo se basa en el paquete R `maskRangeR` para postprocesar un SDM dibujando un polígono o cargando un shapefile especificado por el usuario o un archivo CSV que representa un polígono (Merow et al. 2022).

Estos análisis requieren los siguientes datos:

Área definida por expertos: Esta puede ser un área para agregar o eliminar de la distribución de la especie. En Wallace, puede dibujar un polígono directamente mientras está en la vista de mapa, o cargar un shapefile de área de polígono para agregar o eliminar el área del SDM.

SDM: Puede ser un SDM continuo o con umbral. Se puede usar un modelo generado en `wallace` (Wallace SDM, Module: *Map Prediction* en **Component: Visualize Model Results**), un modelo transferido en el tiempo o en el espacio en `wallace` (Transferred SDM, **Component: Transfer**), o usar un modelo cargado por el Usuario (User uploaded SDM, este componente). También puede hacer una máscara adicional en un SDM previamente enmascarado. Tenga en cuenta que si desea crear más de una máscara, la pestaña SDM se establecerá de forma predeterminada en SDM enmascarado para actualizar la máscara. Esto significa que la máscara anterior se perderá si no se guarda antes de continuar.

Nota: Puede agregar o quitar un polígono a una predicción de modelo con umbral. Sin embargo, solo puede eliminar un polígono de una predicción de modelo continua.

---

**REFERENCES**

Merow, C., Galante, P.J., Kass, J.M., Aiello-Lammens, M., Babich Morrow, C., Gerstner, B.E., Grisales-Betancur, V., Moore, A., Noguera-Urbano, E.A., Pinilla- Buitrago, G.E., Velasquez-Tibatá, J., Anderson, R.P., Blair, M.E. (2022). Operationalizing expert knowledge in species' range estimates using diverse data types. *Frontiers of Biogeography*, 14(2), e53589. <a href="https://doi.org/10.21425/F5FBG53589" target="_blank">DOI:10.21425/F5FBG53589</a>

Velásquez-Tibatá, J., Olaya-Rodríguez, M.H., López-Lozano, D., Gutiérrez, C., González, I. & Londoño-Murcia, M.C. (2019). BioModelos: A collaborative online system to map species distributions. *PloS ONE* 14(3), e0214522. <a href="https://doi.org/10.1371/journal.pone.0214522" target="_blank">DOI:10.1371/journal.pone.0214522</a>
