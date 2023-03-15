### **Module: Calculate Richness**

**BACKGROUND**

Community-level metrics can be calculated by combining range estimates across multiple species. The estimation of species richness from SDM predictions is often done by summing binary or continuous model prediction rasters (Calabrese et al., 2014). However, because richness can be overestimated using this methodology, we suggest that these estimates be constrained with macroecological models or biotic filters (as can be done in the **Component: Mask Prediction**, and see Merow et al. 2022). 

**IMPLEMENTATION**

The species richness calculation is implemented in Wallace using the R packages `raster` and `changeRangeR` (Galante et al. 2023). Binary species distribution models of the same resolution projected to the area of interest are stacked and summed to obtain the number of species per pixel. The resulting raster layer is plotted in the Map tab with a continuous color scale where colder colors denote fewer species and warmer colors more species. The resulting layer’s resolution depends on the resolution used for building individual species distribution models. 

These analyses require the following data:

SDMs to be stacked: These must be binary or thresholded SDMs of the same resolution. You can use a model generated in `wallace` (Wallace SDM, Module: *Map Prediction* in **Component: Visualize Model Results**), a model transferred in time or space in `wallace` (Transferred SDM, **Component: Transfer**), a Masked SDM (**Component: Mask Prediction**), or a model uploaded by the User (User uploaded SDM; uploaded in the **Mask Prediction** component in the *Upload User Prediction* module, see Module: *Upload User Prediction* guidance text for information on uploaded user SDMs).

---

**CONTEXTO**

Las métricas a nivel de comunidad se pueden calcular combinando estimaciones de rango a través de múltiples especies. La estimación de la riqueza de especies a partir de las predicciones SDM a menudo se realiza mediante la suma de rásteres de predicción de modelos binarios o continuos (Calabrese et al., 2014). Sin embargo, debido a que la riqueza se puede sobreestimar con esta metodología, sugerimos que estas estimaciones se limiten con modelos macroecológicos o filtros bióticos (como se puede hacer en Component: **Mask Prediction**, y ver a Merow et al. 2022).

**IMPLEMENTACIÓN**

El cálculo de la riqueza de especies se implementa en Wallace utilizando los paquetes de R `raster` y `changeRangeR` (Galante et al. 2023). Los modelos binarios de distribución de especies de la misma resolución proyectados en el área de interés se apilan y se suman para obtener el número de especies por píxel. La capa ráster resultante se traza en la pestaña Map con una escala de color continua donde los colores más fríos indican menos especies y los colores más cálidos más especies. La resolución de la capa resultante depende de la resolución utilizada para construir modelos de distribución de especies individuales.

Estos análisis requieren los siguientes datos:

SDM para apilar: Estos deben ser SDM binarios o con umbral de la misma resolución. Se puede usar un modelo generado en `wallace` (Wallace SDM, Module: *Map Prediction* en **Component: Visualize Model Results**), un modelo transferido en el tiempo o en el espacio en `wallace` (Transferred SDM, **Component: Transfer**), un Masked SDM (**Component: Mask Prediction**) o un modelo cargado por el usuario (User uploaded SDM; cargado en el componente **Mask Prediction** en el módulo *Upload User Prediction*, consulte Módulo: *Upload User Prediction* para obtener información sobre los SDM cargados por el usuario).

---

**REFERENCES**

Calabrese, J.M., Certain, G., Kraan, C., & Dormann, C.F. (2014). Stacking species distribution models and adjusting bias by linking them to macroecological models: Stacking species distribution models. *Global Ecology and Biogeography*, 23(1), 99–112. <a href="https://doi.org/10.1111/geb.12102" target="_blank">DOI:10.1111/geb.12102</a>

Galante, P.J., Chang, S., Paz, A., Aiello-Lammens, M., Gerstner, B.E., Johnson, B.A., Kass, J.M., Merow, C., Noguera-Urbano, E.A., Pinilla-Buitrago, G.E., and Blair, M.E. (2023). changeRangeR: an R package for reproducible biodiversity change metrics from species distribution estimates. *Conservation Science & Practice*, 5(1): e12863. <a href="https://doi.org/10.1111/csp2.12863" target="_blank">DOI:10.1111/csp2.12863</a>

Merow, C., Galante, P.J., Kass, J.M., Aiello-Lammens, M., Babich Morrow, C., Gerstner, B.E., Grisales-Betancur, V., Moore, A., Noguera-Urbano, E.A., Pinilla- Buitrago, G.E., Velasquez-Tibatá, J., Anderson, R.P., Blair, M.E. (2022). Operationalizing expert knowledge in species' range estimates using diverse data types. *Frontiers of Biogeography*, 14(2), e53589. <a href="https://doi.org/10.21425/F5FBG53589" target="_blank">DOI:10.21425/F5FBG53589</a>
