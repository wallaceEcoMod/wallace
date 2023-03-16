### **Module: Calculate Endemism**

**BACKGROUND**

Endemic species and areas of high species endemism are important targets for conservation efforts, and represent unique combinations of geography (Rosauer et al., 2009). Species endemism can be calculated by stacking binary maps of species’ ranges and then calculating a value for each cell equal to the number of species found in that cell divided by the total number of cells in which they are found (Calabrese et al., 2014).

**IMPLEMENTATION**

The species endemism calculation is implemented in Wallace using the R packages `raster` and `changeRangeR` (Galante et al. 2023). Multiple binary species distribution models of the same resolution projected to the area of interest are stacked. With the stacked models the number of species found in every pixel is divided by the total number of pixels in which they are found. The resulting raster layer is plotted in the Map tab with a continuous color scale where colder colors denote less endemism and warmer colors more endemism. The resulting layer’s resolution depends on the resolution used for building individual species distribution models.
These analyses require the following data:

SDMs to be stacked: These must be binary or thresholded SDMs of the same resolution. You can use a model generated in `wallace` (Wallace SDM, Module: *Map Prediction* in **Component: Visualize Model Results**), a model transferred in time or space in `wallace` (Transferred SDM, **Component: Transfer**), a Masked SDM (**Component: Mask Prediction**), or a model uploaded by the User (User uploaded SDM; uploaded in the **Mask Prediction** component in the *Upload User Prediction* module, see Module: *Upload User Prediction* guidance text for information on uploaded user SDMs).

---

**CONTEXTO**

Las especies endémicas y las áreas de alto endemismo de especies son objetivos importantes para los esfuerzos de conservación y representan combinaciones únicas de geografía (Rosauer et al., 2009). El endemismo de las especies se puede calcular apilando mapas binarios de los rangos de las especies y luego calculando un valor para cada celda igual al número de especies encontradas en esa celda dividido por el número total de celdas en las que se encuentran (Calabrese et al., 2014). 

**IMPLEMENTACIÓN**

El cálculo del endemismo de especies se implementa en Wallace utilizando los paquetes de R `raster` y `changeRangeR` (Galante et al. 2023). Se apilan múltiples modelos binarios de distribución de especies de la misma resolución proyectados al área de interés. Con los modelos apilados, el número de especies que se encuentran en cada píxel se divide por el número total de píxeles en los que se encuentran. La capa ráster resultante se traza en la pestaña Mapa con una escala de color continua donde los colores más fríos indican menos endemismo y los colores más cálidos más endemismo. La resolución de la capa resultante depende de la resolución utilizada para construir modelos de distribución de especies individuales.

Estos análisis requieren los siguientes datos:

SDM para apilar: Estos deben ser SDM binarios o con umbral de la misma resolución. Se puede usar un modelo generado en `wallace` (Wallace SDM, Module: *Map Prediction* en **Component: Visualize Model Results**), un modelo transferido en el tiempo o en el espacio en `wallace` (Transferred SDM, **Component: Transfer**), un Masked SDM (**Component: Mask Prediction**) o un modelo cargado por el usuario (User uploaded SDM; cargado en el componente **Mask Prediction** en el módulo *Upload User Prediction*, consulte Módulo: *Upload User Prediction* para obtener información sobre los SDM cargados por el usuario).

---

**REFERENCES**

Calabrese, J.M., Certain, G., Kraan, C., & Dormann, C.F. (2014). Stacking species distribution models and adjusting bias by linking them to macroecological models: Stacking species distribution models. *Global Ecology and Biogeography*, 23(1), 99–112. <a href="https://doi.org/10.1111/geb.12102" target="_blank">DOI:10.1111/geb.12102</a>

Galante, P.J., Chang, S., Paz, A., Aiello-Lammens, M., Gerstner, B.E., Johnson, B.A., Kass, J.M., Merow, C., Noguera-Urbano, E.A., Pinilla-Buitrago, G.E., and Blair, M.E. (2023). changeRangeR: an R package for reproducible biodiversity change metrics from species distribution estimates. *Conservation Science & Practice*, 5(1), e12863. <a href="https://doi.org/10.1111/csp2.12863" target="_blank">DOI:10.1111/csp2.12863</a>

Rosauer, D., Laffan, S.W., Crisp, M.D., Donnellan, S.C., & Cook L.G. (2009). Phylogenetic endemism: a new approach for identifying geographical concentrations of evolutionary history. *Molecular Ecology*, 18, 4061–72. <a href="https://doi.org/10.1111/j.1365-294X.2009.04311.x" target="_blank">DOI:10.1111/j.1365-294X.2009.04311.x</a>
