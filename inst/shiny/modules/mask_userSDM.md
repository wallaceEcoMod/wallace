### **Module: Upload User Prediction**

**BACKGROUND**

Here, you have the option to upload a prediction such as an SDM that you already made to use in other modules in this component, or for downstream components.

**IMPLEMENTATION**

This module uses the `raster` package to enable the user to upload a file in .tif format. Make sure any model you upload in *Upload User Prediction* is projected and has the same name as the scientific name of your species occurrence csv file as in “Genus_species.tif”. Note that some other modules in this component (e.g., Module: *Mask by Drawn Polygon*) and in the Indicators component (e.g., Module: *Calculate Area Metrics*) require thresholded (binary) instead of continuous predictions.

If users intend to use the *Temporal Extract* module in Component: **Mask**, or *Calculate Change Over Time* module in Component: **Indicators**, occurrence data for the species needs to be upload (using either query or user-specified) **BEFORE** uploading the SDM, and the genus and species names need to match.

---

**CONTEXTO**

Aquí, se tiene la opción de cargar una predicción, como un SDM que ya se realizó para usar en otros módulos de este componente, o para componentes posteriores.

**IMPLEMENTACIÓN**

Este módulo utiliza el paquete `raster` para permitir al usuario cargar un archivo en formato .tif. Asegúrese de que cualquier modelo que cargue en Cargar predicción de usuario se proyecte y tenga el mismo nombre que el nombre científico del archivo csv de ocurrencia de su especie como en "Genus_species.tif". Tenga en cuenta que algunos otros módulos en este componente (Módulo: *Mask by Drawn Polygon*) y en el componente **Indicadores** (Módulo: *Calculate Area Metrics*) requieren predicciones con umbral (binarias) en lugar de predicciones continuas.

Si los usuarios tienen la intención de utilizar el módulo de *Extracción temporal* en Component: **Mask**, o el módulo *Calcular cambio a lo largo del tiempo* en Component: **Indicators**, los datos de presencia de la especie deben cargarse (usando consultas o especificados por el usuario) **ANTES** de cargar el SDM, y los nombres de género y especie deben coincidir.

