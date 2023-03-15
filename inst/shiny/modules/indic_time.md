### **Module: Calculate Change Over Time**

**BACKGROUND**

For users that have information on past environmental conditions or future scenarios for a series of years, they can calculate changes in metrics over time and view a line graph and table of those changes.

**IMPLEMENTATION**

Wallace implements the envChange function from `changeRangeR`.

These analyses require the following data:

SDM: This can be a continuous or thresholded SDM. You can use a model generated in `wallace` (Wallace SDM, Module: *Map Prediction* in **Component: Visualize Model Results**), a model transferred in time or space in `wallace` (Transferred SDM, **Component: Transfer**), a Masked SDM (**Component: Mask Prediction**), or a model uploaded by the User (User uploaded SDM; uploaded in the **Mask Prediction** component in the *Upload User Prediction* module, see Module: *Upload User Prediction* guidance text for information on uploaded user SDMs).

Environmental data: In raster file format. The filename must include the year of the dataset.  

Threshold value: Enter a value for the environmental data that you want to compare across time within the species' range. For example, the lower bound value determined in the **Component: Mask Prediction** in Module: *Temporal Extract* can be entered here for the user to compare what proportion of the range falls above that value for the years where dated environmental rasters (e.g. forest cover) are provided.  

---

**CONTEXTO**

Para los usuarios que tienen información sobre condiciones ambientales pasadas o escenarios futuros para una serie de años, pueden calcular los cambios en las métricas a lo largo del tiempo y ver un gráfico de líneas y una tabla de esos cambios.

**IMPLEMENTACIÓN**

Wallace implementa la función envChange de `changeRangeR`.

Estos análisis requieren los siguientes datos:

SDM: Puede ser un SDM continuo o con umbral. Se puede usar un modelo generado en `wallace` (Wallace SDM, Module: *Map Prediction* en **Component: Visualize Model Results**), un modelo transferido en el tiempo o en el espacio en `wallace` (Transferred SDM, **Component: Transfer**), un Masked SDM (**Component: Mask Prediction**) o un modelo cargado por el usuario (User uploaded SDM; cargado en el componente **Mask Prediction** en el módulo *Upload User Prediction*, consulte Módulo: *Upload User Prediction* para obtener información sobre los SDM cargados por el usuario).

Datos ambientales: En formato de archivo ráster. El nombre del archivo debe incluir el año del conjunto de datos.

Valor de umbral: Ingrese un valor para los datos ambientales que desea comparar a lo largo del tiempo dentro del rango de la especie. Por ejemplo, el valor límite inferior determinado en **Component: Mask Prediction**  del módulo *Temporal Extract* se puede ingresar aquí para que el usuario compare qué proporción del rango cae por encima de ese valor para los años en los que se proporcionan rásteres ambientales fechados (por ejemplo, cobertura forestal).

---

**REFERENCES**

Galante, P.J., Chang, S., Paz, A., Aiello-Lammens, M., Gerstner, B.E., Johnson, B.A., Kass, J.M., Merow, C., Noguera-Urbano, E.A., Pinilla-Buitrago, G.E., and Blair, M.E. (2023). changeRangeR: an R package for reproducible biodiversity change metrics from species distribution estimates. *Conservation Science & Practice*, 5(1), e12863. <a href="https://doi.org/10.1111/csp2.12863" target="_blank">DOI:10.1111/csp2.12863</a> 



