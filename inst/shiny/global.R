library(glue)

UPLOAD_SIZE_MB <- 5000
options(shiny.maxRequestSize = UPLOAD_SIZE_MB*1024^2)

# load all modules
module_files <- list.files('modules', pattern = "\\.R$", full.names = TRUE)
for (file in module_files) source(file, local = TRUE)
