# Packages
library(tidyverse)
library(sf)
library(terra)
library(geodata)
library(leaflet)

#### PARÁMETROS COMUNES ####
# Directorio de salida
output.dir <- "./Predictors/"

# crea el directorio de salida si no exite
if (dir.exists(output.dir) == F) {
  dir.create(output.dir)
}


# # AOI
# Generar el AOI en base a las presencias
presences <- st_read("C:/SCIENCE/2025_INIA_Cinchona/Data/Data_C_Officinalis/Data_C_Officinalis.gdb") %>% 
  st_transform(crs = 4326)

bbox <- st_bbox(presences)
bbox_buffer <- vect(st_buffer(st_as_sfc(st_bbox(presences)), dist = 10000))


# Visualize data
leaflet() %>%
  addProviderTiles(
    providers$Esri.WorldImagery,
    options = providerTileOptions()
  ) %>%
  addTiles(options = tileOptions(opacity = 0.5)) %>%
  addCircleMarkers(data = presences, 
                   lng = st_coordinates(presences)[,1], 
                   lat = st_coordinates(presences)[,2], 
                   radius = 5, 
                   color = "red", 
                   fillOpacity = 0.5) %>% 
  addPolygons(data = bbox_buffer, 
              color = "blue", 
              fill = NA, 
              weight = 2)



# Preparo la plantilla raster en base a uno de los archivos de CHELSA
dir.root <- "C:/SCIENCE/FOP_CC_Hyperphagia/CHELSA/"
list <- list.files(dir.root, pattern = ".tif$", full.names = T)
list

template <- rast(list[1]) %>%
  crop(ext(bbox_buffer), mask = T)


#### VARIABLES DE SUELOS ####
# download data
ph <- soil_world(var = "phh2o", depth = 15, path = "./Soilgrids250/") %>%
  crop(ext(template), mask = T) %>%
  resample(template, method = "average")

names(ph) <- "phh2o"

# Scale factor
ph <- ph / 10


soc <- soil_world(var = "soc", depth = 15, path = "./Soildgrids250/") %>%
  crop(ext(template), mask = T) %>%
  resample(template, method = "average")

names(soc) <- "soc"

# Scale factor
soc <- soc / 10


bdod<- soil_world(var = "bdod", depth = 15, path = "./Soildgrids250/") %>%
  crop(ext(template), mask = T) %>%
  resample(template, method = "average")
names(bdod) <- "bdod"

# Scale factor
bdod <- bdod / 10



writeRaster(ph, paste0(output.dir, "phh2o.tif"), overwrite = T)
writeRaster(soc, paste0(output.dir, "soc.tif"), overwrite = T)
writeRaster(bdod, paste0(output.dir, "bdod.tif"), overwrite = T)


#### VARIABLES BIOCLIMÁTICAS CHELSA ####
dir.root <- "C:/SCIENCE/FOP_CC_Hyperphagia/CHELSA/"
list <- list.files(dir.root, pattern = ".tif$")
list

desired_variables <- c("bio1_", "bio12_", "bio15_")

# Bio1 – Temperatura media anual
# ↳ Refleja la preferencia por climas frescos y estables.
# 
# Bio12 – Precipitación anual
# ↳ Directamente relacionada con la supervivencia (la mortalidad disminuye con mayor precipitación).
# 
# Bio15 – Estacionalidad de la precipitación (coeficiente de variación)
# ↳ Relacionado con la estabilidad climática, importante para el desarrollo en bosques nublados.

# Filtra las variables deseadas
list <- list[grepl(paste(desired_variables, collapse = "|"), list)]


# Carga, transforma, recorta al AOI y remuestrea empleando como base el dem todas las variables
for (var in list) {
  r <- rast(paste0(dir.root, var)) %>%
    crop(ext(bbox_buffer), mask = T)

  name <- gsub("CHELSA_", "", names(r))
  name <- gsub("_1981-2010_V.2.1", "", name)

  names(r) <- name

  writeRaster(r, paste0(output.dir, var), overwrite = T)
}


#### VARIABLES BIOCLIMÁTICAS CHELSA - ESCENARIOS DE CAMBIO CLIMÁTICO ####
output.dir.climproj <- "./Predictors_climate_projections"

# crea el directorio de salida si no exite
if (dir.exists(output.dir.climproj) == F) {
  dir.create(output.dir.climproj)
}



dir.root <- "C:/SCIENCE/FOP_CC_Hyperphagia/CHELSA_Projections_raw_data/"
list <- list.files(dir.root, pattern = ".tif$")
list

models <- c()
scenarios <- c()
years <- c()

for (layer in list) {
  model <- str_split(layer, "_")[[1]][4]
  scenario <- str_split(layer, "_")[[1]][5]
  year <- str_split(layer, "_")[[1]][3]

  models <- unique(c(models, model))
  scenarios <- unique(c(scenarios, scenario))
  years <- unique(c(years, year))
}


for (scenario in unique(scenarios)) {
  if (dir.exists(paste0(output.dir.climproj, "/", scenario)) == F) {
    dir.create(paste0(output.dir.climproj, "/", scenario))
  }
  for (year in unique(years)) {
    if (dir.exists(paste0(output.dir.climproj, "/", scenario, "/", year)) == F) {
      dir.create(paste0(output.dir.climproj, "/", scenario, "/", year))
    }
  }
}


variables <- c("bio1", "bio12", "bio15")

for(var in variables) {
  for (year in unique(years)) {
    filtered_list_1 <- list[grepl(paste0(var, "_", year), list)]
    for (scenario in unique(scenarios)) {
      filtered_list_2 <- filtered_list_1[grepl(scenario, filtered_list_1)]
      r <- rast(paste0(dir.root, filtered_list_2)) %>%
        crop(ext(bbox_buffer), mask = T)
      
      r <- median(r)
      names(r) <- var
      
      writeRaster(r, file.path(output.dir.climproj, scenario, year, paste0(var, ".tif")), 
                  overwrite = T)
    }
  }
}

