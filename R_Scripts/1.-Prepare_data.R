library(sf)
library(leaflet)

# Read data
presences <- st_read("C:/SCIENCE/2025_INIA_Cinchona/Data/Data_C_Officinalis/Data_C_Officinalis.gdb") %>% 
  st_transform(crs = 4326)


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
                   fillOpacity = 0.5) 
