library(sf)
library(leaflet)

# Read data
presences <- st_read("C:/SCIENCE/2025_INIA_Cinchona/Data/Coordenadas_Cinchona _officinalis_Peru.gdb") %>% 
  st_transform(crs = 4326)


# Visualize data
leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = presences, 
                   lng = st_coordinates(presences)[,1], 
                   lat = st_coordinates(presences)[,2], 
                   radius = 5, 
                   color = "red", 
                   fillOpacity = 0.5) 
