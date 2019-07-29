library(maps)
library(ggplot2)
library(dplyr)
library(leaflet)
library(leaflet.extras)
police <- read.csv("police_killings.csv")
View(police)
data(county.fips)
View(county.fips)

pal <- colorFactor(
  palette = "Set2", #c('red', 'blue', 'green', 'purple', 'orange'),
  domain = police$cause)

label <- "label"


#CIRCLES
m <- leaflet(police) %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addCircleMarkers(lat=police$latitude,lng=police$longitude,
                   color = ~pal(cause),
                   stroke = FALSE,
                   fillOpacity = 0.5,
                   popup = ~label,
                   popupOptions = labelOptions(noHide = T,
                                               textsize = "15px")
  )  %>%
  addLegend("bottomright", pal = pal, values = ~cause,
            title = "Cause of Death",
            labFormat = labelFormat(),
            opacity = 0.75
  ) %>%
  addProviderTiles(providers$CartoDB.Positron)
m  # Print the map


#HEATMAP
m <- leaflet(police) %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addWebGLHeatmap(size=30,units='px')   %>% 
  addProviderTiles(providers$CartoDB.Positron)
m  # Print the map
