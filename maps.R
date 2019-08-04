library(googleVis)
library(ggmap)
library(tidyverse)
library(leaflet)

#Killings by State  - Googgle Charts
states <- police %>% 
          group_by(state) %>% 
          summarise(deaths = n())

states$deaths <- as.numeric(states$deaths)



GeoStates <- gvisGeoChart(states, locationvar='state',
                          colorvar='deaths',
                          options=list(region="US", 
                                       displayMode="regions", 
                                       resolution="provinces",
                                       width=600, height=400))
                         
plot(GeoStates)

#Killings by percent population /state/ black







GeoStates <- gvisGeoChart(state_race_black, locationvar='State.x',
                          colorvar='relative_percent',
                          options=list(region="US", 
                                       displayMode="regions", 
                                       resolution="provinces",
                                       color = 'red',
                                       width=600, height=400))

plot(GeoStates)


# Leaflet maps
pal <- colorFactor(
  palette = "Set2",
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
