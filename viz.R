library(tidyverse)
library(plotly)
library(googleVis)
library(ggmap)
library(leaflet)
library(leaflet.extras)




#COMPLETE - LOCATION OF ALL DEATHS - CITY LEVEL

 
  city_rates <- leaflet(city) %>%
    addTiles() %>%  # Add default OpenStreetMap map tiles
    setView(-96, 37.8, 4) %>%
    addCircleMarkers(lat= ~latt,lng= ~long,
                     color = "#fd0000",
                     stroke = FALSE,
                     fillOpacity = 0.5,
                     radius = ~deaths/2,
                     popup = ~label,
                     popupOptions = labelOptions(noHide = T,
                                                 textsize = "15px"))  %>%
    addProviderTiles(providers$CartoDB.Positron)
  
  city_rates  # Print the map
  

#NOT COMPLETE DATE CHARTS

males <- plot_ly(date_gender_summary) %>%
   add_bars(
     x = ~Year,
     y = ~Male,
     marker = list(
       color = "#fd0000",
       opacity = .5
     ),
     name = 'Men Killed',
     text = ~Male,
     textposition = "auto"
   ) %>% 
  layout(title = 'Men Killed in the US (2015-2019)',
         xaxis = list(title = "Year"),
         yaxis = list(title = "Number of Deaths"),
         legend = list(x = 0, y = 1, bgcolor = 'rgba(255, 255, 255, 0)',
         bordercolor = 'rgba(255, 255, 255, 0)'),
         barmode = 'group', 
         bargap = 0.09
        )
         

female<-  plot_ly(date_gender_summary) %>%
  add_bars(
    x = ~Year,
    y = ~Female,
    marker = list(
      color = "#fd0000",
      opacity = .5
    ),
    text = ~Female,
    textposition = "auto"
  ) %>% 
  layout(title = 'Women Killed in the US (2015-2019)',
         xaxis = list(title = "Year"),
         yaxis = list(title = "Number of Deaths"),
         legend = list(x = 0, y = 1, bgcolor = 'rgba(255, 255, 255, 0)',
                       bordercolor = 'rgba(255, 255, 255, 0)'),
         barmode = 'group', 
         bargap = 0.09
  ) 
  
  


  