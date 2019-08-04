library(tidyverse)
library(plotly)
library(googleVis)
library(ggmap)
library(leaflet)
library(leaflet.extras)

#COMPLETED - AGE HISTOGRAM
age_hist <- police %>% 
  filter(Age >0 ) %>% 
  plot_ly(x=police$Age, 
          type = "histogram",
          #mode = "bar
          text= "24",
          name = "",
          hovertemplate = paste(
            "Age Group: %{x}<br>",
            "Number killed: %{y}<br>"
          )
  ) %>%
  layout(yaxis=list(type='linear'),legend = ~Age) 

age_hist


#COMPLETED - RELATIVE RACE DANGER BY STATE - OPTIONAL 
relative_by_race <- gvisGeoChart(state_race_black, locationvar='State.x',
                                   colorvar= "Relative-Percent",
                                   hovervar = "State_name",
                                   options=list(region="US", 
                                                displayMode="regions", 
                                                resolution="provinces",
                                                color = 'red',
                                                width=600, 
                                                height=400,
                                                colorAxis = "{minValue: -1,maxValue: 5,  colors: ['#00FF00','#FF0000']}"))
 plot(relative_by_race)
  

 

#COMPLETED - RELATIVE BY STATE - ALL STATES TO EACH OTHER  
relative_per_by_state <- gvisGeoChart(state_relative_population_deaths, locationvar='State', 
                            colorvar='relative_percent',
                            options=list(region="US", 
                                         displayMode="regions", 
                                         resolution="provinces",
                                         colorAxis = "{colors: ['#00FF00', '#FF0000']}"))
  
 plot(relative_per_by_state)
  

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
  
  


  