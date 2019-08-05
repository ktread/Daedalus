library(tidyverse)
library(plotly)
library(googleVis)
library(ggmap)
library(leaflet)
library(leaflet.extras)




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



  