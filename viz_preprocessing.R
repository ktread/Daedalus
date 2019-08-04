library(tidyverse)
library(plotly)
library(googleVis)
library(ggmap)
library(leaflet)



how_armed <- police %>% 
  group_by(Armed) %>% 
  summarise(deaths = n()) %>% 
  arrange(desc(deaths)) %>% 
  top_n(10, deaths)

p <- plot_ly(how_armed) %>%
  add_bars(
    x = ~Armed,
    y = ~deaths,
    marker = list(
      color = "#fd0000",
      opacity = .85
    ),
    name = 'Top 10 Weapons Held by Victims',
    text = ~deaths,
    textposition = "auto"
  ) %>% 
  layout(title = 'Top 10 Weapons Held by Victims',
         xaxis = list(title = "Weapon"),
         yaxis = list(title = "Number of Deaths"),
         legend = list(x = 0, y = 1, bgcolor = 'rgba(255, 255, 255, 0)',
                       bordercolor = 'rgba(255, 255, 255, 0)'),
         barmode = 'group', 
         bargap = 0.01
  )
p
