library(googleVis)
library(dplyr)
police <- read.csv("police_killings.csv")
police$age <- as.numeric(police$age)
police$county_fp<- sprintf("%03d", police$county_fp)
police$FIPS <- paste(police$state_fp,police$county_fp, sep='')
police$state <- paste("US-",police$state,sep='')
police$latlong <- paste(police$latitude,":",police$longitude,sep="")
library(ggmap)
register_google(key = "AIzaSyAlA5GxoE-7-xz_btnBi8y-9bU4Z50eZno", write = TRUE)


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


