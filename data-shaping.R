library(dplyr) 
library(plotly)
library(sf)
library(mapview)
police <- read.csv("police_killings.csv")
police$age <- as.numeric(police$age)
police$county_fp<- sprintf("%03d", police$county_fp)
police$FIPS <- paste(police$state_fp,police$county_fp, sep='')

nc = st_read(system.file("shape/nc.shp", package="sf"))

list.files("shape/nc.shp", package="sf")

pfips <- merge(p,nc, by = "FIPS", all.p=TRUE)


pfips <- pfips %>% 
  select(FIPSNO.x,FIPSNO.y,CNTY_, CNTY_ID,)
  
View(pfips)
View(police)

state_age <- police %>% 
  select(age, state) %>% 
  group_by(state) %>% 
  summarize(mean_age = mean(age, na.rm = TRUE))

sapply(state_age,class)



m <- leaflet(police) %>% 
  addProviderTiles(providers$CartoDB.Positron)


nc <- data.frame(nc)
summary(nc)
ra

plot(st_geometry(nc))

mapview(nc)

library(leaflet)
library(leaflet.extras)



