library(dplyr) 
library(plotly)
library(sf)
library(mapview)
sapply(police, class)
police$age <- as.numeric(police$age)
police$FIPS <- paste(police$state_fp,police$)

state_age <- police %>% 
  select(age, state) %>% 
  group_by(state) %>% 
  summarize(mean_age = mean(age, na.rm = TRUE))

sapply(state_age,class)

print(nc)


m <- leaflet(police) %>% 
  addProviderTiles(providers$CartoDB.Positron)



nc = st_read(system.file("shape/nc.shp", package="sf"))

plot(st_geometry(nc))

mapview(nc)

merge(x = df1, y = df2, by = "CustomerId", all.x = TRUE)