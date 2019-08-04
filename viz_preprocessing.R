library(tidyverse)
library(plotly)
library(googleVis)
library(ggmap)
library(leaflet)

police <- readRDS("police_final.rds")
state_race <- readRDS("state_race.rds")
us_race <- readRDS("us_race.rds")
city_race <- readRDS("city_stats.rds")
 

#ADDDING TOTALS BECAUSE MATH
total <- police %>% 
  summarise(total = n())

total_killed = total$total



#NATIONAL RACE BAR CHART
national_race_agg <- police %>% 
  select(Victim_race) %>% 
  group_by(Victim_race) %>% 
  summarise(killings = n(), percent_race_killings = n()/total_killed) %>% 
  arrange(desc(killings))


national_race_agg <- left_join(national_race_agg, us_race, by = c('Victim_race' = 'us_race'))

national_race_agg <- national_race_data %>% 
  mutate(relative_percent = signif((((percent_race_killings/us_percent_pop)-1)*100),2))











# Leaflet maps
pal <- colorFactor(
  palette = "Set2",
  domain = police$cause)



## MARKER TEST WITH GOOGLE MAPS

city <- police %>% 
  group_by(City, State, Manner_of_death, latt, long) %>% 
  summarise(deaths = n()) %>% 
  distinct()




####---------------------------------------------------------------------------


# DATE CHARTS

date_gender_summary<- police %>% 
  select(Year,Gender) %>% 
  group_by(Year,Gender) %>% 
  summarise(number_killed = n())


date_gender_summary <- dcast(date_gender_summary, Year ~ Gender)



