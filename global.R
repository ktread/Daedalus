library(data.table)
library(tidyverse)
library(plotly)
library(googleVis)
library(ggmap)
library(leaflet)

source("viz_preprocessing.R")

choice <- ( c("Black","Asian/Pacific Islander"
          ,"White"                 
            ,"Hispanic/Latino"   
            ,"Unknown"               
            ,"Native American"
            ,"All Races" ))

killings_by_state <- police  %>% 
  group_by(State_name,State) %>% 
  summarise(killed_in_state = n()) %>% 
  select(State_name,State,killed_in_state) 


killings_by_state <- left_join(police,killings_by_state, by='State_name')


killing_by_state_percent <- killings_by_state %>% 
  select(State_name,State.x, Victim_race, killed_in_state) %>% 
  group_by(State_name,State.x,Victim_race) %>% 
  summarise(killings_by_race_total = n(), state_total_killings= max(killed_in_state), 
            percent_state_killings_by_race = n()/max(killed_in_state)) %>% 
  distinct()

killings_by_state_summary <- left_join(killing_by_state_percent,state_race,by= 
                                         c("State.x" = "state",
                                           "Victim_race" = "state_race"))

killings_by_state_summary <- killings_by_state_summary %>% 
  mutate("Relative-Percent" = (percent_state_killings_by_race/percent_state_pop)-1) %>% 
  filter(Victim_race != 'Unknown') %>% 
  select(State_name, State.x, Victim_race, "Relative-Percent", state_total_killings)

state_race_input <- killings_by_state_summary


states <- police %>% 
  group_by(State) %>% 
  summarise(deaths = n())

states$deaths <- as.numeric(states$deaths)

state_relative_population_deaths <- left_join(states,state_race, by=c("State" = "state"))

state_relative_population_deaths <-  state_relative_population_deaths %>% 
  group_by(State) %>% 
  summarise(state_percent = (max(deaths)/total_killed)*100, percent_state_us_pop = max(percent_state_us_pop)*100, state_deaths = max(deaths)) %>% 
  select(State,state_percent, percent_state_us_pop, state_deaths)

state_relative_population_deaths <- state_relative_population_deaths %>% 
  mutate(relative_percent = ((state_percent/percent_state_us_pop)-1))

