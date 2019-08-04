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


national_race_data <- left_join(national_race_agg, us_race, by = c('Victim_race' = 'us_race'))

national_race_data <- national_race_data %>% 
  mutate(relative_percent = signif((((percent_race_killings/us_percent_pop)-1)*100),2))

# STATE RACE BAR CHART

p <- plot_ly(national_race_data, x = ~Victim_race, y = ~relative_percent, type = 'bar', 
             text = (~relative_percent), textposition = 'auto',
             marker = list(color = c('rgba(204,204,204,1)', '#fd0000',
                                           'rgba(204,204,204,1)', 'rgba(204,204,204,1)',
                                     '#fd0000'),
                           opacity = .75
                           
                           )
                        )  %>%
  layout(title = "Percent More or Less Likely to be Killed",
         barmode = "group",
         xaxis = list(title = ""),
         yaxis = list(title = "")) 


p

#AGGREGATE AND REJOIN ON POLICE DATAFRAME FOR STATE TOTAL
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




# JOIN STATE POPULATION DATA FOR RELATIVE POPULATION 

killings_by_state_summary <- left_join(killing_by_state_percent,state_race,by= 
                                         c("State.x" = "state",
                                           "Victim_race" = "state_race"))



killings_by_state_summary <- killings_by_state_summary %>% 
  mutate("Relative-Percent" = (percent_state_killings_by_race/percent_state_pop)-1) %>% 
  filter(Victim_race != 'Unknown') %>% 
  select(State_name, State.x, Victim_race, "Relative-Percent", state_total_killings)




state_race_input <- killings_by_state_summary


unique(state_race_input$Victim_race)

####---------------------------------------------------------------------------
#MAPS



##RELATIVE DEATHS TO STATES

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



