library(tidyverse)
library(plotly)

police <- readRDS("police_final.rds")
state_race <- readRDS("state_race.rds")
us_race <- readRDS("us_race.rds")
city_race <- readRDS("city_stats.rds")

#AGE HISTOGRAM
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


#ADDDING TOTALS BECAUSE MATH
total <- police %>% 
  summarise(total = n())

total_killed = total$total
 


#NATIONAL RATE BAR CHART
national_race_agg <- police %>% 
  select(Victim_race) %>% 
  group_by(Victim_race) %>% 
  summarise(killings = n(), percent_race_killings = n()/total_killed) %>% 
  arrange(desc(killings))
  

national_race_data <- left_join(national_race_agg, us_race, by = c('Victim_race' = 'us_race'))

national_race_data <- national_race_data %>% 
  mutate(relative_percent = percent_race_killings/us_percent_pop)

national_race_data  <- national_race_data %>%  plot_ly(
      x = ~Victim_race,
      y= ~relative_percent,
      name = "Killings by RAce",
      type = "bar")

national_race_data


# STATE RACE BAR CHART


#AGGREGATE AND REJOIN ON POLICE DATAFRAME FOR STATE TOTAL
killings_by_state <- police  %>% 
  select(State_name,State) %>% 
  group_by(State_name,State) %>% 
  summarise(killed_in_state = n())


killings_by_state <- left_join(police,killings_by_state, by='State_name')


killing_by_state_percent <- killings_by_state %>% 
  select(State_name,State.x, Victim_race, killed_in_state) %>% 
  group_by(State_name,State.x,Victim_race) %>% 
  summarise(killings_by_race_total = n(), state_total_killings= max(killed_in_state), 
            percent_state_killings_by_race = n()/max(killed_in_state))

rm(killings_by_state)


# JOIN STATE POPULATION DATA FOR RELATIVE POPULATION 

killings_by_state_summary <- left_join(killing_by_state_percent,state_race,by= 
                               c("State.x" = "state",
                                 "Victim_race" = "state_race"))

rm(killing_by_state_percent)

killings_by_state_summary <- killings_by_state_summary %>% 
  mutate(relative_percent = (percent_state_killings_by_race/percent_state_pop)) %>% 
  filter(Victim_race != 'Unknown')


#GET DATA FOR EACH RACE 
state_race_black <- killings_by_state_summary %>% 
  filter(Victim_race=="Black")
  mutate(relative_percent = relative_percent*100) %>% 
  select(state, relative_percent)

  
state_race_hispanic <- killings_by_state_summary %>% 
  filter(Victim_race=="Hispanic/Latino")
  mutate(relative_percent = relative_percent*100) %>% 
  select(state, relative_percent)
  
  
  state_race_native_american <- killings_by_state_summary %>% 
    filter(Victim_race=="Native American")
  mutate(relative_percent = relative_percent*100) %>% 
    select(state, relative_percent)
  
  
  state_race_asianpi <- killings_by_state_summary %>% 
    filter(Victim_race=="Asian/Pacific Islander")
  mutate(relative_percent = relative_percent*100) %>% 
    select(state, relative_percent)
  
  
  state_race_white <- killings_by_state_summary %>% 
    filter(Victim_race=="White")
  mutate(relative_percent = relative_percent*100) %>% 
    select(state, relative_percent)
  

  
  # DATE CHARTS

 date_gender_summary<- police %>% 
    select(Year,Gender) %>% 
    group_by(Year,Gender) %>% 
    summarise(number_killed = n())
  

 date_gender_summary <- dcast(date_gender_summary, Year ~ Gender)
  
  p <- plot_ly(date_gender_summary, x = ~Year, y = ~Male, name = 'Men Killed', type = 'bar',text=  ~Male, textposition = "auto") %>%
    add_trace(y = ~Female, name = 'Women Killed', type = 'bar',text=  ~Female, textposition = "auto") %>% 
    layout(title = "Victims by Gender",
           xaxis = list(title = "Years"),
           yaxis = list (title = "Number Killed"),
           barmode = 'group')
  
  p
  
