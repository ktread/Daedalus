library(tidyverse)
library(plotly)

police<- readRDS("police_final.rds") 
race_stats<- readRDS("race_stats.rds") 




#AGE HISTOGRAM
age_hist <- police %>% 
    filter(age >0 ) %>% 
    plot_ly(x=police$age, 
             type = "histogram",
             #mode = "bar
             text= "24",
            name = "",
             hovertemplate = paste(
               "Age Group: %{x}<br>",
               "Number killed: %{y}<br>"
             )
             ) %>%
  layout(yaxis=list(type='linear'),legend = ~age) 

age_hist

#POVERTY HISTOGRAM
pov_hist <- police %>% 
  filter(pov >0 ) %>% 
  plot_ly(x=police$pov, 
          type = "histogram",
          #mode = "bar
          text= "24",
          name = "",
          hovertemplate = paste(
            "% of Population in Poverty: %{x}%<br>",
            "Number killed: %{y}<br>"
          )
  ) %>%
  layout(yaxis=list(type='linear'),legend = ~pov) 

pov_hist

#ADDDING TOTALS BECAUSE MATH
police_total <- police %>% 
  summarise(total = n())

police_total = total$total


#NATIONAL RATE BAR CHART
national_race_agg <- police %>% 
  select(raceethnicity) %>% 
  group_by(raceethnicity) %>% 
  summarise(killings = n(), percent_race_killings = n()/police_total) %>% 
  arrange(desc(killings))
  
bar_tot <- left_join(national_race_agg, race_stats, by = 'raceethnicity')

bar_tot <- bar_tot %>% 
  mutate(relative_percent = percent_race_killings/percent_pop)

race_bar  <- bar_tot %>%  plot_ly(
      x = ~raceethnicity,
      y= ~relative_percent,
      name = "Killings by RAce",
      type = "bar")

race_bar


# STATE RATE BAR CHART
state_race <- readRDS("state_race.rds")

state_agg <- police  %>% 
  select(state) %>% 
  group_by(state) %>% 
  summarise(state_total = n())

state_percent <- left_join(police,state_agg, by='state')

View(state_percent)

state_percent <- state_percent %>% 
  select(state, raceethnicity, state_total) %>% 
  group_by(state,raceethnicity) %>% 
  summarise(killings = n(), state_total= max(state_total), percent_state_k = n()/max(state_total))

  
state_bar_total <- left_join(state_percent,state_race,by= 
                               c("state" = "state",
                                 "raceethnicity" = "raceethnicity"))

state_bar_total <- state_bar_total %>% 
  mutate(relative_percent = percent_state_k/percent_pop)


state_race_black <- state_bar_total %>% 
  filter(raceethnicity=="Black")

