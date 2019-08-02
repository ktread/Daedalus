library(tidyverse)
library(reshape2)

# POLICE AND STATE DATA FRAME
police <- read.csv("police.csv",stringsAsFactors = FALSE)
state_detail <- read.csv("state_detail.csv",stringsAsFactors = FALSE)

View(police)

police <- left_join(police,state_detail, by = c("state" = "State.Code"))



#CONVERTING TYPES
police$age <- as.numeric(police$age)

##PREPROCESSING POLICE DATA 

police <- police %>% 
  mutate(juvenile = case_when(age<18 ~ "Juvenile",
                               age>=18~"Adult")) %>% 
  mutate(date = as.Date(date,"%Y-%m-%d")) %>% 
  mutate(armed = ifelse(armed =="No", "Unarmed",armed)) %>%
  mutate(manner_of_death = str_to_title(manner_of_death),
         armed = str_to_title(armed),
         flee = str_to_title(flee),
         threat_level = str_to_title(threat_level)) %>% 
  mutate(gender =  ifelse(gender == "M", "Male","Female"),
         race =  ifelse(race == "W", "White",
                                          ifelse(race == "B", "Black",
                                                 ifelse(race == "A", "Asian/Pacific Islander",
                                                        ifelse(race == "O", "Other", 
                                                               ifelse(race == "N", "Native American",
                                                                      ifelse(race == "H", "Hispanic/Latino",
                                                                             "Unknown"))))))) %>% 
  mutate(year = substring(date,1,4))



# STATE POPULTION DATA 
state_race <- read.csv("race_per_state.csv",stringsAsFactors = FALSE)
state_race <- state_race %>%  rename("Hispanic/Latino" = Hispanic.Latino, "Asian/Pacific Islander" = Asian.Pacific.Islander, "Native American" = Native.American)
state_race <- melt(state_race, id.vars = "State", measure.vars = c("Hispanic/Latino", "White","Black", "Asian/Pacific Islander", "Native American"))
view(state_race)

state_race <- state_race %>% 
  arrange(State, variable) %>% 
  rename(raceethnicity = variable, percent_pop = value, state= State)


state_race$raceethnicity = as.character(state_race$raceethnicity)



# US POPULATION DATA 
raceethnicity = c("White",
         "Native American",
         "Hispanic/Latino",
         "Black",
         "Asian/Pacific Islander")

percent_pop = c(.7650,
                .0130,
                .1830,
                .1340,
                .0610)

race_stats = data.frame(raceethnicity,percent_pop)



#CITY RACE DATA 


city_race <- read.csv("city_race_clean.csv",stringsAsFactors = FALSE)

city_race <- city_race %>%  
  rename("Hispanic/Latino" = Hispanic, 
         "Asian/Pacific Islander" = Asian.Pacific.Islander,
         "Native American" = Native.American) %>% 
  mutate(White = as.numeric(White),
         Black = as.numeric(Black),
         `Hispanic/Latino` = as.numeric(`Hispanic/Latino`),
         `Native American` = as.numeric(`Native American`),
         `Asian/Pacific Islander` = as.numeric(`Asian/Pacific Islander`)) %>% 
  mutate(Majority = ifelse(White >= 50.0 , "White", 
                              ifelse(Black >= 50.0 , "Black",
                                ifelse(`Hispanic/Latino` >= 50.0 , "Hispanic/Latino",
                                  ifelse(`Native American` >= 50.0 , "Native American",
                                    ifelse(`Asian/Pacific Islander` >= 50.0 , "Asian/Pacific Islander", "No Majority")))))) 


View(city_race)

city_race <- city_race %>% 
    rename(raceethnicity = variable, city_percent_pop = value) %>% 
    mutate(city_percent_pop = city_percent_pop/100) %>% 
    mutate(Majority = replace_na(Majority,"Other")) %>% 
    
  
#SAVING EVERYTHING FOR EXPORT
saveRDS(city_race,"city_race.rds")
saveRDS(race_stats,"race_stats.rds")
saveRDS(police_final,"police_final.rds")
saveRDS(state_race,"state_race.rds")




