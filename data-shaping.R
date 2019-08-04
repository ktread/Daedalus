library(tidyverse)
library(reshape2)

# IMPORTING DATA 
police <- read.csv("police.csv",stringsAsFactors = FALSE)
state_geo <- read.csv("state_detail.csv",stringsAsFactors = FALSE)
state_race <- read.csv("state_race.csv",stringsAsFactors = FALSE)
city_race <- read.csv("city_race_clean.csv",stringsAsFactors = FALSE)


#JOINING GEO DATA 
state_geo <- state_geo %>% 
  rename(state_name = State)
police <- left_join(police,state_geo, by = c("state" = "State.Code"))



#CONVERTING TYPES
police$age <- as.numeric(police$age)


##PREPROCESSING POLICE DATA 

police <- police %>% 
  mutate(juvenile = case_when(age<18 ~ "Juvenile",
                               age>=18~"Adult")) %>% 
  mutate(date = as.Date(date,"%Y-%m-%d")) %>% 
  mutate(armed = ifelse(armed =="", "Unknown",armed)) %>%
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
  mutate(year = substring(date,1,4)) %>% 
  rename(victim_race = race)


names(police) <- str_to_title(names(police))
# CLEANING STATE POPULATION DATA
state_race <- state_race %>%  
          rename("Hispanic/Latino" = Hispanic.Latino, 
                  "Asian/Pacific Islander" = Asian.Pacific.Islander, 
                  "Native American" = Native.American) %>% 
          mutate(percent_state_us_pop = as.numeric(percent_state_us_pop)) %>% 
          mutate(State_Majority = ifelse(White >= .500 , "White", 
                           ifelse(Black >= .500 , "Black",
                                  ifelse(`Hispanic/Latino` >= .500 , "Hispanic/Latino",
                                         ifelse(`Native American` >= .500 , "Native American",
                                                ifelse(`Asian/Pacific Islander` >= .500 , "Asian/Pacific Islander", "No Majority")))))) 


state_race <- melt(state_race, id.vars = c("State","State_Majority","state_pop_raw",	"percent_state_us_pop"), measure.vars = c("Hispanic/Latino", "White","Black", "Asian/Pacific Islander", "Native American"))


state_race <- state_race %>% 
  arrange(State, variable) %>% 
  rename(state_race = variable, percent_state_pop = value, state= State)


state_race$state_race = as.character(state_race$state_race)
view(state_race)




# CLEANING NATIONAL POPULATION DATA
us_race = c("White",
         "Native American",
         "Hispanic/Latino",
         "Black",
         "Asian/Pacific Islander")

us_percent_pop = c(.7650,
                .0130,
                .1830,
                .1340,
                .0610)

us_race = data.frame(us_race,us_percent_pop)




# CLEANING CITY POPULATION DATA
city_race <- city_race %>%  
  rename("Hispanic/Latino" = Hispanic, 
         "Asian/Pacific Islander" = Asian.Pacific.Islander,
         "Native American" = Native.American) %>% 
  mutate(White = as.numeric(White),
         Black = as.numeric(Black),
         `Hispanic/Latino` = as.numeric(`Hispanic/Latino`),
         `Native American` = as.numeric(`Native American`),
         `Asian/Pacific Islander` = as.numeric(`Asian/Pacific Islander`)) %>% 
  mutate(White = White/100,
         Black = Black/100,
         `Hispanic/Latino` = `Hispanic/Latino`/100,
         `Native American` = `Native American`/100,
         `Asian/Pacific Islander` = `Asian/Pacific Islander`/100) %>% 
  mutate(City_Majority = ifelse(White >= .500 , "White", 
                              ifelse(Black >= .500 , "Black",
                                ifelse(`Hispanic/Latino` >= .500 , "Hispanic/Latino",
                                  ifelse(`Native American` >= .500 , "Native American",
                                    ifelse(`Asian/Pacific Islander` >= .500 , "Asian/Pacific Islander", "No Majority")))))) 


city_race <- melt(city_race, id.vars = c("City","State","City_Majority"), measure.vars = c("Hispanic/Latino", "White","Black", "Asian/Pacific Islander", "Native American"))

city_race <- city_race %>% 
  rename(city_race = variable, percent_city_pop = value)

#SAVING EVERYTHING FOR EXPORT
saveRDS(police,"police_final.rds")
saveRDS(state_race,"state_race.rds")
saveRDS(us_race,"us_race.rds")
saveRDS(city_race,"city_stats.rds")




