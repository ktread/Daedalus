library(tidyverse)
library(leaflet)
library(googleVis)


# POLICE DATA FRAME

police <- read.csv("police_killings.csv",stringsAsFactors = FALSE)

#CONVERTING TYPES
police$state_fp <-as.numeric(police$state_fp)
police$county_fp <-  sprintf("%02d", police$county_fp)
police$county_fp <-as.numeric(police$county_fp)
police$county_fp<- sprintf("%03d", police$county_fp)
police$FIPS <- paste(police$state_fp,police$county_fp, sep='')
police$FIPS <- sprintf("%06d", as.numeric(police$FIPS))
police$age <- as.numeric(police$age)
police$pov <- as.numeric(police$pov)
police$share_white <- as.numeric(police$share_white)
police$share_black <- as.numeric(police$share_black)
police$share_white <- as.numeric(police$share_white)
police$day = sprintf("%02d", police$day)

##PREPROCESSING DATA 

p_clean <- police %>% 
  mutate(race_maj = case_when(share_white >= 50.0 ~ "White", 
                              share_black >= 50.0 ~ "Black",
                              share_hispanic >= 50.0 ~ "Hispanic/Latino")) %>% 
  mutate(race_maj = replace_na(race_maj,"Other")) %>% 
  mutate(is_minority = if_else(raceethnicity!=race_maj, 1,0)) %>% 
  mutate(pov_ratio = (pov/pop)*100) %>% 
  mutate(pov_ratio = case_when(pov_ratio>= 25 ~ 25,
                               pov_ratio<25 ~pov_ratio)) %>% 
  mutate(pov_ratio=na_if(pov_ratio, Inf)) %>% 
  mutate(pov_ratio.html.tooltip = pov_ratio) %>% 
  mutate(unemployment = urate*100) %>% 
  mutate(unemployment= case_when(unemployment>=25 ~ 25,
                                 unemployment<25 ~unemployment)) %>% 
  mutate(juvenile = case_when(age<18 ~ "Juvenile",
                               age>=18~"Adult")) %>% 
  mutate(date = paste(police$month,day,police$year,sep="/")) %>% 
  mutate(date = as.Date(date,"%B/%d/%Y")) %>% 
  mutate(agency = str_extract(police$lawenforcementagency, "State|Sheriff|Marshals|and|County|Us Border Patrol|FBI|ATF|US Forest Service|Police Department|US Border Patrol|Highway Patrol")) %>% 
  mutate(agency =  ifelse(agency == "Police Department", "City Police",
                               ifelse(agency == "Sheriff", "Sheriff",
                                      ifelse(agency == "Marshals", "Marshals",
                                             ifelse(agency == "State", "State Police", 
                                                    ifelse(agency == "Highway Patrol", 'Hightway Patrol',
                                                           ifelse(agency == "FBI", "FBI",
                                                                  ifelse(agency== "ATF","ATF", 
                                                                         ifelse(agency ==  "and", "Multiple Agencies", "Other Law Enforcement"))))))))) %>% 
  mutate(agency = replace_na(agency,"Unknown")) %>% 
  mutate(armed = ifelse(agency =="No", "Unarmed",armed)) %>% 
  rename("t_pop" = pop, t_person_income = p_income, t_house_income = h_income) 




## COUNTY DATA FROM 
county <- read.csv("names.csv",stringsAsFactors = FALSE, header = FALSE)

cl_county <- county %>% 
  rename(FIPS = V1, County = V2, State = V3)
  
  
cl_county$FIPS = as.numeric(cl_county$FIPS)
cl_county$FIPS = sprintf("%06d", cl_county$FIPS)

fp<- merge(p_clean,cl_county, by= "FIPS", all.x=TRUE)

colnames(fp)

## PIPING TO NEW CSV 


final_police <- fp %>% 
  select(-c(pov_ratio.html.tooltip ,FIPS ,day ,tract_ce ,namelsad ,year,
      state_fp ,geo_id ,month ,county_fp ,county_id,date)) %>% 
  mutate(t_pop = as.numeric(t_pop), 
         nat_bucket = as.numeric(nat_bucket),
         t_house_income = as.numeric(t_house_income),
         t_person_income = as.numeric( t_person_income),
         is_minority  = as.character(is_minority),
         county_income = as.numeric(county_income),
         county_bucket = as.numeric(county_bucket),
         share_hispanic = as.numeric(share_hispanic),
         
         
         
         )


sapply(final_police, class)

write.csv(final_police,"police_final.csv", row.names = FALSE)




