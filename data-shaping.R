library(dplyr) 
library(tidyr)
library(plotly)
library(leaflet)
library(googleVis)
options(digits=9)
police <- read.csv("police_killings.csv",stringsAsFactors = FALSE)
police$county_fp <-as.numeric(police$county_fp)
police$FIPS <- paste(police$state_fp,police$county_fp, sep='')
police$age <- as.numeric(police$age)
police$pov <- as.numeric(police$pov)
police$share_white <- as.numeric(police$share_white)
police <- police %>%  mutate(share_white = na_if(share_white, "-"))
police$share_black <- as.numeric(police$share_black)
police$share_hispanic <- as.numeric(police$share_hispanic)

#nc = st_read(system.file("shape/nc.shp", package="sf"))
#list.files("shape/nc.shp", package="sf")
#pfips <- pfips %>% 
#  select(FIPSNO.x,FIPSNO.y,CNTY_, CNTY_ID,)

police$county_fp<- sprintf("%03d", police$county_fp)
day = sprintf("%02d", police$day)

"State" %in% police$lawenforcementagency

## HISTOGRAM ON POVERTY AND UNEMPLOYMENT

police <- police %>% 
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
  mutate(agency = case_when("State" %in% police$lawenforcementagency, ~ "State Police",
                            "Police" %in% police$lawenforcementagency~ "Local Police",
                            "Marshall"%in% police$lawenforcementagency ~ "Marshall",
                            "Sheriff" %in% police$lawenforcementagency  ~ "Sheriff"
                                          ))
  

View(police)

poverty <- police %>% 
  select(state,pov_ratio, unemployment)



         





         
         





  





