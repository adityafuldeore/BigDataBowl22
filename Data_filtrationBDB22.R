#CHAOS Score Coding - II. Data Filtration
#Big Data Bowl 2022
#Aditya Fuldeore & Andrew Stasell

#-------------------------------------------------------------------------------------------------------------------
all_merged <- merge(plays, PFFScoutingData, by = c("gameId", "playId"))
all_punts2 <- all_merged %>% filter(specialTeamsPlayType == "Punt") %>% filter(specialTeamsResult == "Return") %>% 
  filter(kickLength >= 40) %>% filter(kickLength <= 60) %>% filter(hangTime >= 3) %>% filter(hangTime <= 5) %>% 
  filter(!is.na(gunners)) %>% filter(!(str_detect(playDescription, "Lateral"))) %>% 
  filter(!(str_detect(returnerId, ";")))%>% 
  filter(!is.na(kickReturnYardage)) %>% filter(is.na(penaltyCodes))

all_merged %>% filter(specialTeamsPlayType == "Punt") %>% filter(specialTeamsResult == "Return") %>% 
  filter(kickLength < 40) %>% nrow() #172
all_merged %>% filter(specialTeamsPlayType == "Punt") %>% filter(specialTeamsResult == "Return") %>% 
  filter(kickLength > 60) %>% nrow() #109
all_merged %>% filter(specialTeamsPlayType == "Punt") %>% filter(specialTeamsResult == "Return") %>% 
  filter(hangTime < 3) %>% nrow() #11
all_merged %>% filter(specialTeamsPlayType == "Punt") %>% filter(specialTeamsResult == "Return") %>% 
  filter(hangTime > 5) %>% nrow() #86

#punt data filtered to only include kicks of length 40 to 60, hang time of 3 to 5 sec., no laterals or plays with multiple returners, and no plays with penalties, just clean returns.

punt_rec2 <- df_tracking %>% filter(event == "punt_received")

#All non-returns that are muffed or fair catches: didn't use downed or out of bounds or touchback - too much punter variability, less gunner impact
all_other <- all_merged %>% filter(specialTeamsPlayType == "Punt") %>% filter(specialTeamsResult != "Return") %>% 
  filter(kickLength >= 40) %>% filter(kickLength <= 60) %>% 
  filter(!is.na(gunners)) %>% filter(!(str_detect(playDescription, "Lateral"))) %>% 
  filter(!(str_detect(returnerId, ";")))%>% 
  filter(is.na(penaltyCodes))

pnt_fcmfd <- df_tracking %>% filter(event == "fair_catch" | event == "punt_muffed") #fair catches/muffed

frst_cntct <- df_tracking %>% filter(event == "first_contact") #first contact tracking data

post_pr <- df_tracking[which(df_tracking$event == "punt_received") + 10,] #tracking data one second after punt received

mean(all_punts2$kickReturnYardage) #average of 9.53 return yards on punts from data