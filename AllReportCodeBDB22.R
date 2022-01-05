#CHAOS Score Coding - All parts together
#Big Data Bowl 2022
#Aditya Fuldeore & Andrew Stasell
#Load Data----------------------------------------------------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(ggrepel)
games <- data.frame(read_csv("games.csv"))
PFFScoutingData <- data.frame(read_csv("PFFScoutingData.csv"))
plays <- data.frame(read_csv("plays.csv"))
players_df <- data.frame(read_csv("players.csv"))
df_tracking <- data.frame()
for(s in seq(2018, 2020)){
  
  df_tracking_temp <- read_csv(paste0("tracking",s,".csv"),
                               col_types = cols())
  
  df_tracking <- bind_rows(df_tracking_temp, df_tracking)                            
  
}
#Functions----------------------------------------------------------------------------------------------------

#Calculating Distances between two points
distance_disp <- function(x, y) {
  dist <- sqrt(x^2 + y^2)
  return(dist)
}

#Getting Gunner/vise jersey numbers given a certain play's gunners/vises
#EX: gunnerNum(play$gunners) spits out jersey numbers of gunners
gunnerNum <- function(g) {
  G1 <- str_split(str_split(g, " ")[[1]], ";")
  G <- c()
  gNum <- NULL
  for (i in 1:length(G1)) {
    if(i %% 2 == 0) {
      gNum <- str_split(str_split(g, " ")[[1]], ";")[[i]][1]
    }
    G <- c(G, gNum)
  }
  return(G[!duplicated(G)])
}

viseNum <- function(vise) {
  v1 <- str_split(str_split(vise, " ")[[1]], ";")
  v <- c()
  vNum <- NULL
  for (i in 1:length(v1)) {
    if(i %% 2 == 0) {
      vNum <- str_split(str_split(vise, " ")[[1]], ";")[[i]][1]
    }
    v <- c(v, vNum)
  }
  return(v[!duplicated(v)])
}

#Getting displacement of players from a returner on a given play (how far a defender is from the returner)
#feed in the tracking data from a play and the returner's tracking data from that play
getDisplacement <- function(tracking_all, returner_info) {
  
  tot_xdisp <- matrix(nrow = nrow(tracking_all), ncol = 1)
  tot_ydisp <- matrix(nrow = nrow(tracking_all), ncol = 1)
  for (k in 1:23) {
    xdisp <- c()
    ydisp <- c()
    for (i in 1:nrow(returner_info)) {
      x_component <- returner_info[i,]$x - tracking_all[i+((k-1)*nrow(returner_info)),]$x
      xdisp <- c(xdisp, x_component)
      y_component <- returner_info[i,]$y - tracking_all[i+((k-1)*nrow(returner_info)),]$y
      ydisp <- c(ydisp, y_component)
    }
    ind <- (((k-1)*nrow(returner_info)) + 1):(k*nrow(returner_info))
    tot_xdisp[ind,] <- xdisp
    tot_ydisp[ind,] <- ydisp
  }
  
  displaced_return <- cbind(tracking_all, tot_xdisp, tot_ydisp) #Got X and Y Displacement
  
  return(displaced_return)
  
}

#Gets all tracking punt data and tracking punt returner data for particular play, given gameId, playId, 
#and dataset from which to get the play information from
your_punt_all <- function(gmID, plyID, data) {
  return ( data %>% filter(gameId == gmID, playId == plyID) %>% as.data.frame() )
}

your_punt_returner <- function(gmID, plyID, retID, data) {
  return( data %>% 
            filter(gameId == gmID, playId == plyID, nflId == retID) %>%
            as.data.frame())
}

#Add x and y displacement, as well as total displacement from returner to given tracking data
#feed in your_punt_all(), your_punt_returner(), and the particular play from which you want tracking data from
#EX: disps_to_ret(your_punt_all(~), your_punt_returner(~), plays[1,])
disps_to_ret <- function(trck_all, ret_info, punt_player) {
  disp_ret <- getDisplacement(trck_all, ret_info)
  dist_retdisp <- distance_disp(disp_ret$tot_xdisp, disp_ret$tot_ydisp) #total distance to returner
  disp_ret <- cbind(disp_ret, dist_retdisp) #all players involved in return's displacement from returner (x,y,total distance)
  return(disp_ret)
}

#Get the jersey numbers of tacklers on a play
#EX: tkl_Num(play$tacklers) spits out jersey numbers of tackler(s)
tkl_num <- function(tkl) {
  t1 <- str_split(str_split(tkl, " ")[[1]], ";")
  t <- c()
  tNum <- NULL
  for (i in 1:length(t1)) {
    if(i %% 2 == 0) {
      tNum <- str_split(str_split(tkl, " ")[[1]], ";")[[i]][1]
    }
    t <- c(t, tNum)
  }
  return(t[!duplicated(t)])
}

#Data Filtration----------------------------------------------------------------------------------------------
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

#4 Factors -------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------------------------------

#Proximity Score analysis (Closeness) -----------------------------

retyds5 <- c()
retyds15 <- c()
retyds30 <- c()
for(r in 1:nrow(all_punts2)) {
  
  temp <- punt_rec2 %>% filter(gameId == all_punts2[r,]$gameId, playId == all_punts2[r,]$playId)
  
  if(nrow(temp != 0)) { #check that punt exists out of punt reception tracking data
    
    r_info_pr <- your_punt_returner(all_punts2[r,]$gameId, all_punts2[r,]$playId, all_punts2[r,]$returnerId,
                                    punt_rec2) #get returner tracking data
    
    dtr_pr <- disps_to_ret(your_punt_all(all_punts2[r,]$gameId, all_punts2[r,]$playId, punt_rec2), 
                           r_info_pr, all_punts2[r,])
    #get distance/displacement from returner for each player in play - use your_punt_all() for tracking data of play, 
    #returner tracking data, and current punt in loop for this
    
    proximity_to_ret <- cbind(dtr_pr, "WithinYdg" = 
                                ifelse(dtr_pr$dist_retdisp <= 5, 3, 
                                       ifelse(dtr_pr$dist_retdisp <= 15, 2,
                                              ifelse(dtr_pr$dist_retdisp <= 30, 1,0))),
                              "ret_yds" = all_punts2[r,]$kickReturnYardage)
    #categorize players based on proximity interval (0-5, 5-15, 15-30, 30+)
    
    gunner_ret_disps <- proximity_to_ret %>% filter(jerseyNumber %in% gunnerNum(all_punts2[r,]$gunners)) %>% 
      filter(team != r_info_pr$team)
    closest_g <- gunner_ret_disps[which.min(gunner_ret_disps$dist_retdisp),]
    #get closest gunner on play for calculating average return yards in each interval
    
    if(nrow(closest_g) != 0) { #check there is a gunner for this
      if(closest_g$WithinYdg == 3) { #0-5
        retyds5 <- c(retyds5, closest_g$ret_yds)
      }
      if(closest_g$WithinYdg == 2) { #5-15
        retyds15 <- c(retyds15, closest_g$ret_yds)
      }
      if(closest_g$WithinYdg == 1) { #15-30
        retyds30 <- c(retyds30, closest_g$ret_yds)
      }
    }
    #calculated average return yards at each interval
  }
}
mean(retyds5, na.rm = T) #4.55615
mean(retyds15, na.rm = T) #9.446571
mean(retyds30, na.rm = T) #13.92083


#first contact - gunner within 1.5 yds at first contact (Hit) -------------------

fc_retyds <- c()
nonfc_retyds <- c()
for(f in 1:nrow(all_punts2)){
  temp2 <- frst_cntct %>% filter(gameId == all_punts2[f,]$gameId, playId == all_punts2[f,]$playId)
  if(nrow(temp2 != 0)) { #check punt exists in first contact tracking set
    r_info_fc <- your_punt_returner(all_punts2[f,]$gameId, all_punts2[f,]$playId, all_punts2[f,]$returnerId,
                                    frst_cntct)
    dtr_fc <- disps_to_ret(your_punt_all(all_punts2[f,]$gameId, all_punts2[f,]$playId, frst_cntct), 
                           r_info_fc, all_punts2[f,])
    #got returner, all player tracking data for the play, distances to returner
    #no need to get one gunner for this play, as only one can get first contact anyway
    fc <- dtr_fc %>% filter(jerseyNumber %in% gunnerNum(all_punts2[f,]$gunners)) %>% filter(team != r_info_fc$team)
    #get tracking details of first contact gunner, if there is one
    for(d in 1:nrow(fc)) { #check if there is a gunner that initiated first contact
      if(fc[d,]$dist_retdisp <= 1.5) {
        fc_retyds <- c(fc_retyds , all_punts2[f,]$kickReturnYardage)
      } else {
        nonfc_retyds <- c(nonfc_retyds , all_punts2[f,]$kickReturnYardage)
      }
    }
  }
}
#Average return yards if gunner initiates first contact or not
mean(fc_retyds, na.rm = T)  #6.974239
mean(nonfc_retyds, na.rm = T) #10.43082


#Tackle analysis (Stop) ------------------------------

gtckl_retyds <- c()
gasttckl_retyds <- c()
gmsdtckl_retyds <- c()
for(s in 1:nrow(all_punts2)) {
  r_info_tkl <- your_punt_returner(all_punts2[s,]$gameId, all_punts2[s,]$playId, all_punts2[s,]$returnerId,
                                   punt_rec2)
  if(nrow(r_info_tkl) != 0) { #check if there is a returner from this play in the punt received tracking data
    dtr_tkl <- disps_to_ret(your_punt_all(all_punts2[s,]$gameId, all_punts2[s,]$playId, punt_rec2), 
                            r_info_tkl, all_punts2[s,])
    #get tracking data, distances to returner
    
    if(!is.na(all_punts2[s,]$tackler)) {
      gunner_tackle <- all_punts2[s,] %>% filter(tkl_num(tackler) %in% gunnerNum(gunners))
      if(nrow(gunner_tackle) != 0) {
        tklr <- dtr_tkl %>% filter(jerseyNumber == tkl_num(gunner_tackle$tackler)) %>% 
          filter(team != r_info_tkl$team)
        gtckl_retyds <- c(gtckl_retyds, gunner_tackle$kickReturnYardage)
      }
    } #check if tackler is a gunner
    if(!is.na(all_punts2[s,]$assistTackler)) {
      gunner_asttckl <- all_punts2[s,] %>% filter(tkl_num(assistTackler) %in% gunnerNum(gunners))
      if(nrow(gunner_asttckl) != 0) {
        atklr <- dtr_tkl %>% filter(jerseyNumber == tkl_num(gunner_asttckl$assistTackler)) %>% 
          filter(team != r_info_tkl$team)
        gasttckl_retyds <- c(gasttckl_retyds, gunner_asttckl$kickReturnYardage)
      }
    } #check if assist tackler is gunner (only 1 tackler/assist tackler per play)
    if(!is.na(all_punts2[s,]$missedTackler)) {
      for(q in 1:length(tkl_num(all_punts2[s,]$missedTackler))) {
        if(tkl_num(all_punts2[s,]$missedTackler)[q] %in% gunnerNum(all_punts2[s,]$gunners)) {
          jn <- as.numeric(tkl_num(all_punts2[s,]$missedTackler)[q])
          mtklr <- dtr_tkl %>% filter(jerseyNumber == jn) %>% 
            filter(team != r_info_tkl$team)
          gmsdtckl_retyds <- c(gmsdtckl_retyds, all_punts2[s,]$kickReturnYardage)
        }
      }
    } #check if one of the missed tacklers was a gunner
  }
  
}
#Average return yards for each tackle type
mean(gtckl_retyds, na.rm = T) #5.784884
mean(gasttckl_retyds, na.rm = T) #5.5
mean(gmsdtckl_retyds, na.rm = T) #10.05076


#Angle change of returner or if returner's change in speed is <= 0.5 one second after punt received ------------
#(Angle and Obstruction)

#meant ret yds not rec 
angl_recyds <- c() #Angled
strt_recyds <- c() #Neither Angled or Obstruction disruptions
anglspd_recyds <- c() #Angled and Obstruction
spd_recyds <- c() #Obstruction
for(z in 1:nrow(all_punts2)) {
  temp3 <- punt_rec2 %>% filter(gameId == all_punts2[z,]$gameId, playId == all_punts2[z,]$playId)
  if(nrow(temp3) != 0) { #check punt exists in punt received tracking data
    r_info_a <- your_punt_returner(all_punts2[z,]$gameId, all_punts2[z,]$playId, all_punts2[z,]$returnerId,
                                   punt_rec2)
    dtr_a <- disps_to_ret(your_punt_all(all_punts2[z,]$gameId, all_punts2[z,]$playId, punt_rec2), 
                          r_info_a, all_punts2[z,])
    r_info_a2 <- your_punt_returner(all_punts2[z,]$gameId, all_punts2[z,]$playId, all_punts2[z,]$returnerId,
                                    post_pr)
    dtr_a2 <- disps_to_ret(your_punt_all(all_punts2[z,]$gameId, all_punts2[z,]$playId, post_pr), 
                           r_info_a, all_punts2[z,])
    #get returner, all tracking data, and distances to returner for when punt is received and one sec after
    
    gnr_ret_disps_pr <- dtr_a %>% filter(jerseyNumber %in% gunnerNum(all_punts2[z,]$gunners)) %>% 
      filter(team != r_info_a$team)
    
    closest_g_angl <- gnr_ret_disps_pr[which.min(gnr_ret_disps_pr$dist_retdisp),]
    #get closest gunner
    
    closest_def <- dtr_a %>% filter(team != r_info_a$team) %>% filter(displayName != "football")
    closest_def <- closest_def[which.min(closest_def$dist_retdisp),]
    #get closest defender on play
    
    if(nrow(closest_g_angl != 0)) { #check there is a closest defender - removes an obs or two with data entry bugs
      if(closest_def$displayName == closest_g_angl$displayName) { #check that closest def is closest gunner
        if((abs(r_info_a2$o - r_info_a$o) >= 45 & abs(r_info_a2$o - r_info_a$o) <= 315) | 
           (r_info_a2$s - r_info_a$s) <= 0.5) { 
          #check if returner angle changed >= 45 degrees or speed changed <= 0.5 y/s one sec after punt received
          
          if((abs(r_info_a2$o - r_info_a$o) >= 45 & abs(r_info_a2$o - r_info_a$o) <= 315) &
             ((r_info_a2$s - r_info_a$s) <= 0.5)) { #check if both happened
            anglspd_recyds <- c(anglspd_recyds, all_punts2[z,]$kickReturnYardage)
            
          } else if((abs(r_info_a2$o - r_info_a$o) >= 45 & abs(r_info_a2$o - r_info_a$o) <= 315)) { 
            #otherwise, if just angle
            angl_recyds <- c(angl_recyds, all_punts2[z,]$kickReturnYardage)
            
          } else if ((r_info_a2$s - r_info_a$s) <= 0.5){
            #otherwise, if just speed/obstruction
            spd_recyds <- c(spd_recyds, all_punts2[z,]$kickReturnYardage)
            
          }
        } else {
          strt_recyds <- c(strt_recyds, all_punts2[z,]$kickReturnYardage)
        }
      }
    }
  }
}
#average return yards under each condition
mean(angl_recyds, na.rm = T) #9.231293
mean(strt_recyds, na.rm = T) #10.67019
mean(anglspd_recyds, na.rm = T) #4.876543
mean(spd_recyds, na.rm = T) # 8.118644

#CHAOS calculation--------------------------------------------------------------------------------------------

#Chaos of all gunners ~ not just closest gunner - all 4 factors on 0-1 scale
chaos_calc_initial <- function(play) {
  r_info_calc <- your_punt_returner(play$gameId, play$playId, play$returnerId,
                                    punt_rec2)
  #returner info
  CHAOS <- c()
  if(nrow(r_info_calc) != 0) { #check returner in the punt received tracking data
    
    dtr_calc <- disps_to_ret(your_punt_all(play$gameId, play$playId, punt_rec2), 
                             r_info_calc, play)
    #all tracking data and distances to returner
    
    r_info_calc2 <- your_punt_returner(play$gameId, play$playId, play$returnerId,
                                       post_pr)
    #returner info for one sec after punt is received
    
    gnrs <- gunnerNum(play$gunners)
    gunner_ret_disps_calc <- dtr_calc %>% filter(jerseyNumber %in% gnrs) %>%
      filter(team != r_info_calc$team)
    #gunner info on play
    
    if(nrow(gunner_ret_disps_calc) != 0) { #check there are gunners on play
      
      for(b in 1:nrow(gunner_ret_disps_calc)) { #loop through all gunners on play
        
        closest_def_calc <- dtr_calc %>% filter(team != r_info_calc$team) %>% filter(displayName != "football")
        closest_def_calc <- closest_def_calc[which.min(closest_def_calc$dist_retdisp),]
        #get closest defender (for Angle score)
        
        g_calc <- gunner_ret_disps_calc[b,]
        #current gunner info
        
        proximityScore = round(
          ifelse(g_calc$dist_retdisp <= 5, 1, 
                 ifelse(g_calc$dist_retdisp <= 15, 
                        (2/3) + ((15-g_calc$dist_retdisp)*((1/3)/(15-5))),
                        ifelse(g_calc$dist_retdisp <= 30, 
                               (1/3) + ((30-g_calc$dist_retdisp)*((1/3)/(30-15))),
                               ((play$kickLength-g_calc$dist_retdisp)*
                                  ((1/3)/(play$kickLength-30))) )))
          , 2 ) #Proximity Score 0-1
        
        angleScore <- ifelse(closest_def_calc$displayName == g_calc$displayName,
                             ifelse(
                               (abs(r_info_calc2$o - r_info_calc$o) >= 45 & 
                                  abs(r_info_calc2$o - r_info_calc$o) <= 315) | 
                                 ((r_info_calc2$s - r_info_calc$s) <= 0.5) , 
                               ifelse((abs(r_info_calc2$o - r_info_calc$o) >= 45 & 
                                         abs(r_info_calc2$o - r_info_calc$o) <= 315) & 
                                        ((r_info_calc2$s - r_info_calc$s) <= 0.5), 1, 0.5), 0), 0)
        #angle change of 45 needs absolute value of angles <= 315 because a change of 0 degrees to 315 degrees from the
        #data is calculated as 315 degrees, but in reality is 45 degrees
        #Angle Score 0-1
        
        tklScore <- 0 #initialize score to 0 - if it misses all checks it stays at 0
        if(!is.na(play$missedTackler)) {
          for(n in 1:length(tkl_num(play$missedTackler))) {
            if(tkl_num(play$missedTackler)[n] == g_calc$jerseyNumber) {
              tklScore <- 0.25
            } 
          }
        }
        if(!is.na(play$tackler)) {
          if(tkl_num(play$tackler) == g_calc$jerseyNumber) { 
            if(tkl_num(play$tackler) %in% tkl_num(play$missedTackler)) {#check if tackler also missed tackle
              tklScore <- 0.5
            } else {
              tklScore <- 1
            }
          } else if(!is.na(play$assistTackler)) {
            if(tkl_num(play$assistTackler) == g_calc$jerseyNumber) { 
              if(tkl_num(play$assistTackler) %in% tkl_num(play$missedTackler)) {#check if ast tackler also missed tackle
                tklScore <- 0.5
              } else {
                tklScore <- 0.75
              }
            } 
          }
        }
        
        r_info_fccalc <- your_punt_returner(play$gameId, play$playId, play$returnerId,
                                            frst_cntct)
        #get returner info for first contact
        
        if(nrow(r_info_fccalc) != 0) {
          dtr_fccalc <- disps_to_ret(your_punt_all(play$gameId, play$playId, frst_cntct), 
                                     r_info_fccalc, play)
          dtr_fccalc <- dtr_fccalc[!duplicated(dtr_fccalc$displayName),]
          fccalc <- dtr_fccalc %>% filter(jerseyNumber == g_calc$jerseyNumber) %>% filter(team != r_info_calc$team)
          #get gunners in first contact tracking dadta
          
          frstCntctScore <- ifelse(fccalc$dist_retdisp <= 1.5, 1, 0)
        } else {
          frstCntctScore <- 0
        }
        
        #final CHAOS calc
        CHAOS <- rbind(CHAOS, cbind(g_calc, "punt_ret_yds" = play$kickReturnYardage,
                                    proximityScore, tklScore, angleScore, frstCntctScore, 
                                    "CHAOSscore" = round(
                                      ((proximityScore) + tklScore + angleScore + frstCntctScore)*(10/4) , 2) ) )
        
      }
    }
  }
  
  return(CHAOS)
}

allCHAOS_initial <- c()
for(a in 1:nrow(all_punts2)) {
  allCHAOS_initial <- rbind(allCHAOS_initial, chaos_calc_initial(all_punts2[a,]))
}

library(randomForest)
library(vip)
library(ranger)
set.seed(1)
rf_punt_ret <- ranger(punt_ret_yds ~ proximityScore + tklScore + angleScore + frstCntctScore, data = allCHAOS_initial,
                      num.trees = 100, importance = "impurity")
vip(rf_punt_ret)
#proximity score is significantly the most important - scale it as out of 3 - 1 for each of the 3 levels

#FINAL CHAOS OF ALL GUNNERS CALCULATION - with proximity edited - everything else same from prior calculation
chaos_calc<- function(play) {
  r_info_calc <- your_punt_returner(play$gameId, play$playId, play$returnerId,
                                    punt_rec2)
  CHAOS <- c()
  if(nrow(r_info_calc) != 0) {
    
    dtr_calc <- disps_to_ret(your_punt_all(play$gameId, play$playId, punt_rec2), 
                             r_info_calc, play)
    r_info_calc2 <- your_punt_returner(play$gameId, play$playId, play$returnerId,
                                       post_pr)
    gnrs <- gunnerNum(play$gunners)
    gunner_ret_disps_calc <- dtr_calc %>% filter(jerseyNumber %in% gnrs) %>%
      filter(team != r_info_calc$team)
    
    if(nrow(gunner_ret_disps_calc) != 0) {
      
      for(b in 1:nrow(gunner_ret_disps_calc)) {
        
        closest_def_calc <- dtr_calc %>% filter(team != r_info_calc$team) %>% filter(displayName != "football")
        closest_def_calc <- closest_def_calc[which.min(closest_def_calc$dist_retdisp),]
        
        g_calc <- gunner_ret_disps_calc[b,]
        
        proximityScore = round(
          ifelse(g_calc$dist_retdisp <= 5, 3, 
                 ifelse(g_calc$dist_retdisp <= 15, 
                        2 + ((15-g_calc$dist_retdisp)*(1/(15-5))),
                        ifelse(g_calc$dist_retdisp <= 30, 
                               1 + ((30-g_calc$dist_retdisp)*(1/(30-15))),
                               ((play$kickLength-g_calc$dist_retdisp)*
                                  (1/(play$kickLength-30))) )))
          , 2 ) #Now 0-3 scale
        
        angleScore <- ifelse(closest_def_calc$displayName == g_calc$displayName,
                             ifelse(
                               (abs(r_info_calc2$o - r_info_calc$o) >= 45 & 
                                  abs(r_info_calc2$o - r_info_calc$o) <= 315) | 
                                 ((r_info_calc2$s - r_info_calc$s) <= 0.5) , 
                               ifelse((abs(r_info_calc2$o - r_info_calc$o) >= 45 & 
                                         abs(r_info_calc2$o - r_info_calc$o) <= 315) & 
                                        ((r_info_calc2$s - r_info_calc$s) <= 0.5), 1, 0.5), 0), 0)
        
        tklScore <- 0
        if(!is.na(play$missedTackler)) {
          for(n in 1:length(tkl_num(play$missedTackler))) {
            if(tkl_num(play$missedTackler)[n] == g_calc$jerseyNumber) {
              tklScore <- 0.25
            } 
          }
        }
        if(!is.na(play$tackler)) {
          if(tkl_num(play$tackler) == g_calc$jerseyNumber) { 
            if(tkl_num(play$tackler) %in% tkl_num(play$missedTackler)) {
              tklScore <- 0.5
            } else {
              tklScore <- 1
            }
          } else if(!is.na(play$assistTackler)) {
            if(tkl_num(play$assistTackler) == g_calc$jerseyNumber) { 
              if(tkl_num(play$assistTackler) %in% tkl_num(play$missedTackler)) {
                tklScore <- 0.5
              } else {
                tklScore <- 0.75
              }
            } 
          }
        }
        
        r_info_fccalc <- your_punt_returner(play$gameId, play$playId, play$returnerId,
                                            frst_cntct)
        if(nrow(r_info_fccalc) != 0) {
          dtr_fccalc <- disps_to_ret(your_punt_all(play$gameId, play$playId, frst_cntct), 
                                     r_info_fccalc, play)
          dtr_fccalc <- dtr_fccalc[!duplicated(dtr_fccalc$displayName),]
          fccalc <- dtr_fccalc %>% filter(jerseyNumber == g_calc$jerseyNumber) %>% filter(team != r_info_calc$team)
          
          frstCntctScore <- ifelse(fccalc$dist_retdisp <= 1.5, 1, 0)
        } else {
          frstCntctScore <- 0
        }
        
        CHAOS <- rbind(CHAOS, cbind(g_calc, "punt_ret_yds" = play$kickReturnYardage,
                                    proximityScore, tklScore, angleScore, frstCntctScore, 
                                    "CHAOSscore" = round(
                                      ((proximityScore) + tklScore + angleScore + frstCntctScore)*(10/6) , 2) ) )
        
      }
    }
  }
  
  return(CHAOS)
}

allCHAOS <- c()
for(a in 1:nrow(all_punts2)) {
  allCHAOS <- rbind(allCHAOS, chaos_calc(all_punts2[a,]))
}
#Calculated all CHAOS scores

#Chaos of gunners closest to returner when punt received for each play
pr_modset <- c()
for(a in 1:nrow(all_punts2)) {
  r_info_all <- your_punt_returner(all_punts2[a,]$gameId, all_punts2[a,]$playId, all_punts2[a,]$returnerId,
                                   punt_rec2)
  
  if(nrow(r_info_all) != 0) {
    
    dtr_all <- disps_to_ret(your_punt_all(all_punts2[a,]$gameId, all_punts2[a,]$playId, punt_rec2), 
                            r_info_all, all_punts2[a,])
    r_info_all2 <- your_punt_returner(all_punts2[a,]$gameId, all_punts2[a,]$playId, all_punts2[a,]$returnerId,
                                      post_pr)
    gnrs <- gunnerNum(all_punts2[a,]$gunners)
    gunner_ret_disps_all <- dtr_all %>% filter(jerseyNumber %in% gnrs) %>%
      filter(team != r_info_all$team)
    
    if(nrow(gunner_ret_disps_all) != 0) {
      closest_def_all <- dtr_all %>% filter(team != r_info_all$team) %>% filter(displayName != "football")
      closest_def_all <- closest_def_all[which.min(closest_def_all$dist_retdisp),]
      closest_g_all <- gunner_ret_disps_all[which.min(gunner_ret_disps_all$dist_retdisp),]
      
      proximityScore = round(
        ifelse(closest_g_all$dist_retdisp <= 5, 3, 
               ifelse(closest_g_all$dist_retdisp <= 15, 
                      2 + ((15-closest_g_all$dist_retdisp)*(1/(15-5))),
                      ifelse(closest_g_all$dist_retdisp <= 30, 
                             1 + ((30-closest_g_all$dist_retdisp)*(1/(30-15))),
                             ((all_punts2[a,]$kickLength-closest_g_all$dist_retdisp)*
                                (1/(all_punts2[a,]$kickLength-30))) )))
        , 2 )
      
      angleScore <- ifelse(closest_def_all$displayName == closest_g_all$displayName,
                           ifelse((abs(r_info_all2$o - r_info_all$o) >= 45 & abs(r_info_all2$o - r_info_all$o) <= 315) |
                                    ((r_info_all2$s - r_info_all$s) <= 0.5), 
                                  ifelse((abs(r_info_all2$o - r_info_all$o) >= 45 & 
                                            abs(r_info_all2$o - r_info_all$o) <= 315) & 
                                           ((r_info_all2$s - r_info_all$s) <= 0.5), 1, 0.5), 0),0)
      
      tklScore <- 0
      if(!is.na(all_punts2[a,]$missedTackler)) {
        for(n in 1:length(tkl_num(all_punts2[a,]$missedTackler))) {
          if(tkl_num(all_punts2[a,]$missedTackler)[n] == closest_g_all$jerseyNumber) {
            tklScore <- 0.25
          } 
        }
      }
      if(!is.na(all_punts2[a,]$tackler)) {
        if(tkl_num(all_punts2[a,]$tackler) == closest_g_all$jerseyNumber) {
          if(tkl_num(all_punts2[a,]$tackler) %in% tkl_num(all_punts2[a,]$missedTackler)) {
            tklScore <- 0.5
          } else {
            tklScore <- 1
          }
        } else if(!is.na(all_punts2[a,]$assistTackler)) {
          if(tkl_num(all_punts2[a,]$assistTackler) == closest_g_all$jerseyNumber) {
            if(tkl_num(all_punts2[a,]$assistTackler) %in% tkl_num(all_punts2[a,]$missedTackler)) {
              tklScore <- 0.25
            } else {
              tklScore <- 0.75
            }
          } 
        }
      }
      
      r_info_fc1 <- your_punt_returner(all_punts2[a,]$gameId, all_punts2[a,]$playId, all_punts2[a,]$returnerId,
                                       frst_cntct)
      if(nrow(r_info_fc1) != 0) {
        dtr_fc1 <- disps_to_ret(your_punt_all(all_punts2[a,]$gameId, all_punts2[a,]$playId, frst_cntct), 
                                r_info_fc1, all_punts2[a,])
        dtr_fc1 <- dtr_fc1[!duplicated(dtr_fc1$displayName),]
        fc1 <- dtr_fc1 %>% filter(jerseyNumber == closest_g_all$jerseyNumber) %>% filter(team != r_info_all$team)
        frstCntctScore <- ifelse(fc1$dist_retdisp <= 1.5, 1, 0) 
      } else {
        frstCntctScore <- 0
      }
      
      pr_modset2 <- cbind(all_punts2[a,], "distToRet" = closest_g_all$dist_retdisp, proximityScore,
                          angleScore, tklScore, frstCntctScore, "closest_gunner" = closest_g_all$displayName) 
      
      pr_modset <- rbind(pr_modset, pr_modset2)
    }
  }
  
}

chaos_closegunner <-  pr_modset %>% mutate(CHAOSscore = round(
  ((proximityScore) + (tklScore) + (angleScore) + (frstCntctScore))*(10/6), 2))
#CHAOS scores of just closest gunners to returners at time punt received on plays

mean(allCHAOS$CHAOSscore) #4.24 average CHAOS score

#CHAOS distribution
ggplot(allCHAOS, aes(x = event, y = CHAOSscore)) + 
  geom_boxplot(color = "red", fill = "orange") + ggtitle("Distribution of CHAOSscores") + 
  xlab("All Instances of Punt Returns")

sum(allCHAOS$CHAOSscore == 10) #22 perfect scores
sum(allCHAOS$CHAOSscore >= 7.5) #195 scores above 7.5
quantile(allCHAOS$CHAOSscore) #75% under 5.42, 50% between 2.78 and 5.42


#CHAOS evaluation-----------------------------------------------------------------------------------------------

#2022 Pro Bowl Special Teamers
allCHAOS %>% group_by(displayName) %>% 
  summarise(obs = n(), chaos_avg = mean(CHAOSscore), 
            px_avg = mean(proximityScore), 
            tkl_avg = mean(tklScore), fc_avg = mean(frstCntctScore), angl_avg = mean(angleScore)) %>% 
  filter(displayName == "J.T. Gray" | displayName == "Matthew Slater")

#CHAOS Perfect Scores
allCHAOS %>% filter(CHAOSscore == 10) %>% group_by(displayName) %>% 
  summarise(perfects = n()) %>% arrange(desc(perfects))


#Gunners with 20+ plays from 2018-2020
top_gunners <- allCHAOS %>% group_by(displayName) %>% 
  summarise(obs = n(), chaos_avg = mean(CHAOSscore), 
            punt_retyds_avg = mean(punt_ret_yds), 
            ts_avg = mean(tklScore), ps_avg = mean(proximityScore), 
            fc_avg = mean(frstCntctScore), as_avg = mean(angleScore)) %>% 
  arrange(desc(chaos_avg)) %>% filter(obs >= 18)

#plot gunners with 20+ plays
ggplot(data = top_gunners, aes(x = chaos_avg, y = punt_retyds_avg)) + 
  geom_point(aes(size = obs, color = ps_avg)) + geom_text_repel(aes(label = displayName)) +
  geom_vline(xintercept = mean(allCHAOS$CHAOSscore)) + 
  geom_hline(yintercept = mean(allCHAOS$punt_ret_yds)) + xlab("Average CHAOS score") + 
  ylab("Average Punt Return Yards Allowed") + 
  labs(title = "Avg. CHAOS vs Avg. Punt Return Yards Allowed",
       subtitle = "Size of dots are the # of punts fielded, lighter color means better average Proximity Score") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))

#small model to demonstrate relationship between CHAOS and return yards for plot
punt_ret_model <- lm(kickReturnYardage ~ CHAOSscore, data = chaos_closegunner)
#plot of closest gunner of all punts in data
ggplot(data = chaos_closegunner, aes(x = CHAOSscore, y = kickReturnYardage)) + 
  geom_point(aes(color = proximityScore)) +  geom_vline(xintercept = mean(chaos_closegunner$CHAOSscore)) +
  geom_hline(yintercept = mean(chaos_closegunner$kickReturnYardage)) +
  geom_abline(intercept = punt_ret_model$coefficients[1], slope = punt_ret_model$coefficients[2], color = "red") + 
  xlab("CHAOS score") + ylab("Punt Return Yards Allowed") + 
  labs(title = "CHAOS vs Punt Return Yards Allowed",
       subtitle = "Only the closest gunner on 1567 punts, lighter color means better Proximity Score") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +
  annotate("text", x = 8, y = -10, label = "Good CHAOS, limited return") +
  annotate("text", x = 8, y = 70, label = "Good CHAOS, large return") +
  annotate("text", x = 2.5, y = -10, label = "Bad CHAOS, limited return") +
  annotate("text", x = 2.5, y = 50, label = "Bad CHAOS, large return")

#% of observations above CHAOS mean with return yds above mean return yards
( allCHAOS %>% filter(CHAOSscore > mean(allCHAOS$CHAOSscore)) %>% 
    filter(punt_ret_yds>=mean(allCHAOS$punt_ret_yds)) %>% nrow() ) / (allCHAOS %>% filter(CHAOSscore > mean(allCHAOS$CHAOSscore))) %>% nrow #30.26%

#% of observations below CHAOS mean with return yds above mean return yards
( allCHAOS %>% filter(CHAOSscore < mean(allCHAOS$CHAOSscore)) %>% 
    filter(punt_ret_yds>=mean(allCHAOS$punt_ret_yds)) %>% nrow() ) / (allCHAOS %>% filter(CHAOSscore < mean(allCHAOS$CHAOSscore)) %>% nrow()) #47.11%


#muffed/fair catch proximity scores
prx_mfd_fc <- c()
for(d in 1:nrow(all_other)) {
  r_info_o <- your_punt_returner(all_other[d,]$gameId, all_other[d,]$playId, all_other[d,]$returnerId,
                                 pnt_fcmfd)
  
  if(nrow(r_info_o) != 0) {
    
    dtr_o <- disps_to_ret(your_punt_all(all_other[d,]$gameId, all_other[d,]$playId, pnt_fcmfd), 
                          r_info_o, all_other[d,])
    
    gnrsO <- gunnerNum(all_other[d,]$gunners)
    gunner_ret_disps_o <- dtr_o %>% filter(jerseyNumber %in% gnrsO) %>%
      filter(team != r_info_o$team)
    if(nrow(gunner_ret_disps_o) != 0) {
      
      for(b in 1:nrow(gunner_ret_disps_o)) {
        
        g_calc_o <- gunner_ret_disps_o[b,]
        
        proximityScore = round(
          ifelse(g_calc_o$dist_retdisp <= 5, 3, 
                 ifelse(g_calc_o$dist_retdisp <= 15, 
                        2 + ((15-g_calc_o$dist_retdisp)*(1/(15-5))),
                        ifelse(g_calc_o$dist_retdisp <= 30, 
                               1 + ((30-g_calc_o$dist_retdisp)*(1/(30-15))),
                               ((all_other[d,]$kickLength-g_calc_o$dist_retdisp)*
                                  (1/(all_other[d,]$kickLength-30))) )))
          , 2 )
        
        
        prx_mfd_fc <- rbind(prx_mfd_fc, cbind(g_calc_o,
                                              proximityScore))
      }
    }
  }
}

#matthew slater avg proximity score when covering muffed/fair catch punts
prx_mfd_fc %>% filter(displayName == "Matthew Slater") %>% 
  summarise(avg_ps = mean(proximityScore))