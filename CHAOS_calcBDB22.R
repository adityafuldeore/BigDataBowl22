#CHAOS Score Coding - III.D: CHAOS calculation
#Big Data Bowl 2022
#Aditya Fuldeore & Andrew Stasell

#------------------------------------------------------------------------------------------------
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
