#CHAOS Score Coding - III. CHAOS four factors
#Big Data Bowl 2022
#Aditya Fuldeore & Andrew Stasell

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
