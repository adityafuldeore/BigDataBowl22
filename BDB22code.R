#Actionable code/findings
#add bar graphs/tier graphs
#maybe change some cut-offs here and there - to discuss
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
#getting distances
distance_disp <- function(x, y) {
  dist <- sqrt(x^2 + y^2)
  return(dist)
}

#Getting Gunners/vVses
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

#getting displacement of given tracking data of players from a returner on a given play
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
  
  displaced_return <- cbind(tracking_all, tot_xdisp, tot_ydisp) #GOT X and Y Displacement
  
  return(displaced_return)
  
}

#getting return yards periodically - maybe not used? - or only use for Peppers example
periodicRetYd <- function(returner_info) {
  
  ret_yd <- c()
  for (j in which(returner_info$event == "punt_received"):which(returner_info$event == "tackle")) {
    ret_yd <- c(ret_yd, returner_info[j,]$x - returner_info[j-1,]$x)
  }
  
  return(ret_yd)
  
}

#get all tracking punt data and tracking punt returner data
your_punt_all <- function(gmID, plyID, data) {
  return ( data %>% filter(gameId == gmID, playId == plyID) %>% as.data.frame() )
}

your_punt_returner <- function(gmID, plyID, retID, data) {
  return( data %>% 
            filter(gameId == gmID, playId == plyID, nflId == retID) %>%
            as.data.frame())
}

#add x and y displacement, as well as total displacement from returner to given tracking data
disps_to_ret <- function(trck_all, ret_info, punt_player) {
  disp_ret <- getDisplacement(trck_all, ret_info)
  #ret_yds <- periodicRetYd(ret_info)
  dist_retdisp <- distance_disp(disp_ret$tot_xdisp, disp_ret$tot_ydisp) #total distance to returner
  disp_ret <- cbind(disp_ret, dist_retdisp) #all players involved in return's displacement from returner (x,y,total distance)
  return(disp_ret)
}

#potentially used? - for Peppers return
all_disps_fn <- function(disp_ret_bs, disp_ret, r_info_bs, r_info_pr, punt_player) {
  dispobs_v <- c()
  dispobs_g <- c()
  dispobs_v_bs <- c()
  dispobs_g_bs <- c()
  for (v in 1:length(viseNum(punt_player$vises))) {
    dispobs_v <- rbind(dispobs_v, disp_ret %>%
                         filter(jerseyNumber == viseNum(punt_player$vises)[v]) %>%
                         filter(team == r_info_pr$team))
    dispobs_v_bs <- rbind(dispobs_v_bs, disp_ret_bs %>%
                            filter(jerseyNumber == viseNum(punt_player$vises)[v]) %>%
                            filter(team == r_info_bs$team))
  }
  for (g in 1:length(viseNum(punt_player$gunners))) {
    dispobs_g <- rbind(dispobs_g, disp_ret %>%
                         filter(jerseyNumber == gunnerNum(punt_player$gunners)[g]) %>%
                         filter(team != r_info_pr$team))
    dispobs_g_bs <- rbind(dispobs_g_bs, disp_ret_bs %>%
                            filter(jerseyNumber == gunnerNum(punt_player$gunners)[g]) %>%
                            filter(team != r_info_bs$team))
  }


  paired <- c()
  #edit to show ball_snap stuff?
  for(v in 1:nrow(dispobs_v_bs)) {
    for(g in 1:nrow(dispobs_g_bs)) {
      if(abs(dispobs_v_bs[v,]$y - dispobs_g_bs[g,]$y) <= 5) {
        paired <- rbind(paired, dispobs_v[v,], dispobs_g[g,])
      }
    }
  }
  #^getting and pairing gunners and vises
  gv_pairs <- c()
  if(is.null(paired)) {
    gv_pairs <- NA
  } else {

    gv_pairs <- c()
    for (a in 1:nrow(paired)) {
      if(a %% 2 == 0) {
        ind <- a-1
        name_Gunner <- paired[a,]$displayName
        dist_diff =
          disp_ret[disp_ret$displayName == paired[ind,]$displayName, ]$dist_retdisp -
          disp_ret[disp_ret$displayName == name_Gunner, ]$dist_retdisp
        #difference in distance to returner between gunner/vise
        xdiff_dist =
          disp_ret[disp_ret$displayName == paired[ind,]$displayName, ]$x -
          disp_ret[disp_ret$displayName == name_Gunner, ]$x
        ydiffdist =
          disp_ret[disp_ret$displayName == paired[ind,]$displayName, ]$y -
          disp_ret[disp_ret$displayName == name_Gunner, ]$y
        dist_VToG = distance_disp(xdiff_dist, ydiffdist)
        vise_name <- rep(paired[ind,]$displayName, length(dist_diff))
        gv_pairs <- rbind(gv_pairs,
                          cbind(disp_ret %>% filter(displayName == name_Gunner), dist_diff,
                                dist_VToG, vise_name))
      }

    }
  }
  return(gv_pairs)
}

#Actionable insights ---------------------------------------------------------------------------------------------------
all_merged <- merge(plays, PFFScoutingData, by = c("gameId", "playId"))
all_punts2 <- all_merged %>% filter(specialTeamsPlayType == "Punt") %>% filter(specialTeamsResult == "Return") %>% 
  filter(kickLength >= 40) %>% filter(kickLength <= 60) %>% filter(hangTime >= 3) %>% filter(hangTime <= 5) %>% 
  filter(!is.na(gunners)) %>% filter(!(str_detect(playDescription, "Lateral"))) %>% 
  filter(!(str_detect(returnerId, ";")))%>% 
  filter(!is.na(kickReturnYardage)) %>% filter(is.na(penaltyCodes))
#no laterals, invalid fair catches - only 2 laterals - icky on data
all_merged %>% filter(specialTeamsPlayType == "Punt") %>% filter(specialTeamsResult == "Return") %>% 
  filter(kickLength < 40) %>% nrow() #172
all_merged %>% filter(specialTeamsPlayType == "Punt") %>% filter(specialTeamsResult == "Return") %>% 
  filter(kickLength > 60) %>% nrow() #109
all_merged %>% filter(specialTeamsPlayType == "Punt") %>% filter(specialTeamsResult == "Return") %>% 
  filter(hangTime < 3) %>% nrow() #11
all_merged %>% filter(specialTeamsPlayType == "Punt") %>% filter(specialTeamsResult == "Return") %>% 
  filter(hangTime > 5) %>% nrow() #86

punt_rec2 <- df_tracking %>% filter(event == "punt_received")


#Modeling---------------------------------------------------------------------------------------------------------

#get number of vises, gunners
num_g <- c()
num_v <- c()
for(p in 1:nrow(all_punts2)) {
  num_g <- c(num_g, length(gunnerNum(all_punts2[p,]$gunners)))
  num_v <- c(num_v, length(viseNum(all_punts2[p,]$vises)))
}
all_punts2 <- cbind(all_punts2,  num_g, num_v)

punt_ret_mod <- lm(kickReturnYardage ~ as.factor(quarter) + yardsToGo + gameClock + absoluteYardlineNumber + kickLength + hangTime + as.factor(kickType) + as.factor(kickDirectionIntended) + as.factor(kickDirectionActual) + as.factor(returnDirectionIntended)+ as.factor(returnDirectionActual) +num_g +num_v, data = all_punts2) 
summary(punt_ret_mod)
#variable selection
final_pryoe_model <- step(punt_ret_mod, direction="backward", k = 2)
exp_pry <- predict(final_pryoe_model, all_punts2)
pryoe <- all_punts2$kickReturnYardage - predict(final_pryoe_model, all_punts2)

all_punts2 <- cbind(all_punts2, pryoe, exp_pry) 

all_punts2 %>% filter(gameId == 2019112402, playId == 1632) %>% as.data.frame()

#Actionable metrics/info-----------------------------------------------------------------------------------------

pryoe3 <- c()
pryoe2 <- c()
pryoe1 <- c()
retyds5 <- c()
retyds15 <- c()
retyds30 <- c()
ret_avgretdisp <- c()
for(r in 1:nrow(all_punts2)) {
  temp <- punt_rec2 %>% filter(gameId == all_punts2[r,]$gameId, playId == all_punts2[r,]$playId)
  if(nrow(temp != 0)) {
    r_info_pr <- your_punt_returner(all_punts2[r,]$gameId, all_punts2[r,]$playId, all_punts2[r,]$returnerId,
                                    punt_rec2)
    dtr_pr <- disps_to_ret(your_punt_all(all_punts2[r,]$gameId, all_punts2[r,]$playId, punt_rec2), 
                           r_info_pr, all_punts2[r,])
    
    proximity_to_ret <- cbind(dtr_pr, "WithinYdg" = 
                                ifelse(dtr_pr$dist_retdisp <= 5, 3, 
                                       ifelse(dtr_pr$dist_retdisp <= 15, 2,
                                              ifelse(dtr_pr$dist_retdisp <= 30, 1,0))),
                              "ret_yds" = all_punts2[r,]$kickReturnYardage)
    
    gunner_ret_disps <- proximity_to_ret %>% filter(jerseyNumber %in% gunnerNum(all_punts2[r,]$gunners)) %>% 
      filter(team != r_info_pr$team)
    closest_g <- gunner_ret_disps[which.min(gunner_ret_disps$dist_retdisp),]
    
    if(nrow(closest_g) != 0) {
      if(closest_g$WithinYdg == 3) {
        retyds5 <- c(retyds5, closest_g$ret_yds)
        pryoe3 <- c(pryoe3, all_punts2[r,]$pryoe)
      }
      if(closest_g$WithinYdg == 2) {
        retyds15 <- c(retyds15, closest_g$ret_yds)
        pryoe2 <- c(pryoe2, all_punts2[r,]$pryoe)
      }
      if(closest_g$WithinYdg == 1) {
        retyds30 <- c(retyds30, closest_g$ret_yds)
        pryoe1 <- c(pryoe1, all_punts2[r,]$pryoe)
      }
      ret_avgretdisp <- c(ret_avgretdisp, closest_g$dist_retdisp)
    }
  }
}
#can seperate by closest returner, all returners?? idk - get average disp when fair catch, muffed, etc.
#measure by if player hit this level of displacement or not
mean(retyds5, na.rm = T) #5.215232
sd(retyds5, na.rm = T)
mean(retyds15, na.rm = T) #9.384216
sd(retyds15, na.rm = T)
mean(retyds30, na.rm = T) #13.09915
sd(retyds30, na.rm = T)
mean(all_punts2$kickReturnYardage) #9.463415
mean(ret_avgretdisp, na.rm = T) #10.76788
mean(pryoe3, na.rm = T) #-2.110956
mean(pryoe2, na.rm = T) #-0.1261381
mean(pryoe1, na.rm = T) #1.512261


#scale the stat: 
1/ (all_punts2[2,]$kickLength - 30) #1st segment
1 + ((1/(30-15)) * (closest_g$dist_retdisp-15)) #second segment
2 + ((1/(15-5)) * (closest_g$dist_retdisp-5)) #third segment

#didn't use downed or out of bounds or touchback - too much punter variability, less gunner
all_other <- all_merged %>% filter(specialTeamsPlayType == "Punt") %>% filter(specialTeamsResult != "Return") %>% 
  filter(kickLength >= 40) %>% filter(kickLength <= 60) %>% 
  filter(!is.na(gunners)) %>% filter(!(str_detect(playDescription, "Lateral"))) %>% 
  filter(!(str_detect(returnerId, ";")))%>% 
  filter(is.na(penaltyCodes))

pnt_fcmfd <- df_tracking %>% filter(event == "fair_catch" | event == "punt_muffed")
#change fair catch to 1 sec prior to fair catch??
fc_avgretdisp <- c()
mfd_avgretdisp <- c()
for(j in 1:nrow(all_other)) {
  temp_fcmfd <- pnt_fcmfd %>% filter(gameId == all_other[j,]$gameId, playId == all_other[j,]$playId)
  if(nrow(temp_fcmfd != 0)) {
    r_info_fcmfd <- your_punt_returner(all_other[j,]$gameId, all_other[j,]$playId, all_other[j,]$returnerId,
                                       pnt_fcmfd)
    dtr_fcmfd <- disps_to_ret(your_punt_all(all_other[j,]$gameId, all_other[j,]$playId, pnt_fcmfd), 
                              r_info_fcmfd, all_other[j,])
    
    gnr_ret_disps_fcmfd <- dtr_fcmfd %>% filter(jerseyNumber %in% gunnerNum(all_other[j,]$gunners)) %>% 
      filter(team != r_info_fcmfd$team)
    closest_g_fcmfd <- gnr_ret_disps_fcmfd[which.min(gnr_ret_disps_fcmfd$dist_retdisp),]
    
    if(nrow(closest_g_fcmfd) != 0) {
      if(unique(temp_fcmfd$event) == "fair_catch") {
        fc_avgretdisp <- c(fc_avgretdisp, closest_g_fcmfd$dist_retdisp)
      }
      if(unique(temp_fcmfd$event) == "punt_muffed") {
        mfd_avgretdisp <- c(mfd_avgretdisp, closest_g_fcmfd$dist_retdisp)
      }
    }
    
  }
}
mean(fc_avgretdisp, na.rm = T) #4.073713
mean(mfd_avgretdisp, na.rm = T) #7.450931
#proximity of gunner to returner - factor in hangtime, punt distance - fair catch = full pts - downed/muffed - how? - do seperate analyses? - how close is gunner on fair catch/downed/muffed punts? - something like average ret yds over expected prevented or allowed?
#if fair catch, and gunner within x yards, full points? if muffed/downed, and gunner within x yards, full points?
# - maybe proximity score seperate, chaos score for returns alltogether
#sure we want to limit kick length?


#first contact - filter all first_contacts - if any is gunner...- what should length away be for first contact
frst_cntct <- df_tracking %>% filter(event == "first_contact")

pryoe_fc <- c()
fc_retyds <- c()
pryoe_nonfc <- c()
nonfc_retyds <- c()
for(f in 1:nrow(all_punts2)){
  temp2 <- frst_cntct %>% filter(gameId == all_punts2[f,]$gameId, playId == all_punts2[f,]$playId)
  if(nrow(temp2 != 0)) {
    r_info_fc <- your_punt_returner(all_punts2[f,]$gameId, all_punts2[f,]$playId, all_punts2[f,]$returnerId,
                                    frst_cntct)
    dtr_fc <- disps_to_ret(your_punt_all(all_punts2[f,]$gameId, all_punts2[f,]$playId, frst_cntct), 
                           r_info_fc, all_punts2[f,])
    fc <- dtr_fc %>% filter(jerseyNumber %in% gunnerNum(all_punts2[f,]$gunners)) %>% filter(team != r_info_fc$team)
    for(d in 1:nrow(fc)) {
      if(fc[d,]$dist_retdisp <= 1.5) {
        fc_retyds <- c(fc_retyds , all_punts2[f,]$kickReturnYardage)
        pryoe_fc <- c(pryoe_fc, all_punts2[f,]$pryoe)
      } else {
        nonfc_retyds <- c(nonfc_retyds , all_punts2[f,]$kickReturnYardage)
        pryoe_nonfc <- c(pryoe_nonfc, all_punts2[f,]$pryoe)
      }
    }
  }
}
mean(fc_retyds, na.rm = T) 
mean(pryoe_fc, na.rm = T) 
mean(nonfc_retyds, na.rm = T)
mean(pryoe_nonfc, na.rm = T)
#explain why used 1.5

#tackle - filter all tackles - if any is gunner...
# - how to do fumbles? missed tackles? - no tackle score if not tackling
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
pryoe_tkl <- c()
pryoe_asttkl <- c()
pryoe_msdtkl <- c()
gtckl_retyds <- c()
gasttckl_retyds <- c()
gmsdtckl_retyds <- c()
avg_retdisp_tkl <- c()
avg_retdisp_asttkl <- c()
avg_retdisp_msdtkl <- c()
for(s in 1:nrow(all_punts2)) {
  r_info_tkl <- your_punt_returner(all_punts2[s,]$gameId, all_punts2[s,]$playId, all_punts2[s,]$returnerId,
                                   punt_rec2)
  if(nrow(r_info_tkl) != 0) {
    dtr_tkl <- disps_to_ret(your_punt_all(all_punts2[s,]$gameId, all_punts2[s,]$playId, punt_rec2), 
                            r_info_tkl, all_punts2[s,])
    
    if(!is.na(all_punts2[s,]$tackler)) {
      gunner_tackle <- all_punts2[s,] %>% filter(tkl_num(tackler) %in% gunnerNum(gunners))
      if(nrow(gunner_tackle) != 0) {
        tklr <- dtr_tkl %>% filter(jerseyNumber == tkl_num(gunner_tackle$tackler)) %>% 
          filter(team != r_info_tkl$team)
        gtckl_retyds <- c(gtckl_retyds, gunner_tackle$kickReturnYardage)
        avg_retdisp_tkl <- c(avg_retdisp_tkl, tklr$dist_retdisp)
        pryoe_tkl <- c(pryoe_tkl, gunner_tackle$pryoe)
      }
    }
    if(!is.na(all_punts2[s,]$assistTackler)) {
      gunner_asttckl <- all_punts2[s,] %>% filter(tkl_num(assistTackler) %in% gunnerNum(gunners))
      if(nrow(gunner_asttckl) != 0) {
        atklr <- dtr_tkl %>% filter(jerseyNumber == tkl_num(gunner_asttckl$assistTackler)) %>% 
          filter(team != r_info_tkl$team)
        gasttckl_retyds <- c(gasttckl_retyds, gunner_asttckl$kickReturnYardage)
        avg_retdisp_asttkl <- c(avg_retdisp_asttkl, atklr$dist_retdisp)
        pryoe_asttkl <- c(pryoe_asttkl, gunner_asttckl$pryoe)
      }
    }
    if(!is.na(all_punts2[s,]$missedTackler)) {
      for(q in 1:length(tkl_num(all_punts2[s,]$missedTackler))) {
        if(tkl_num(all_punts2[s,]$missedTackler)[q] %in% gunnerNum(all_punts2[s,]$gunners)) {
          jn <- as.numeric(tkl_num(all_punts2[s,]$missedTackler)[q])
          mtklr <- dtr_tkl %>% filter(jerseyNumber == jn) %>% 
            filter(team != r_info_tkl$team)
          gmsdtckl_retyds <- c(gmsdtckl_retyds, all_punts2[s,]$kickReturnYardage)
          avg_retdisp_msdtkl <- c(avg_retdisp_msdtkl, mtklr$dist_retdisp)
          pryoe_msdtkl <- c(pryoe_msdtkl, all_punts2[s,]$pryoe)
        }
      }
    }
  }
  
}
mean(gtckl_retyds, na.rm = T) #5.510721
mean(gasttckl_retyds, na.rm = T) #5.789474
mean(gmsdtckl_retyds, na.rm = T) #10.3169
mean(avg_retdisp_tkl, na.rm = T) #10.94863
mean(avg_retdisp_asttkl, na.rm = T) #13.1506
mean(avg_retdisp_msdtkl, na.rm = T) #10.44276
mean(pryoe_tkl, na.rm = T) #-2.734773
mean(pryoe_asttkl, na.rm = T) #-3.622784
mean(pryoe_msdtkl, na.rm = T) # 0.3006439
#missed tackle = close enough

#angle change of returner - tough - angle changed whenever gunner within 5 yards? - do we even use this or too much confounding to only credit this to gunners - just have this as section?
#orientation - how returner jives - fakes or moves drastically
#10 obs after punt_rec2 of kick returner
post_pr <- df_tracking[which(df_tracking$event == "punt_received") + 10,]

angl_retdisp <- c()
strt_retdisp <- c()
pryoe_angl <- c()
pryoe_strt <- c()
angl_recyds <- c()
strt_recyds <- c()
for(z in 1:nrow(all_punts2)) {
  temp3 <- punt_rec2 %>% filter(gameId == all_punts2[z,]$gameId, playId == all_punts2[z,]$playId)
  if(nrow(temp3) != 0) {
    r_info_a <- your_punt_returner(all_punts2[z,]$gameId, all_punts2[z,]$playId, all_punts2[z,]$returnerId,
                                   punt_rec2)
    dtr_a <- disps_to_ret(your_punt_all(all_punts2[z,]$gameId, all_punts2[z,]$playId, punt_rec2), 
                          r_info_a, all_punts2[z,])
    r_info_a2 <- your_punt_returner(all_punts2[z,]$gameId, all_punts2[z,]$playId, all_punts2[z,]$returnerId,
                                    post_pr)
    dtr_a2 <- disps_to_ret(your_punt_all(all_punts2[z,]$gameId, all_punts2[z,]$playId, post_pr), 
                           r_info_a, all_punts2[z,])
    
    gnr_ret_disps_pr <- dtr_a %>% filter(jerseyNumber %in% gunnerNum(all_punts2[z,]$gunners)) %>% 
      filter(team != r_info_a$team)
    
    closest_g_angl <- gnr_ret_disps_pr[which.min(gnr_ret_disps_pr$dist_retdisp),]
    
    closest_def <- dtr_a %>% filter(team != r_info_a$team) %>% filter(displayName != "football")
    closest_def <- closest_def[which.min(closest_def$dist_retdisp),]
    
    if(nrow(closest_g_angl != 0)) {
      if(closest_def$displayName == closest_g_angl$displayName) {
        if(closest_g_angl$dist_retdisp <= 30) {
          if((abs(r_info_a2$o - r_info_a$o) >= 45 & abs(r_info_a2$o - r_info_a$o) <= 315) | 
             (r_info_a2$s - r_info_a$s) <= 0.5) {
            angl_recyds <- c(angl_recyds, all_punts2[z,]$kickReturnYardage)
            pryoe_angl <- c(pryoe_angl,all_punts2[z,]$pryoe)
            angl_retdisp <- c(angl_retdisp, closest_g_angl$dist_retdisp)
          } else {
            strt_recyds <- c(strt_recyds, all_punts2[z,]$kickReturnYardage)
            pryoe_strt <- c(pryoe_strt,all_punts2[z,]$pryoe)
            strt_retdisp <- c(strt_retdisp, closest_g_angl$dist_retdisp)
          }
        }
      }
    }
  }
  
  #change in angle between 
}
mean(angl_recyds, na.rm = T) #7.952614
mean(strt_recyds, na.rm = T) #10.64235
mean(pryoe_angl, na.rm = T) #-0.5715652
mean(pryoe_strt, na.rm = T) #0.08179736
mean(angl_retdisp, na.rm = T) #8.474356
mean(strt_retdisp, na.rm = T) #10.02699
#orientation - want to see if direction player faces changes


#MAYBE MODEL SIGNIFICANCE OF THESE FACTORS ON RET YDS?? ~within ydg + angle change + tackle/missed/ast(Factor) + first contact

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
        , 2 ) #closest gunner
      
      angleScore <- ifelse(closest_def_all$displayName == closest_g_all$displayName & closest_g_all$dist_retdisp <= 30,
        ifelse((abs(r_info_all2$o - r_info_all$o) >= 45 & abs(r_info_all2$o - r_info_all$o) <= 315) | 
          ((r_info_a2$s - r_info_a$s) <= 0.5), 1, 0), 0) #closest gunner being closest defender?
      
      #the first tackle type - if related to the gunner - what gunner did first - missed a tackle, completed, assisted
      tklScore <- c()
      if(!is.na(all_punts2[a,]$missedTackler)) {
        for(n in 1:length(tkl_num(all_punts2[a,]$missedTackler))) {
          if(tkl_num(all_punts2[a,]$missedTackler)[n] == closest_g_all$jerseyNumber) { #%in% gnrs
            tklScore <- 1
          } 
        }
      }
      if(is.null(tklScore)) {
        if(!is.na(all_punts2[a,]$tackler)) {
          if(tkl_num(all_punts2[a,]$tackler) == closest_g_all$jerseyNumber) { #%in% gnrs
            tklScore <- 3
          } else if(!is.na(all_punts2[a,]$assistTackler)) {
            if(tkl_num(all_punts2[a,]$assistTackler) == closest_g_all$jerseyNumber) { #%in% gnrs
              tklScore <- 2
            } else {
              tklScore <- 0
            }
          } else {
            tklScore <- 0
          }
        } else {
          tklScore <- 0
        }
      }
      
      r_info_fc1 <- your_punt_returner(all_punts2[a,]$gameId, all_punts2[a,]$playId, all_punts2[a,]$returnerId,
                                       frst_cntct)
      if(nrow(r_info_fc1) != 0) {
        dtr_fc1 <- disps_to_ret(your_punt_all(all_punts2[a,]$gameId, all_punts2[a,]$playId, frst_cntct), 
                                r_info_fc1, all_punts2[a,])
        dtr_fc1 <- dtr_fc1[!duplicated(dtr_fc1$displayName),]
        fc1 <- dtr_fc1 %>% filter(jerseyNumber == closest_g_all$jerseyNumber) %>% filter(team != r_info_all$team)
        #%in% gnrs
        #minretdisp_fc <- fc1[which.min(fc1$dist_retdisp),]
        frstCntctScore <- ifelse(fc1$dist_retdisp <= 1.5, 1, 0) #minretdisp_fc$dist_retdisp 
      } else {
        frstCntctScore <- 0
      }

      pr_modset2 <- cbind(all_punts2[a,], "distToRet" = closest_g_all$dist_retdisp, proximityScore,
                          angleScore, tklScore, frstCntctScore, "closest_gunner" = closest_g_all$displayName) 
      #all relative to closest gunner on play, not all gunners
      
      pr_modset <- rbind(pr_modset, pr_modset2)
    }
  }
  
}
#figure out how to do with all gunners? if time
#for/if loops looping through gunners and seeing if that gunner closest, run through all gunners in that play
#create fn to calculate chaos for any guy :
chaos_calc <- function(play) {
  
}


#analysis/evaluation
chaos_data <- pr_modset %>% mutate(ChaosScore = proximityScore + angleScore + tklScore + frstCntctScore)
#closest gunners on each play
#Closeness Score (proximity) + Hit Score (tkl) + Angle + stopping Score (angle) + Opening contact score (frstcntc) Score
chaos_data %>% filter(gameId == all_punts2[863,]$gameId, playId == all_punts2[863,]$playId)
#result data could be skewed by instances in data where maybe immediate tackles not counting first contact?

punt_ret_model <- lm(kickReturnYardage ~ ChaosScore, data = chaos_data)
summary(punt_ret_model)

boxplot(chaos_data$ChaosScore)
ggplot(data = chaos_data, aes(x = ChaosScore, y = kickReturnYardage)) + 
  geom_point(aes(color = proximityScore)) + 
  geom_abline(intercept = punt_ret_model$coefficients[1], slope = punt_ret_model$coefficients[2], color = "blue")

all_punts2[863,] #mcmanis gets a 3.32/8, patterson gets 1smtnish/8
#do we really weight tackling so much?
#make asterisk - scores best used seperately? - person that does everything right except tackle/contact: 4/8

#--------------------------------------------------------------------------------------------------------Peppers ex.

CHI_NYGpunt <- PFFScoutingData %>% filter(gameId == 2019112402, playId == 1632) #pff punt data

df_tracking %>% filter(gameId == 2019112402, playId == 1632, event != "None") %>% as.data.frame() %>% filter(jerseyNumber == 21 | jerseyNumber == 82 | jerseyNumber == 57 | jerseyNumber == 27)

punt_Peppers <- merge( (plays %>% filter(kickLength == 51, grepl("Peppers", playDescription, fixed = TRUE), possessionTeam == "CHI")), 
                       CHI_NYGpunt, by = "gameId")
punt_Peppers <- cbind(punt_Peppers, (players_df %>% filter(nflId == 44837))) #all punt data except tracking


df_tracking %>% filter(gameId == 2019112402, playId == 1632, event != "None") %>% as.data.frame() %>% filter(jerseyNumber == gunnerNum(punt_Peppers$gunners)$G1 | jerseyNumber == gunnerNum(punt_Peppers$gunners)$G2)

returner_info_p <- df_tracking %>% filter(gameId == 2019112402, playId == 1632, nflId == punt_Peppers$returnerId) %>% as.data.frame() #Peppers (returner) tracking info

peppers_tracking_all <- df_tracking %>% filter(gameId == 2019112402, playId == 1632) %>% as.data.frame() #all involved tracking info

displaced_return_peppers <- getDisplacement(peppers_tracking_all, returner_info_p)
ret_yd_p <- periodicRetYd(returner_info_p)
disp_peppers <- distance_disp(displaced_return_peppers$tot_xdisp, displaced_return_peppers$tot_ydisp)
displaced_return_peppers <- cbind(displaced_return_peppers, disp_peppers) #all players involved in return's displacement from returner (x,y,total distance)

PFFScoutingData[19,] %>% mutate(gunnerNums = toString(gunnerNum(gunners)), viseNums = toString(viseNum(vises)))
plot(abs(displaced_return_peppers[((18*nrow(returner_info_p))+which(returner_info_p$event == "punt_received")):((18*nrow(returner_info_p))+which(returner_info_p$event == "tackle")),]$tot_xdisp), cumsum(-1*ret_yd_p))
plot(abs(displaced_return_peppers[((18*nrow(returner_info_p))+which(returner_info_p$event == "punt_received")):((18*nrow(returner_info_p))+which(returner_info_p$event == "tackle")),]$tot_ydisp), cumsum(-1*ret_yd_p))

plot(abs(displaced_return_peppers[((3*nrow(returner_info_p))+which(returner_info_p$event == "punt_received")):((3*nrow(returner_info_p))+which(returner_info_p$event == "tackle")),]$disp_peppers), cumsum(-1*ret_yd_p))

dispobs_v <- c()
dispobs_g <- c()
for (g in 1:length(viseNum(punt_Peppers$vises))) {
  dispobs_v <- rbind(dispobs_v, displaced_return_peppers %>% filter(team == returner_info_p$team) %>% 
                       filter(jerseyNumber == viseNum(punt_Peppers$vises)[g]))
  dispobs_g <- rbind(dispobs_g, displaced_return_peppers %>% filter(team != returner_info_p$team) %>% 
                       filter(jerseyNumber == gunnerNum(punt_Peppers$gunners)[g]) )
}
dispobs_v
dispobs_g
dispobs_v <- dispobs_v %>% filter(event == "ball_snap")
dispobs_g <- dispobs_g %>% filter(event == "ball_snap")

paired <- c()
for(v in 1:nrow(dispobs_v)) {
  for(g in 1:nrow(dispobs_g)) {
    if (abs(dispobs_v[v,]$y - dispobs_g[g,]$y) <= 5) {
      paired <- rbind(paired, dispobs_v[v,], dispobs_g[g,])
    }
  }
} #^getting and pairing gunners and vises

displaced_return_peppers %>% 
  filter(displayName == paired[1,]$displayName) %>% 
  mutate(dist_diff =  
           displaced_return_peppers[displaced_return_peppers$displayName == paired[1,]$displayName, ]$disp_peppers -           displaced_return_peppers[displaced_return_peppers$displayName == paired[2,]$displayName, ]$disp_peppers)
name_Gunner <- paired[a,]$displayName
cbind(displaced_return_peppers %>% filter(displayName == name_Gunner), dist_diff)

gv_pairs <- c()
for (a in 1:nrow(paired)) {
  if(a %% 2 == 0) {
    ind <- a-1
    name_Gunner <- paired[a,]$displayName
    dist_diff = 
      displaced_return_peppers[displaced_return_peppers$displayName == paired[ind,]$displayName, ]$disp_peppers -   displaced_return_peppers[displaced_return_peppers$displayName == name_Gunner, ]$disp_peppers 
    #difference in distance to returner between gunner/vise
    xdiff_dist = 
      displaced_return_peppers[displaced_return_peppers$displayName == paired[ind,]$displayName, ]$x -   displaced_return_peppers[displaced_return_peppers$displayName == name_Gunner, ]$x 
    ydiffdist = 
      displaced_return_peppers[displaced_return_peppers$displayName == paired[ind,]$displayName, ]$y -   displaced_return_peppers[displaced_return_peppers$displayName == name_Gunner, ]$y
    dist_VToG = distance_disp(xdiff_dist, ydiffdist)
    vise_name <- rep(paired[ind,]$displayName, length(dist_diff))
    gv_pairs <- rbind(gv_pairs, 
                      cbind(displaced_return_peppers %>% filter(displayName == name_Gunner), dist_diff, 
                            dist_VToG, vise_name))
  }
  
}
gv_pairs #vise distance diff with gunner's distance to returner, distance between each other, gunner name matched

plot(abs(gv_pairs[((2*nrow(returner_info_p))+which(gv_pairs$event == "punt_received")):((2*nrow(returner_info_p))+which(gv_pairs$event == "tackle")),]$dist_VToG), cumsum(-1*ret_yd_p))
