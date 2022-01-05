#CHAOS Score Coding - Set-up and Functions
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
