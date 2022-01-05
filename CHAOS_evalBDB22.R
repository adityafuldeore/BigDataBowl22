#CHAOS Score Coding - IV. CHAOS evaluation
#Big Data Bowl 2022
#Aditya Fuldeore & Andrew Stasell

#--------------------------------------------------------------------------------------

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

