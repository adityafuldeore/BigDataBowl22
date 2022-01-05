#Visual - Bears-Giants punt
#Big Data Bowl 2022
#Aditya Fuldeore & Andrew Stasell

#---------------------------------------------------------------------------------------------------
#BDB VISUALIZATION - Kaggle Tim Bliss - https://www.kaggle.com/tombliss/analyzing-place-kicker-offset-from-center-r
#Loading pre-installed libraries
library(tidyverse)
library(gganimate)
library(cowplot)
library(ggridges)
library(repr)
library(gganimate)
source("gg_field.R") #https://github.com/mlfurman3/gg_field
set.seed(6)
PFFScoutingData <- data.frame(read_csv("PFFScoutingData.csv"))
str(PFFScoutingData)
plays <- data.frame(read_csv("plays.csv"))
str(plays)
tracking2020 <- data.frame(read_csv("tracking2020.csv"))
str(tracking2020)
players_df <- data.frame(read_csv("players.csv"))
str(players_df)
#turning off warnings
options(warn=-1)
#setting plot width and height
options(repr.plot.width=15, repr.plot.height = 10)
head(players_df)

#Reading tracking data (needs to be done iteratively)
#blank dataframe to store tracking data
df_tracking <- data.frame()
#iterating through all weeks
for(s in seq(2018, 2020)){
  
  #temperory dataframe used for reading season for given iteration
  df_tracking_temp <- read_csv(paste0("tracking",s,".csv"),
                               col_types = cols())
  
  #storing temporary dataframe in full season dataframe
  df_tracking <- bind_rows(df_tracking_temp, df_tracking)                            
  
}

df_tracking <- df_tracking %>%
  mutate(x = ifelse(playDirection == "left", 120-x, x),
         y = ifelse(playDirection == "left", 160/3 - y, y))

df_ballPunt <- df_tracking %>%
  #filtering for football
  filter( displayName == "football") %>%
  #selecting plays as ball crosses through uprights
  #grouping by gameId and playId
  group_by(gameId, playId) %>%
  arrange(gameId, playId, frameId) %>%
  #filtering for play immediately after ball goes through uprights
  filter(lag(x) < 75, x >= 40) %>%
  #selecting first occurence in case it crosses uprights multiple times
  filter(row_number() == 1) %>%
  #ungrouping
  ungroup() %>%
  #creating variable for absolute offset from center.
  #Center of field is at coordinate 160/6
  mutate(offsetFromCenter = abs(y - 160/6)) %>%
  #selecting offsetFromCenter and key variables only
  select(gameId, playId, offsetFromCenter)

df_PuntAnalysis <- plays %>%
  #filtering for unblocked extra points and field goals only
  filter(specialTeamsResult %in% c("Return",
                                   "Fair Catch"),
         #using play description to remove attempts that were missed short           
         !grepl('No Good, Short', playDescription), specialTeamsPlayType == "Punt") %>%
  #kickLength is sometimes missing on extra points.
  #In that case we use impute as yards from target endzone + 18.
  mutate(yardsFromTargetEndzone = case_when(yardlineNumber == 50 ~ 50,possessionTeam == yardlineSide ~ 50 + yardlineNumber,possessionTeam != yardlineSide ~ yardlineNumber),
         #imputing kick length as yards from target endzone + 18
         kickLength = ifelse(is.na(kickLength),
                             yardsFromTargetEndzone + 18,
                             kickLength)) %>%
  #joining players by kickerId to get displayName
  inner_join(players_df, by = c('kickerId' = 'nflId')) %>%
  #joining filtered tracking data
  inner_join(df_ballPunt, by = c("gameId", 'playId')) %>%
  #selecting only variables of interest:
  select(gameId, playId,  displayName,
         kickLength, offsetFromCenter,
         playDescription)

df_PuntAnalysis %>% head()

#picking the lowest offset play---------------------------------------------------------
example_play <- df_PuntAnalysis %>%
  filter(kickLength ==51) %>% filter(displayName == "Pat O'Donnell") %>% 
  filter(offsetFromCenter == max(offsetFromCenter))

#merging tracking data to play
example_play <- inner_join(example_play,
                           df_tracking,
                           by = c("gameId" = "gameId",
                                  "playId" = "playId"))


#attributes used for plot. first is away, second is football, third is home.
cols_fill <- c("dodgerblue1", "#663300", "firebrick1")
cols_col <- c("#000000", "#663300", "#000000")
size_vals <- c(6, 4, 6)
shape_vals <- c(21, 16, 21)
plot_title <- example_play$playDescription[1]
nFrames <- max(example_play$frameId)

#plotting
anim <- ggplot() +
  
  # #creating field underlay
   gg_field(yardmin = 0, yardmax = 120) +
  
  #filling forest green for behind back of endzone
  theme(panel.background = element_rect(fill = 'forestgreen',
                                        color = 'forestgreen'),
        panel.grid = element_blank()) +
  

  #setting size and color parameters
  scale_size_manual(values = size_vals, guide = FALSE) + 
  scale_shape_manual(values = shape_vals, guide = FALSE) +
  scale_fill_manual(values = cols_fill, guide = FALSE) + 
  scale_colour_manual(values = cols_col, guide = FALSE) +
  #adding players
  geom_point(data = example_play, aes(x = x,
                                      y = y, 
                                      shape = team,
                                      fill = team,
                                      group = nflId,
                                      size = team,
                                      colour = team), alpha = 0.7) +  
  #adding jersey numbers
  geom_text(data = example_play,
            aes(x = x, y = y, label = jerseyNumber),
            colour = "white", 
            vjust = 0.36, size = 3.5) + 
  #titling plot with play description
  labs(title = plot_title) +
  #setting animation parameters
  transition_time(frameId)  +
  ease_aes('linear') + 
  NULL 

#saving animation to display in markdown cell below:
anim_save('punt1.gif',
          animate(anim, width = 720, height = 440,
                  fps = 10, nframe = nFrames))
