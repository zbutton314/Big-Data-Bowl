library(tidyverse)
library(gganimate)
library(cowplot)
library(stringr)
library(sqldf)

source('C:\\Users\\zachb\\Documents\\Big Data Bowl\\BDB Functions.r')
source('C:\\Users\\zachb\\Documents\\Big Data Bowl\\BDB Data.r')

########################################################################################
# Test receiver scores
game <- 2017090700

play <- 68
rm(list = c('dataPass'))
dataPass <- gameData(gameId = game) %>% filter(isPenalty + isSTPlay == 0) %>% filter(PassResult %in% c('C','I','IN'))
animation(dataPass, play)
View(routes[which(routes$playId==play & routes$gameId==game),])

frame <- 46
snapshot(dataPass, play, frame)

########################################################################################

########################################################################################
# Test route labels
playIds <- routes %>% ungroup() %>% distinct(gameId, playId)
s <- sample(dim(playIds)[1], 20)
test <- routes %>% 
  inner_join(data.frame(gameId = playIds$gameId[s], playId = playIds$playId[s]), by = c('gameId', 'playId'))

routeLabels <- read.csv('C:\\Users\\zachb\\Documents\\Big Data Bowl\\Route Identification Testing.csv', header = T)
routeLabels <- routeLabels %>% inner_join(routes, by = c('gameId','playId','jerseyNumber' = 'jerseyNumber.rec')) %>%
  select(gameId, playId, jerseyNumber, PositionAbbr.rec, routeTypeActual, routeTypePred = routeType)
write.csv(routeLabels, 'C:\\Users\\zachb\\Documents\\Big Data Bowl\\Route Identification Testing.csv')
########################################################################################



##########################################
## Create Full Routes Table (All Games) ##
##########################################

games <- read.csv("C:\\Users\\zachb\\Documents\\Big Data Bowl\\Data\\games.csv", header = T)
players <- read.csv("C:\\Users\\zachb\\Documents\\Big Data Bowl\\Data\\players.csv", header = T)
plays <- read.csv("C:\\Users\\zachb\\Documents\\Big Data Bowl\\Data\\plays.csv", header = T)

#Split out personnel counts (only 3 primary positions on each side)
plays$pers.off.RB = as.numeric(str_extract(str_extract(plays$personnel.offense, '[0-9] RB'), '[0-9]'))
plays$pers.off.WR = as.numeric(str_extract(str_extract(plays$personnel.offense, '[0-9] WR'), '[0-9]'))
plays$pers.off.TE = as.numeric(str_extract(str_extract(plays$personnel.offense, '[0-9] TE'), '[0-9]'))
plays$pers.def.DL = as.numeric(str_extract(str_extract(plays$personnel.defense, '[0-9] DL'), '[0-9]'))
plays$pers.def.LB = as.numeric(str_extract(str_extract(plays$personnel.defense, '[0-9] LB'), '[0-9]'))
plays$pers.def.DB = as.numeric(str_extract(str_extract(plays$personnel.defense, '[0-9] DB'), '[0-9]'))

# Set single game value
game <- NA #2017091000

ptm <- proc.time()
oldw <- getOption("warn")
options(warn = -1)

rm(routes)
l = c('dataPass', 'eventFrames', 'rec_dir', 'rec_nd', 'rec_release', 'rec_score', 'rec_side', 'rec_snap', 'route_angles', 'routes_single', 
      'side_counts', 'teams', 'd1', 'd2', 'i')

if (is.na(game) == F) {
  
  source('C:\\Users\\zachb\\Documents\\Big Data Bowl\\BDB Routes.r')
  routes <- routes_single
  
} else {
  
  for (game in games$gameId) {
    
    cat(sprintf('Adding routes data for gameId %d.\n', game))
    source('C:\\Users\\zachb\\Documents\\Big Data Bowl\\BDB Routes.r')
    
    if (exists('routes')) {
      routes <- bind_rows(routes, routes_single)
    } else {
      routes <- routes_single
    }
    
    cat('     --Done.\n')
    
  }
  
}

rm(list = l)
options(warn = oldw)
proc.time() - ptm


######################
## Aggregate Scores ##
######################

# Route Types
score_RouteType <- routes %>% group_by(routeType) %>% 
  summarize(n = n(), separationScore_mean = round(mean(separationScore),1), separationScore_median = median(separationScore), 
            positionScore_mean = round(mean(positionScore),1), positionScore_median = median(positionScore), 
            depthScore_mean = round(mean(depthScore),1), depthScore_median = median(depthScore), firstdownScore_mean = round(mean(firstdownScore),1), 
            firstdownScore_median = median(firstdownScore), score_mean = round(mean(receiverScore),1), score_median = median(receiverScore))

# Players/Route Types
score_PlayerRouteType <- routes %>% group_by(routeType, nflId) %>% 
  summarize(n = n(), score_mean = mean(receiverScore), score_median = median(receiverScore)) %>% inner_join(players, by = 'nflId') %>% 
  select(routeType, nflId, FirstName, LastName, PositionAbbr, Height, Weight, n, score_mean, score_median)

# Players
score_Player <- routes %>% group_by(nflId) %>%
  summarize(n = n(), separationScore_mean = mean(separationScore), separationScore_median = median(separationScore), 
            positionScore_mean = mean(positionScore), positionScore_median = median(positionScore), depthScore_mean = mean(depthScore), 
            depthScore_median = median(depthScore), firstdownScore_mean = mean(firstdownScore), firstdownScore_median = median(firstdownScore), 
            score_mean = mean(receiverScore), score_median = median(receiverScore)) %>%
  inner_join(players, by = 'nflId') %>%
  select(nflId, FirstName, LastName, PositionAbbr, Height, Weight, n, separationScore_mean, separationScore_median, positionScore_mean, 
         positionScore_median, depthScore_mean, depthScore_median, firstdownScore_mean, firstdownScore_median, score_mean, score_median)

# Plays/Route Combos
score_PlayRouteCombo <- routes %>% arrange(gameId, playId, sideOfField, routeType) %>% group_by(gameId, playId, sideOfField) %>% 
  mutate(routeCombo = paste0(routeType, sep = ', ', collapse = '')) %>%
  summarize(score_mean = mean(receiverScore), score_max = max(receiverScore), score_min = min(receiverScore), sideCount = max(sideCount),
            routeCombo = max(substr(routeCombo, 1, nchar(routeCombo)-2)))

# Route Combos
score_RouteCombo <- score_PlayRouteCombo %>% group_by(routeCombo) %>%
  summarize(sideCount = max(sideCount), n = n(), score_mean = mean(score_mean), score_max = mean(score_max), score_min = mean(score_min)) %>%
  arrange(sideCount, desc(score_max))

# Route Combos/Defenses
def.schemes <- routes %>% ungroup() %>% distinct(pers.def.DL, pers.def.LB, pers.def.DB) %>% 
  mutate(pers.def.DL = as.numeric(pers.def.DL), pers.def.LB = as.numeric(pers.def.LB), pers.def.DB = as.numeric(pers.def.DB)) %>%
  mutate(pers.def.tot = pers.def.DL+pers.def.LB+pers.def.DB) %>% arrange(pers.def.DL, pers.def.LB, pers.def.DB)

score_RouteComboDef <- score_PlayRouteCombo %>% inner_join(plays, by = c('gameId', 'playId')) %>%
  select(gameId, playId, sideOfField, pers.def.DL, pers.def.LB, pers.def.DB, score_mean, score_max, score_min, 
         sideCount, routeCombo) %>%
  #mutate(pers.def.DL = as.numeric(pers.def.DL), pers.def.LB = as.numeric(pers.def.LB), pers.def.DB = as.numeric(pers.def.DB)) %>%
  group_by(routeCombo, pers.def.DL, pers.def.LB, pers.def.DB) %>%
  summarize(sideCount = max(sideCount), n = n(), score_mean = mean(score_mean), score_max = mean(score_max), score_min = mean(score_min)) %>%
  arrange(sideCount, pers.def.DL, pers.def.LB, pers.def.LB, desc(score_max)) %>%
  inner_join(def.schemes, by = c('pers.def.DL','pers.def.LB','pers.def.DB')) %>% filter(pers.def.tot == 11)
score_RouteComboDef$pers.def <- with(score_RouteComboDef, paste(pers.def.DL, pers.def.LB, pers.def.DB, sep='.'))
score_RouteComboDef$scheme <- with(score_RouteComboDef, ifelse(pers.def == '4.3.4', '4-3',
                                                               ifelse(pers.def == '3.4.4', '3-4', 
                                                                      ifelse(pers.def == '5.2.4', '5-2', 
                                                                             ifelse(pers.def == '4.4.3', '4-4', 
                                                                                    ifelse(pers.def == '4.2.5', 'Nickel', 
                                                                                           ifelse(pers.def == '4.1.6', 'Dime', NA)))))))




# Write results for easier handling
write.csv(score_RouteCombo, 'C:\\Users\\zachb\\Documents\\Big Data Bowl\\RouteComboResults.csv', col.names = T)
write.csv(score_RouteComboDef, 'C:\\Users\\zachb\\Documents\\Big Data Bowl\\RouteComboDefResults.csv', col.names = T)










