################################
## Import and Preprocess Data ##
################################

# data <- gameData(gameId = 2017091700)
# play <- 3954
# animation(data, play)


# Clear out any old data
rm(list = l)

# Create full data and filter to valid pass plays
data <- gameData(gameId = game)
teams <- data.frame(type = c('home', 'away'), team = c(levels(data$homeTeamAbbr)[data$homeTeamAbbr[1]], 
                                                       levels(data$visitorTeamAbbr)[data$visitorTeamAbbr[1]]))
dataPass <- data %>%
  filter(isPenalty + isSTPlay == 0) %>%
  filter(PassResult %in% c('C','I','IN'))

rm(list = c('data'))


###################################
## Label Significant Play Events ##
###################################

playEvents <- dataPass %>% distinct(playId, frame.id)
playEvents$playEvent <- NA
snap <- dataPass %>% filter(event == 'ball_snap') %>% select(playId, frame.id) %>% group_by(playId) %>%
  summarize(frame.id = max(frame.id))
release <- dataPass %>% filter(event == 'pass_forward') %>% select(playId, frame.id) %>% group_by(playId) %>%
  summarize(frame.id = max(frame.id))
playEvents <- playEvents %>% left_join(snap, by = 'playId') %>% left_join(release, by = 'playId') %>%
  rename(frame.id = frame.id.x, frame.id.snap = frame.id.y, frame.id.rel = frame.id)
for (i in 1:dim(playEvents)[1]) {
  s <- NULL
  play <- playEvents$playId[i]
  frame_snap <- playEvents$frame.id.snap[i]
  frame_snap <- ifelse(is.na(frame_snap), -15, frame_snap)
  frame_rel <- playEvents$frame.id.rel[i]
  frame_rel <- ifelse(is.na(frame_rel), -15, frame_rel)
  if (playEvents$frame.id[i] == frame_snap) {
    s <- '|ball_snap'
  }
  if (playEvents$frame.id[i] == frame_rel) {
    s <- '|pass_forward'
  }
  if (playEvents$frame.id[i] == frame_snap + 10) {
    s <- paste(s, '|snap+10', sep = '')
  }
  if (playEvents$frame.id[i] == frame_rel - 5) {
    s <- paste(s, '|release-5', sep = '')
  }
  if (playEvents$frame.id[i] == frame_rel + 10) {
    s <- paste(s, '|release+10', sep = '')
  }
  if (playEvents$frame.id[i] == 30) {
    s <- paste(s, '|30', sep = '')
  }
  if (is.null(s) == T) {
    s <- NA
  } else {
    s <- paste(s, '|', sep = '')
  }
  playEvents$playEvent[i] <- s
}
dataPass <- dataPass %>% inner_join(playEvents, by = c('playId', 'frame.id')) %>% select(-frame.id.snap, -frame.id.rel)

# inner joins require plays to have all five events: snap, snap+10, release, release-5, and release+10
eventFrames <- dataPass %>% filter(grepl('ball_snap', playEvent, fixed=T)) %>% distinct(playId, frame.id) %>% select(playId, snap = frame.id) %>%
  inner_join(dataPass %>% filter(grepl('snap+10', playEvent, fixed=T)) %>% distinct(playId, frame.id) %>% select(playId, snap10 = frame.id), by = 'playId') %>%
  inner_join(dataPass %>% filter(grepl('pass_forward', playEvent, fixed=T)) %>% distinct(playId, frame.id) %>% select(playId, release = frame.id), by = 'playId') %>%
  inner_join(dataPass %>% filter(grepl('release-5', playEvent, fixed=T)) %>% distinct(playId, frame.id) %>% select(playId, release_5 = frame.id), by = 'playId') %>%
  inner_join(dataPass %>% filter(grepl('release+10', playEvent, fixed=T)) %>% distinct(playId, frame.id) %>% select(playId, release10 = frame.id), by = 'playId')

rm(list = c('snap', 'release', 'i', 's', 'play', 'frame_snap', 'frame_rel', 'playEvents'))

#########################################################################
##### FILTERING TO PLAY EVENTS
### Ball snapped
# View(dataPass %>% filter(grepl('ball_snap', playEvent, fixed=T)))
### Ball released
# View(dataPass %>% filter(grepl('pass_forward', playEvent, fixed=T)))
### Snap + 10 frames
# View(dataPass %>% filter(grepl('snap+10', playEvent, fixed=T)))
### Release - 10 frames
# View(dataPass %>% filter(grepl('release-10', playEvent, fixed=T)))
### Release + 5 frames
# View(dataPass %>% filter(grepl('release+5', playEvent, fixed=T)))
### Frame 30
# View(dataPass %>% filter(grepl('30', playEvent, fixed=T)))
#########################################################################


#####################################
## Collect Other Route Details     ##
## (Direction, Side of Field, etc) ##
#####################################

# Receivers when ball was snapped
rec_snap <- dataPass %>% filter(grepl('ball_snap', playEvent, fixed=T)) %>% filter(PositionAbbr %in% c('WR','TE'))

# Receivers 10 frames after ball was snapped
rec_snap10 <- dataPass %>% filter(grepl('snap+10', playEvent, fixed=T)) %>% filter(PositionAbbr %in% c('WR','TE'))

# Find direction receivers are running (x = frame 30, y = ball snap)
rec_dir <- dataPass %>% filter(grepl('30', playEvent, fixed=T)) %>% inner_join(rec_snap, by = c('playId', 'nflId')) %>% 
  group_by(playId) %>% summarize(avgXSnap = mean(x.y), avgX30 = mean(x.x), avgDirSnap = mean(dir.x), avgDir30 = mean(dir.y))
rec_dir$teamDir <- ifelse(rec_dir$avgXSnap >= rec_dir$avgX30, 270, 90)

# Find side of field (left/right)
rec_side <- rec_snap %>% inner_join(rec_snap10, by = c('gameId', 'playId', 'nflId')) %>% inner_join(rec_dir, by = 'playId')
rec_side$sideOfField <- with(rec_side, 
                             ifelse(teamDir == 90,
                                    ifelse(y.x>28.67, 'left',
                                           ifelse(y.x < 24.67, 'right',
                                                  ifelse(y.y>26.67, 'left', 'right'))),
                                    ifelse(y.x>28.67, 'right',
                                           ifelse(y.x < 24.67, 'left',
                                                  ifelse(y.y>26.67, 'right', 'left')))))
rec_side <- rec_side %>% select(playId, nflId, teamDir, sideOfField)

# Find counts of receivers on each side of field for each play
side_counts <- rec_side %>% group_by(playId, sideOfField) %>% summarize(sideCount = n())

# Snapshot of moment when ball was released
rec_release <- dataPass %>% filter(grepl('pass_forward', playEvent, fixed=T)) %>% filter(PositionAbbr %in% c('WR','TE')) %>% 
  select(gameId, playId, nflId.rec = nflId, frame.id.rel = frame.id, x.rec.rel = x, y.rec.rel = y, s.rec.rel = s, dir.rec.rel = dir, displayName.rec = displayName, 
         jerseyNumber.rec = jerseyNumber, yardlineSide, yardlineNumber, possessionTeam)


########################
## Define Route Types ##
########################

route_detail <- dataPass %>% filter(PositionAbbr %in% c('WR','TE')) %>% inner_join(eventFrames, by = 'playId')
route_angles <- route_detail %>% distinct(playId, nflId) %>% 
  inner_join(route_detail %>% filter(frame.id >= snap & frame.id < release_5) %>% group_by(playId, nflId) %>% summarize(pos_a = median(dir)), 
             by = c('playId', 'nflId')) %>%
  inner_join(route_detail %>% filter(frame.id >= release_5 & frame.id < release10) %>% group_by(playId, nflId) %>% summarize(pos_b = median(dir)), 
             by = c('playId', 'nflId')) %>%
  inner_join(rec_side, by = c('playId', 'nflId')) %>%
  select(playId, nflId, pos_a, pos_b, teamDir, sideOfField)

# loop function to find angle difference
for (i in 1:dim(route_angles)[1]){
  d1 <-  route_angles$teamDir[i]
  d2 <-  route_angles$pos_b[i]
  route_angles$angle[i] <- angle(d1, d2)$a  
  route_angles$turnDir[i] <- angle(d1, d2)$turnDir 
}      

# determine if route goes towards sideline or to the inside
route_angles$sideline_inside <- with(route_angles, ifelse(sideOfField == turnDir, 'sideline', 'inside')) 

# for slant/post and flat identification
route_depth <- route_angles %>% inner_join(rec_release, by = c('playId', 'nflId' = 'nflId.rec'))
route_depth$depth <- with(route_depth, ydsDownfield(x.rec.rel, yardlineSide, yardlineNumber, possessionTeam, teamDir))
route_angles <- route_angles %>% inner_join(route_depth %>% select(playId, nflId, depth), by = c('playId', 'nflId'))

# identify route type
route_angles$routeType <- with(route_angles, ifelse(depth < 0 | (depth < 3 & sideline_inside == 'sideline'), 'Flat', ifelse(
  angle >=0 & angle < 17.5, 'Fly', ifelse(
    sideline_inside == 'sideline' & angle < 67.5, 'Corner', ifelse( 
      sideline_inside == 'inside' & angle < 67.5, ifelse(depth >= 10, 'Post', 'Slant'), ifelse(
        sideline_inside == 'sideline' & angle < 112.5, 'Out', ifelse(
          sideline_inside == 'inside' & angle < 112.5, 'Dig', ifelse(
            sideline_inside == 'sideline' & angle <= 180, 'Curl_out', ifelse(
              sideline_inside == 'inside' & angle <= 180, 'Curl_in', NA)))))))))

rm(list = c('route_detail', 'route_depth'))


#######################################
## Calculate Receiver Success Metric ##
#######################################

# Reduced table of receiver info
dataPass_rec <- dataPass %>% filter(PositionAbbr %in% c('WR','TE')) %>%
  select(gameId, playId, nflId, frame.id, homeTeamAbbr, visitorTeamAbbr, yardlineSide, yardlineNumber,
         possessionTeam, yardsToGo, PositionAbbr, time, x, y, s, dir, event, displayName, jerseyNumber, team, teamAbbr)

# Reduced table of defender info
dataPass_def <- dataPass %>% filter(teamAbbr != possessionTeam) %>%
  select(gameId, playId, nflId, frame.id, homeTeamAbbr, visitorTeamAbbr, yardlineSide, yardlineNumber,
         possessionTeam, yardsToGo, PositionAbbr, time, x, y, s, dir, event, displayName, jerseyNumber, team, teamAbbr)

# Distance matrix between rec and def
# Requires both (x,y) pairs to exist, only considers frames snap+10 to release
rec_def_dist <- dataPass_rec %>% inner_join(dataPass_def, by = c('gameId', 'playId', 'frame.id')) %>%
  filter(is.na(x.x+x.y+y.x+y.y) == F) %>%
  inner_join(eventFrames, by = 'playId') %>% filter((frame.id >= snap10) & (frame.id <= release)) %>%
  inner_join(teams %>% select(yardlineSideType = type, team), by = c('yardlineSide.x' = 'team')) %>%
  inner_join(rec_dir %>% select(playId, teamDir), by = 'playId')
rec_def_dist$dist <- with(rec_def_dist, sqrt((x.x - x.y)^2 + (y.x - y.y)^2))

# Reduce dist matrix to nearest defender to each receiver per frame
rec_nd <- rec_def_dist %>% group_by(playId, nflId.x, frame.id) %>% summarize(dist = min(dist)) %>%
  inner_join(rec_def_dist %>% select(-teamDir), by = c('playId', 'nflId.x', 'frame.id', 'dist')) %>% 
  inner_join(route_angles, by = c('playId', 'nflId.x' = 'nflId'))

# Calculate other useful metrics
rec_nd$yardsDownfield <- with(rec_nd, ydsDownfield(x.x, yardlineSide.x, yardlineNumber.x, possessionTeam.x, teamDir))
rec_nd$fieldLength <- with(rec_nd, ifelse(teamDir==270, ydsDownfield(10, yardlineSide.x, yardlineNumber.x, possessionTeam.x, teamDir),
                                  ydsDownfield(110, yardlineSide.x, yardlineNumber.x, possessionTeam.x, teamDir)))
rec_nd$percentFieldLength <- rec_nd$yardsDownfield/rec_nd$fieldLength
rec_nd$quad <- as.character(with(rec_nd, quadrant(x.rec = x.x, y.rec = y.x, x.def = x.y, y.def = y.y, teamDir = teamDir)))

# Give scores based on route attributes
rec_nd$separationScore <- round(rec_nd$dist, 1) * separationWeight
rec_nd <- rec_nd %>% inner_join(quadScore, by = c('routeType', 'sideOfField', 'quad')) %>% mutate(positionScore = positionScore * positionWeight)
rec_nd$depthScore <- pmin(pmax(rec_nd$yardsDownfield / depthWeight, depthRange[1]), depthRange[2])
rec_nd$firstdownScore <- ifelse(rec_nd$yardsDownfield >= rec_nd$yardsToGo.x, firstdownWeight, 0)

# Calcuate receiver score metric
rec_nd$receiverScore <- with(rec_nd, separationScore + positionScore + depthScore + firstdownScore)

rm(list = c('dataPass_rec', 'dataPass_def', 'rec_def_dist'))


#############################
## Create Final Data Frame ##
#############################

# Find each route's max score across all frames
rec_score <- rec_nd %>% group_by(playId, nflId.x) %>% top_n(1, receiverScore) %>% top_n(1, frame.id)

# Create route-level data frame
routes_single <- rec_score %>% 
  inner_join(plays %>% select(gameId, playId, yardsToGo, pers.off.RB, pers.off.WR, pers.off.TE, pers.def.DL, pers.def.LB, pers.def.DB, defendersInTheBox, 
                              numberOfPassRushers), by = c('playId', 'gameId')) %>%
  inner_join(side_counts, by = c('playId', 'sideOfField')) %>%
  select(gameId, playId, nflId = nflId.x, frame.id, possessionTeam = possessionTeam.x, yardlineSide = yardlineSide.x, yardlineNumber = yardlineNumber.x, yardsToGo, 
         pers.off.RB, pers.off.WR, pers.off.TE, pers.def.DL, pers.def.LB, pers.def.DB, defendersInTheBox, numberOfPassRushers, frame.snap = snap, 
         frame.snap10 = snap10, frame.release = release, frame.release_5 = release_5, frame.release10 = release10, displayName.rec = displayName.x, jerseyNumber.rec = jerseyNumber.x, 
         PositionAbbr.rec = PositionAbbr.x, team.rec = team.x, teamAbbr.rec = teamAbbr.x, x.rec = x.x, y.rec = y.x, s.rec = s.x, dir.rec = dir.x, 
         nflId.def = nflId.y, displayName.def = displayName.y, jerseyNumber.def = jerseyNumber.y, PositionAbbr.def = PositionAbbr.y, team.def = team.y, 
         teamAbbr.def = teamAbbr.y, x.def = x.y, y.def = y.y, s.def = s.y, dir.def = dir.y, yardlineSideType, pos_a, pos_b, teamDir, sideOfField, sideCount, 
         turnAngle = angle, turnDir, sideline_inside, routeType, yardsDownfield, fieldLength, percentFieldLength, separation = dist, positionQuad = quad, 
         separationScore, positionScore, depthScore, firstdownScore, receiverScore)






