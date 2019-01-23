# Join all relevant data for one game
gameData <- function(gameId) {
  trackingData <- read.csv(paste("C:\\Users\\zachb\\Documents\\Big Data Bowl\\Data\\tracking_gameId_", gameId,".csv", sep = ""), header = T)
  data <- games %>% inner_join(plays, by = 'gameId') %>% inner_join(trackingData, by = c('gameId', 'playId')) %>% left_join(players, by = 'nflId')
  data$teamAbbr <- ifelse(data$team == 'home', levels(data$homeTeamAbbr)[data$homeTeamAbbr], 
                          ifelse(data$team == 'away', levels(data$visitorTeamAbbr)[data$visitorTeamAbbr], NA))
  return(data)
}


# Animate full play (all frames)
animation <- function(data, play) {
  
  playData <- data %>% filter(playId == play)
  
  ## General field boundaries
  xmin <- 0
  xmax <- 160/3
  hash.left <- 23.36667
  hash.right <- 29.96667
  numbers.left <- 11.683
  numbers.right <- 41.65
  hash.width <- 3.3
  
  
  ## Specific boundaries for a given play
  ymin <- 0 #max(round(min(playData$x, na.rm = TRUE) - 10, -1), 0)
  ymax <- 120 #min(round(max(playData$x, na.rm = TRUE) + 10, -1), 120)
  df.hash <- expand.grid(x = c(0, hash.left, hash.right, xmax), y = (10:110))
  df.hash <- df.hash %>% filter(!(floor(y %% 5) == 0))
  df.hash <- df.hash %>% filter(y < ymax, y > ymin)
  
  animate.play <- ggplot() +
    geom_point(data = playData, aes(x = (xmax-y), y = x, 
                                    colour = team, group = nflId, pch = team, size = team)) + 
    geom_text(data = playData, aes(x = (xmax-y), y = x, label = jerseyNumber), colour = "white", 
              vjust = 0.36, size = 3.5) + 
    scale_size_manual(values = c(6, 4, 6), guide = FALSE) + 
    scale_shape_manual(values = c(19, 16, 19), guide = FALSE) +
    scale_colour_manual(values = c("#e31837", "#654321", "#002244"), guide = FALSE) + 
    annotate("text", x = df.hash$x[df.hash$x < 55/2], 
             y = df.hash$y[df.hash$x < 55/2], label = "_", hjust = 0, vjust = -0.2) + 
    annotate("text", x = df.hash$x[df.hash$x > 55/2], 
             y = df.hash$y[df.hash$x > 55/2], label = "_", hjust = 1, vjust = -0.2) + 
    annotate("segment", x = xmin, 
             y = seq(max(10, ymin), min(ymax, 110), by = 5), 
             xend =  xmax, 
             yend = seq(max(10, ymin), min(ymax, 110), by = 5)) + 
    annotate("text", x = rep(numbers.left, 11), y = seq(10, 110, by = 10), 
             label = c("G   ", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "   G"), 
             angle = 270, size = 4, vjust = 0.5) + 
    annotate("text", x = rep(numbers.right, 11), y = seq(10, 110, by = 10), 
             label = c("   G", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "G   "), 
             angle = 90, size = 4, vjust = 0.5) + 
    annotate("segment", x = c(xmin, xmin, xmax, xmax), 
             y = c(ymin, ymax, ymax, ymin), 
             xend = c(xmin, xmax, xmax, xmin), 
             yend = c(ymax, ymax, ymin, ymin), colour = "black") + 
    #labs(title = 'Title: {frame_time}') +
    ggtitle('Frame: {frame_time}') +
    #ylim(ymin, ymax) + 
    coord_fixed() +  
    #theme_nothing() + 
    theme(axis.line = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), 
          axis.ticks = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank()) +
    transition_time(frame.id)  +
    ease_aes('linear') + 
    NULL
  
  ## Ensure timing of play matches 10 frames-per-second
  play.length.ex <- length(unique(playData$frame.id))
  animate(animate.play, fps = 10, nframe = play.length.ex)
  
}


# Show snapshot within play (single frame)
snapshot <- function(data, play, frame) {
  
  playData <- data %>% filter((playId == play) & (frame.id == frame))
  
  ## General field boundaries
  xmin <- 0
  xmax <- 160/3
  hash.left <- 23.36667
  hash.right <- 29.96667
  numbers.left <- 11.683
  numbers.right <- 41.65
  hash.width <- 3.3
  
  
  ## Specific boundaries for a given play
  ymin <- 0 #max(round(min(playData$x, na.rm = TRUE) - 10, -1), 0)
  ymax <- 120 #min(round(max(playData$x, na.rm = TRUE) + 10, -1), 120)
  df.hash <- expand.grid(x = c(0, hash.left, hash.right, xmax), y = (10:110))
  df.hash <- df.hash %>% filter(!(floor(y %% 5) == 0))
  df.hash <- df.hash %>% filter(y < ymax, y > ymin)
  
  show.snapshot <- ggplot() +
    geom_point(data = playData, aes(x = (xmax-y), y = x, 
                                    colour = team, group = nflId, pch = team, size = team)) + 
    geom_text(data = playData, aes(x = (xmax-y), y = x, label = jerseyNumber), colour = "white", 
              vjust = 0.36, size = 3.5) + 
    scale_size_manual(values = c(6, 4, 6), guide = FALSE) + 
    scale_shape_manual(values = c(19, 16, 19), guide = FALSE) +
    scale_colour_manual(values = c("#e31837", "#654321", "#002244"), guide = FALSE) + 
    annotate("text", x = df.hash$x[df.hash$x < 55/2], 
             y = df.hash$y[df.hash$x < 55/2], label = "_", hjust = 0, vjust = -0.2) + 
    annotate("text", x = df.hash$x[df.hash$x > 55/2], 
             y = df.hash$y[df.hash$x > 55/2], label = "_", hjust = 1, vjust = -0.2) + 
    annotate("segment", x = xmin, 
             y = seq(max(10, ymin), min(ymax, 110), by = 5), 
             xend =  xmax, 
             yend = seq(max(10, ymin), min(ymax, 110), by = 5)) + 
    annotate("text", x = rep(numbers.left, 11), y = seq(10, 110, by = 10), 
             label = c("G   ", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "   G"), 
             angle = 270, size = 4, vjust = 0.5) + 
    annotate("text", x = rep(numbers.right, 11), y = seq(10, 110, by = 10), 
             label = c("   G", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "G   "), 
             angle = 90, size = 4, vjust = 0.5) + 
    annotate("segment", x = c(xmin, xmin, xmax, xmax), 
             y = c(ymin, ymax, ymax, ymin), 
             xend = c(xmin, xmax, xmax, xmin), 
             yend = c(ymax, ymax, ymin, ymin), colour = "black") + 
    #labs(title = 'Title: {frame_time}') +
    ggtitle(paste('Play:', play, '\nFrame:', frame)) +
    ylim(ymin, ymax) + 
    coord_fixed() +  
    #theme_nothing() + 
    theme(axis.line = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), 
          axis.ticks = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank())
  #transition_time(frame.id)  +
  #ease_aes('linear') + 
  NULL
  
  ## Ensure timing of play matches 10 frames-per-second
  #play.length.ex <- length(unique(playData$frame.id))
  #animate(show.snapshot, fps = 10, nframe = play.length.ex)
  show.snapshot
}


# Find nearest defender for given game, play, player, frame
nearestDefender <- function(df, game, play, frame = 1, player) {
  
  loc_player <- df %>% filter((gameId == game) & (playId == play) & (frame.id == frame) & (nflId == player)) %>%
    select(gameId, playId, frame.id, nflId, team, x, y, LastName)
  loc_comp <- df %>% filter((gameId == game) & (playId == play) & (frame.id == frame)) %>%
    filter((team != loc_player$team) & (team != 'ball')) %>%
    select(gameId, playId, frame.id, nflId, team, x, y, LastName)
  
  dist <- loc_player %>% inner_join(loc_comp, by = c('gameId', 'playId', 'frame.id')) %>%
    select(gameId, playId, frame.id, nflId_p = nflId.x, x_p = x.x, y_p = y.x, LastName_p = LastName.x,
           nflId_c = nflId.y, x_c = x.y, y_c = y.y, LastName_c = LastName.y)
  dist$dist <- sqrt((dist$x_p - dist$x_c)^2 + (dist$y_p - dist$y_c)^2)
  defId <- dist$nflId_c[dist$dist == min(dist$dist)]
  d <- min(dist$dist)
  
  return(list(defId = defId, dist = d))
}
#x <- nearestDefender(data, 2017090700, 2756, 1, 2540258) #2540258 = Kelce, 2552652 = Conley


# Calculate yards downfield (from line of scrimmage)
ydsDownfield <- function(x, yardlineSide, yardlineNumber, possessionTeam, teamDir) {
  
  field <- ifelse(yardlineSide == possessionTeam, 'long', 'short')
  yardline.x <- ifelse((teamDir==90 & field=='long') | (teamDir==270 & field=='short'),
                       yardlineNumber+10, 110-yardlineNumber)
  diff <- ifelse(teamDir == 270, yardline.x - x, x - yardline.x)
  
  return(diff)
}
#ydsDownfield(50, 'KC', 20, 'KC', 270) #40
#ydsDownfield(50, 'KC', 20, 'KC', 90)  #20
#ydsDownfield(100, 'KC', 20, 'NE', 90) #10
#ydsDownfield(100, 'KC', 45, 'NE', 90) #35


# Find defender's quadrant, relative to receiver
#     1 _|_ 4  (from QB's perspective)
#     2  |  3
quadrant <- function(x.rec, y.rec, x.def, y.def, teamDir) {
  quad <- ifelse(teamDir == 90,
                 ifelse(x.def >= x.rec, 
                        ifelse(y.def >= y.rec, 1, 4), 
                        ifelse(y.def >= y.rec, 2, 3)),
                 ifelse(x.def >= x.rec, # represents teamDir == 270
                        ifelse(y.def >= y.rec, 3, 2), 
                        ifelse(y.def >= y.rec, 4, 1)))
  return(quad)
}


# Calculate shortest distance between two angles (max 180)
## ZB: change function to work on vectors?  This would prevent loop.
angle <- function(d1, d2) {
  if (d1 == 90) {
    if (d2 >= 270) {
      a <- 360 - abs(d1 - d2)
    } else {
      a <- abs(d1 - d2)
    }
    if (between(d2, 90, 270)) {
      turnDir <- 'right'
    } else {
      turnDir <- 'left'
    }
  } else if (d1 == 270) {
    if (d2 <= 90) {
      a <- 360 - abs(d1 - d2)
    } else {
      a <- abs(d1 - d2)
    }
    if (between(d2, 90, 270)) {
      turnDir <- 'left'
    } else {
      turnDir <- 'right'
    }
  }
  result <- data.frame(a = a, turnDir = turnDir, stringsAsFactors = F)
  return(result)
}
#angle(90, 100)

