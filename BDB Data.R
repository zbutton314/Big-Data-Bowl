# Score Weights
separationWeight <- 1     # number of points per yard of separation
positionWeight <- 2       # bonus/penalty points for defender's position relative to receiver
depthWeight <- 10         # yards per point awarded
depthRange <- c(-.5, 2.5) # min/max for depthScore
firstdownWeight <- 1      # bonus points for route passing first down marker

# Scores for defender position quadrants (relative to receiver)
quadScore <- as.data.frame(rbind(c('Flat', 'right', 1, -1),c('Flat', 'right', 2, -1),c('Flat', 'right', 3, 1),c('Flat', 'right', 4, 0),
                                 c('Flat', 'left', 1, 0),c('Flat', 'left', 2, 1),c('Flat', 'left', 3, -1),c('Flat', 'left', 4, -1),
                                 c('Slant', 'right', 1, 0),c('Slant', 'right', 2, -1),c('Slant', 'right', 3, 1),c('Slant', 'right', 4, 1),
                                 c('Slant', 'left', 1, 1),c('Slant', 'left', 2, 1),c('Slant', 'left', 3, -1),c('Slant', 'left', 4, 0),
                                 c('Curl_out', 'right', 1, 1),c('Curl_out', 'right', 2, -1),c('Curl_out', 'right', 3, -1),c('Curl_out', 'right', 4, 1),
                                 c('Curl_out', 'left', 1, 1),c('Curl_out', 'left', 2, -1),c('Curl_out', 'left', 3, -1),c('Curl_out', 'left', 4, 1),
                                 c('Curl_in', 'right', 1, 0),c('Curl_in', 'right', 2, -1),c('Curl_in', 'right', 3, 0),c('Curl_in', 'right', 4, 1),
                                 c('Curl_in', 'left', 1, 1),c('Curl_in', 'left', 2, 0),c('Curl_in', 'left', 3, -1),c('Curl_in', 'left', 4, 0),
                                 c('Out', 'right', 1, 1),c('Out', 'right', 2, -1),c('Out', 'right', 3, -1),c('Out', 'right', 4, 0),
                                 c('Out', 'left', 1, 0),c('Out', 'left', 2, -1),c('Out', 'left', 3, -1),c('Out', 'left', 4, 1),
                                 c('Dig', 'right', 1, 0),c('Dig', 'right', 2, -1),c('Dig', 'right', 3, 1),c('Dig', 'right', 4, 1),
                                 c('Dig', 'left', 1, 1),c('Dig', 'left', 2, 1),c('Dig', 'left', 3, -1),c('Dig', 'left', 4, 0),
                                 c('Corner', 'right', 1, 1),c('Corner', 'right', 2, 1),c('Corner', 'right', 3, 1),c('Corner', 'right', 4, -1),
                                 c('Corner', 'left', 1, -1),c('Corner', 'left', 2, 1),c('Corner', 'left', 3, 1),c('Corner', 'left', 4, 1),
                                 c('Post', 'right', 1, 0),c('Post', 'right', 2, -1),c('Post', 'right', 3, 1),c('Post', 'right', 4, 1),
                                 c('Post', 'left', 1, 1),c('Post', 'left', 2, 1),c('Post', 'left', 3, -1),c('Post', 'left', 4, 0),
                                 c('Fly', 'right', 1, -1),c('Fly', 'right', 2, 0),c('Fly', 'right', 3, 1),c('Fly', 'right', 4, 0),
                                 c('Fly', 'left', 1, 0),c('Fly', 'left', 2, 1),c('Fly', 'left', 3, 0),c('Fly', 'left', 4, -1)),
                           stringsAsFactors = F)
colnames(quadScore) <- c('routeType', 'sideOfField', 'quad', 'positionScore')
quadScore$positionScore <- as.numeric(quadScore$positionScore)