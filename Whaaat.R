#############################################################

# Data prep

library("rio")
x <- import("https://docs.google.com/spreadsheets/d/1h7XhLd2Byp4OcXSdtxHly9iS7RA673RJ/export?format=csv&gid=1083477596")
x$commentCount <- as.integer(x$commentCount)
x$viewsCount <- as.numeric(x$viewsCount)
x$acousticness <- as.numeric(sub(",", ".", x$acousticness, fixed = TRUE))
x$danceability <- as.numeric(sub(",", ".", x$danceability, fixed = TRUE))
x$energy <- as.numeric(sub(",", ".", x$energy, fixed = TRUE))
x$instrumentalness <- as.numeric(sub(",", ".", x$instrumentalness, fixed = TRUE))
x$liveness <- as.numeric(sub(",", ".", x$liveness, fixed = TRUE))
x$loudness <- as.numeric(sub(",", ".", x$loudness, fixed = TRUE))
x$speechiness <- as.numeric(sub(",", ".", x$speechiness, fixed = TRUE))
x$tempo <- as.numeric(sub(",", ".", x$tempo, fixed = TRUE))
x$valence <- as.numeric(sub(",", ".", x$valence, fixed = TRUE))
x$key <- as.factor(x$key)
x$time_signature <- as.factor(x$time_signature)
min_max_normalize <- function(x)
{
  return( (1000-10)*((x- min(x)) /(max(x)-min(x))) + 10)
}
x$acousticness <- min_max_normalize(x$acousticness)
x$danceability <- min_max_normalize(x$danceability)
x$energy <- min_max_normalize(x$energy)
x$instrumentalness <- min_max_normalize(x$instrumentalness)
x$liveness <- min_max_normalize(x$liveness)
x$loudness <- min_max_normalize(x$loudness)
x$speechiness <- min_max_normalize(x$speechiness)
x$tempo <- min_max_normalize(x$tempo)
x$valence <- min_max_normalize(x$valence)
x$Genre <- as.factor(x$Genre)
x$key <- as.factor(x$key)
x$mode <- as.factor(x$mode)
x$Charts <- as.factor(x$Charts)

library("dplyr")

x <- dplyr::select(x, -one_of('Streams'))
x <- x[complete.cases(x), ]

var_list <- c('Artist_Albums_Number', 'Artist_Albums_Tracks_Number', 'Artist_Appearances_Number', 'Artist_Appearances_Tracks_Number', 'Artist_Compilations_Number', 'Artist_Compilations_Tracks_Number', 'Artist_Follower', 'Artist_Popularity', 'Artist_Singles_Number', 'Artist_Singles_Tracks_Number', 'Track_Duration_ms', 'Track_Popularity', 'acousticness', 'danceability', 'energy', 'instrumentalness', 'liveness', 'loudness', 'speechiness', 'tempo', 'valence', 'commentCount', 'dislikeCount', 'likeCount', 'viewsCount')

strictly_positive_variables <- c('Artist_Follower', 'Artist_Popularity', 'Artist_Singles_Number', 'Artist_Singles_Tracks_Number', 'Track_Duration_ms', 'Track_Popularity', 'acousticness', 'danceability', 'energy', 'instrumentalness', 'liveness', 'loudness', 'speechiness', 'tempo', 'valence', 'likeCount', 'viewsCount')

library("psych")
library("car")

ksD <- function (p, x) {
  y <- bcPower(x, p)
  ks.test(y, "pnorm", mean=mean(y), sd=sd(y))$statistic
}

oldw <- getOption("warn")
options(warn = -1)

min_values <- c()

for (column_index in 1:length(strictly_positive_variables)){
  
  column_name <- strictly_positive_variables[column_index]
  
  x_sub <- x[[paste(column_name)]]
  
  result <- optimize(ksD, c(-5,5), x=x_sub)
  
  min_values[column_index] <- result$minimum
  
  message(paste(column_index, ', minimum value is: ', result$minimum))
  
}

options(warn = oldw)

column_index <- 1
column_name <- strictly_positive_variables[column_index]
x_sub <- x[[paste(column_name)]]
Artist_Follower_trans <- bcPower(x_sub, min_values[column_index])

column_index <- 2
column_name <- strictly_positive_variables[column_index]
x_sub <- x[[paste(column_name)]]
Artist_Popularity_trans <- bcPower(x_sub, min_values[column_index])

column_index <- 3
column_name <- strictly_positive_variables[column_index]
x_sub <- x[[paste(column_name)]]
Artist_Singles_Number_trans <- bcPower(x_sub, min_values[column_index])

column_index <- 4
column_name <- strictly_positive_variables[column_index]
x_sub <- x[[paste(column_name)]]
Artist_Singles_Tracks_Number_trans <- bcPower(x_sub, min_values[column_index])

column_index <- 5
column_name <- strictly_positive_variables[column_index]
x_sub <- x[[paste(column_name)]]
Track_Duration_ms_trans <- bcPower(x_sub, min_values[column_index])

column_index <- 6
column_name <- strictly_positive_variables[column_index]
x_sub <- x[[paste(column_name)]]
Track_Popularity_trans <- bcPower(x_sub, min_values[column_index])

column_index <- 7
column_name <- strictly_positive_variables[column_index]
x_sub <- x[[paste(column_name)]]
acousticness_trans <- bcPower(x_sub, min_values[column_index])

column_index <- 8
column_name <- strictly_positive_variables[column_index]
x_sub <- x[[paste(column_name)]]
danceability_trans <- bcPower(x_sub, min_values[column_index])

column_index <- 9
column_name <- strictly_positive_variables[column_index]
x_sub <- x[[paste(column_name)]]
energy_trans <- bcPower(x_sub, min_values[column_index])

column_index <- 10
column_name <- strictly_positive_variables[column_index]
x_sub <- x[[paste(column_name)]]
instrumentalness_trans <- bcPower(x_sub, min_values[column_index])

column_index <- 11
column_name <- strictly_positive_variables[column_index]
x_sub <- x[[paste(column_name)]]
liveness_trans <- bcPower(x_sub, min_values[column_index])

column_index <- 12
column_name <- strictly_positive_variables[column_index]
x_sub <- x[[paste(column_name)]]
loudness_trans <- bcPower(x_sub, min_values[column_index])

column_index <- 13
column_name <- strictly_positive_variables[column_index]
x_sub <- x[[paste(column_name)]]
speechiness_trans <- bcPower(x_sub, min_values[column_index])

column_index <- 14
column_name <- strictly_positive_variables[column_index]
x_sub <- x[[paste(column_name)]]
tempo_trans <- bcPower(x_sub, min_values[column_index])

column_index <- 15
column_name <- strictly_positive_variables[column_index]
x_sub <- x[[paste(column_name)]]
valence_trans <- bcPower(x_sub, min_values[column_index])

column_index <- 16
column_name <- strictly_positive_variables[column_index]
x_sub <- x[[paste(column_name)]]
likeCount_trans <- bcPower(x_sub, min_values[column_index])

column_index <- 17
column_name <- strictly_positive_variables[column_index]
x_sub <- x[[paste(column_name)]]
viewsCount_trans <- bcPower(x_sub, min_values[column_index])

hist_trans_list <- list(Artist_Follower_trans, 
                        Artist_Popularity_trans,
                        Artist_Singles_Number_trans,
                        Artist_Singles_Tracks_Number_trans,
                        Track_Duration_ms_trans, 
                        Track_Popularity_trans,
                        acousticness_trans, 
                        danceability_trans, 
                        energy_trans, 
                        instrumentalness_trans,
                        liveness_trans, 
                        loudness_trans, 
                        speechiness_trans, 
                        tempo_trans, 
                        valence_trans,
                        likeCount_trans,
                        viewsCount_trans)

par(mar = rep(2, 4))
par(mfrow=c(4,5))

for (trans_index in 1:length(hist_trans_list)){
  
  column_name <- strictly_positive_variables[trans_index]
  
  selected_trans <- hist_trans_list[trans_index]
  selected_trans <- as.numeric(as.character(unlist(selected_trans[[1]])))
  
  hist(selected_trans, col = "gray", probability = TRUE, main = column_name, xlab = "")
  points(seq(min(selected_trans), max(selected_trans), length.out = 500),
         dnorm(seq(min(selected_trans), max(selected_trans), length.out = 500),
               mean(selected_trans),sd(selected_trans)), type = "l", col = "red")
  
  test_statistic <- ks.test(selected_trans, "pnorm", mean=mean(selected_trans), sd=sd(selected_trans))$statistic
  critical_value <- 1.3581 / sqrt (length(selected_trans))
  
  if (test_statistic > critical_value) {
    message(paste("Transformed ", column_name , " is not approximately normally distributed.", test_statistic, critical_value))
  } else {
    message(paste("Transformed ", column_name , " is approximately normally distributed!", test_statistic, critical_value))  
  }}


par(mar = rep(2, 4))

par(mfrow=c(4,5))

for(trans_index in 1:length(strictly_positive_variables)){
  
  
  column_name <- strictly_positive_variables[trans_index]
  
  selected_trans <- hist_trans_list[trans_index]
  selected_trans <- as.numeric(as.character(unlist(selected_trans[[1]])))
  
  df <- data.frame(selected_trans)
  names(df)[1] <- strictly_positive_variables[trans_index]
  
  boxplot(x = df[,1], main = strictly_positive_variables[trans_index], notch = FALSE)
  
}


