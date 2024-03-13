##### Import the data about TOP200 songs on each week for a year #####

############ Load packages ###########################################

library(readxl)
library(stringr)
library(tidyverse)
library(janitor)
library(spotifyr)

# you may need to load your token first
# load("introduce_to_spotify.R")

############# Import dataset about charts ############################

getwd()
weekly_tops <- read_excel("CSS_Ukraine.xlsx", sheet = 2, col_types = "text", col_names = TRUE)

# get rid of the helper column

weekly_tops <- weekly_tops[, -1]

# change column formats for peak, streak and streams

weekly_tops[, 1 + 5*(1:52) - 2] <- as.data.frame(lapply(weekly_tops[, 1 + 5*(1:52) - 2], as.numeric))
weekly_tops[, 1 + 5*(1:52) - 1] <- as.data.frame(lapply(weekly_tops[, 1 + 5*(1:52) - 1], as.numeric))
weekly_tops[, 1 + 5*(1:52)] <- as.data.frame(lapply(weekly_tops[, 1 + 5*(1:52)], as.numeric))

############ Import data about the songs #############################

### Step 1: get a "list" of unique songs in the top 200

temp <- weekly_tops

# vector for track information

track_artist <- c(2, 3)
for (i in 1:51) {
  track_artist <- c(track_artist, i*5 + 2, i*5 + 3)
}

temp <- temp[, track_artist]
colnames(temp)[2*(1:52) - 1] <- "track"
colnames(temp)[2*(1:52)] <- "artist"

songs <- temp[, 1:2]

for (i in 2:52) {
  songs <- rbind(songs, temp[, c(2*i - 1, 2*i)])
}

songs <- unique(songs)
# 930 different songs

### Step 2: get them in the form to get info from spotify 

# write a function to get the track id of the songs

# function to get the track id, name, artist and the 
# levenshtein distance to the original artist name and title
get_track_id <- function(name, artist) {
  
  #search spotify for the track name
  search_result <- search_spotify(q = paste0("track:", name, ", ", artist), type = "track", limit = 20)
  # get the information list about the artists
  artists_temp <- search_result$artists
  # get just the artist names
  artists_temp <- lapply(artists_temp, function(x) x[, 3])
  # get the artist names as one string as stored in "songs"
  artists_temp <- lapply(artists_temp, function(x) paste(x, collapse = ", "))
  # add the pure artistname information to search result
  search_result$realartist <- as.data.frame(unlist(artists_temp))
  # get the useful information
  useful_output <- cbind(search_result$id, search_result$name, search_result$realartist)
  colnames(useful_output) <- c("Spotifyid", "Spotifyname", "Spotifyartist")
  # compare the artist names with levenshtein distance
  useful_output$artistcomparison <- stringdist::stringdist(useful_output[, 3], artist, method = "lv")
  useful_output$titlecomparison <- stringdist::stringdist(useful_output[, 2], name, method = "lv")
  useful_output$totalcomparison <- useful_output$artistcomparison + useful_output$titlecomparison
  
  useful_output[which.min(useful_output$totalcomparison), ]
}

track_info <- get_track_id(songs[1, 1], songs[1, 2])
for (i in 2:211) {
  temp <- get_track_id(songs[i, 1], songs[i, 2])
  track_info <- rbind(track_info, temp)
}

# can't find song 212, need to add it by hand
track_info[212, ] <- c("0cXmD7OpJfjmKP7zlrMQHS", songs[212, 1], songs[212, 2], 0, 0, 0)

for (i in 213:227) {
  temp <- get_track_id(songs[i, 1], songs[i, 2])
  track_info <- rbind(track_info, temp)
}
# add 228 by hand
track_info[228, ] <- c("6TYmpWMxhqv0TVtiAIwzBW", songs[228, 1], songs[228, 2], 0, 0, 0)

for (i in 229:300) {
  temp <- get_track_id(songs[i, 1], songs[i, 2])
  track_info <- rbind(track_info, temp)
}

for (i in 301:341) {
  temp <- get_track_id(songs[i, 1], songs[i, 2])
  track_info <- rbind(track_info, temp)
}

# add 342 by hand
track_info[342, ] <- c("6p9KkBAfxdiulHJWBBj5OK", songs[342, 1], songs[342, 2], 0, 0, 0)

for (i in 343:402) {
  temp <- get_track_id(songs[i, 1], songs[i, 2])
  track_info <- rbind(track_info, temp)
}
# add 403 by hand
track_info[403, ] <- c("0oA9wBGDY4uyILLg4GymWP", songs[403, 1], songs[403, 2], 0, 0, 0)

temp <- get_track_id(songs[404, 1], songs[404, 2])
track_info <- rbind(track_info, temp)

# add 405 by hand
track_info[405, ] <- c("1RqjijshsZOAFykrr7WDIr", songs[405, 1], songs[405, 2], 0, 0, 0)

for (i in 406:473) {
  temp <- get_track_id(songs[i, 1], songs[i, 2])
  track_info <- rbind(track_info, temp)
}

# add 474 by hand
track_info[474, ] <- c("0hJHCYRlM65zEENBkwBKe5", songs[474, 1], songs[474, 2], 0, 0, 0)

for (i in 475:484) {
  temp <- get_track_id(songs[i, 1], songs[i, 2])
  track_info <- rbind(track_info, temp)
}

# add 485 by hand
track_info[485, ] <- c("2AbA0a53nHOdeLsYveGrap", songs[485, 1], songs[485, 2], 0, 0, 0)

for (i in 486:500) {
  temp <- get_track_id(songs[i, 1], songs[i, 2])
  track_info <- rbind(track_info, temp)
}

for (i in 501:609) {
  temp <- get_track_id(songs[i, 1], songs[i, 2])
  track_info <- rbind(track_info, temp)
}

for (i in 610:700) {
  temp <- get_track_id(songs[i, 1], songs[i, 2])
  track_info <- rbind(track_info, temp)
}

for (i in 701:734) {
  temp <- get_track_id(songs[i, 1], songs[i, 2])
  track_info <- rbind(track_info, temp)
}

# add 735 by hand
track_info[735, ] <- c("4mn5HdatHKN7iFGDes9G8i", songs[735, 1], songs[735, 2], 0, 0, 0)

for (i in 736:800) {
  temp <- get_track_id(songs[i, 1], songs[i, 2])
  track_info <- rbind(track_info, temp)
}

for (i in 801:821) {
  temp <- get_track_id(songs[i, 1], songs[i, 2])
  track_info <- rbind(track_info, temp)
}

# add 822 by hand
track_info[822, ] <- c("4sFGNz4MYpGoz53ZGCwsiE", songs[822, 1], songs[822, 2], 0, 0, 0)

for (i in 823:850) {
  temp <- get_track_id(songs[i, 1], songs[i, 2])
  track_info <- rbind(track_info, temp)
}

# add 851 by hand
track_info[851, ] <- c("0KlLTyELkOCPsPkNLCdNyn", songs[851, 1], songs[851, 2], 0, 0, 0)

for (i in 852:930) {
  temp <- get_track_id(songs[i, 1], songs[i, 2])
  track_info <- rbind(track_info, temp)
}

rownames(track_info) <- as.character(1:930)

# have a look on the songs where totalcomparison > 0

track_info[track_info$totalcomparison > 0, ]

# check songs and change id manually if needs to
# 82 is okay
track_info[149, "Spotifyid"] <- "4tPB6H0QcK4bDib8iauE28"
track_info[191, "Spotifyid"] <- "1IcR6RlgvDczfvoWJSSY2A"
track_info[194, "Spotifyid"] <- "0lLdorYw7lVrJydTINhWdI"
track_info[232, "Spotifyid"] <- "5VXf1XETOMtk0WZIT4Bw3M"
track_info[318, "Spotifyid"] <- "70qbqCW6z92WESMZC1opsd"
# 329 is okay
track_info[340, "Spotifyid"] <- "7rdIKBTAQm4nLlpA5E8DBI"
track_info[430, "Spotifyid"] <- "05d0sXGApO7BPFY9tMwCXm"
# 477 is okay
track_info[583, "Spotifyid"] <- "2WoksKIDPQ8o90cqDbgE7L"
track_info[586, "Spotifyid"] <- "1GYQZYuD73piEmuzpo1xWx"
track_info[697, "Spotifyid"] <- "1insL38CPnJwWoGk7AH6Sq"
track_info[699, "Spotifyid"] <- "4uUG5RXrOk84mYEfFvj3cK"
track_info[725, "Spotifyid"] <- "6BfnNid8w0UyZqzR2mx1HB"
track_info[738, "Spotifyid"] <- "1hlveB9M6ijHZRbzZ2teyh"
track_info[746, "Spotifyid"] <- "1DIXPcTDzTj8ZMHt3PDt8p"
track_info[873, "Spotifyid"] <- "086myS9r57YsLbJpU0TgK9"
track_info[890, "Spotifyid"] <- "3h9XZd7ZON6LubTC3AqwzT"

# save as .RData

save(track_info, file = "track_info.RData")

# safe the information with original title and artist from website
songs$trackid <- track_info$Spotifyid

save(songs, file = "songs.RData")

### Step 3: get the track information with the track id

get_relevant_information <- function(trackid) {
  useful_output <- data.frame(popularity = c())
  # get popularity of the song by get_track
  popularity <- get_track(trackid)$popularity
  # get the other information by get_track_audio_features
  features <- get_track_audio_features(trackid)
  
  useful_output <- as.vector(c(popularity, features[1, c("danceability", "energy",
                                            "loudness", "speechiness",
                                            "acousticness", "instrumentalness",
                                            "valence", "tempo",
                                            "duration_ms")]))
  
  return(useful_output)
}

songs$popularity <- NA
songs$danceability <- NA
songs$energy <- NA
songs$loudness <- NA
songs$speechiness <- NA
songs$acousticness <- NA
songs$instrumentalness <- NA
songs$valence <- NA
songs$tempo <- NA
songs$duration_ms <- NA

for (i in 1:nrow(songs)) {
  temp <- get_relevant_information(songs$trackid[i])
  songs[i, 4:13] <- temp
}

# safe the information of the 26-01-2024 
save(songs, file = "songs_and_attributes.RData")

### Step 4: get the data of the weekly streams in long format
# with the new information of the songs

# start with the first week manually
data <- data.frame(chart_position = 1:200,
                   week_of = as.Date("2022-01-06"),
                   track = weekly_tops$track_1,
                   artist = weekly_tops$interpret_1,
                   peak = weekly_tops$peak_1,
                   streak = weekly_tops$streak_1,
                   streams = weekly_tops$streams_1)

week_dates <- c("2022-01-13", "2022-01-20", "2022-01-27", "2022-02-03",
                "2022-02-10", "2022-02-17", "2022-02-24", "2022-03-03",
                "2022-03-10", "2022-03-17", "2022-03-24", "2022-03-31",
                "2022-04-07", "2022-04-14", "2022-04-21", "2022-04-28",
                "2022-05-05", "2022-05-12", "2022-05-19", "2022-05-26",
                "2022-06-02", "2022-06-09", "2022-06-16", "2022-06-23",
                "2022-06-30", "2022-07-07", "2022-07-14", "2022-07-21",
                "2022-07-28", "2022-08-04", "2022-08-11", "2022-08-18",
                "2022-08-25", "2022-09-01", "2022-09-08", "2022-09-15",
                "2022-09-22", "2022-09-29", "2022-10-06", "2022-10-13",
                "2022-10-20", "2022-10-27", "2022-11-03", "2022-11-10",
                "2022-11-17", "2022-11-24", "2022-12-01", "2022-12-08",
                "2022-12-15", "2022-12-22", "2022-12-29")

# complete with the other 51
for (i in 2:52) {
  temp <- data.frame(chart_position = 1:200,
                     week_of = as.Date(week_dates[i-1]),
                     track = weekly_tops[[1 + i*5 - 4]],
                     artist = weekly_tops[[1 + i*5 - 3]],
                     peak = weekly_tops[[1 + i*5 - 2]],
                     streak = weekly_tops[[1 + i*5 - 1]],
                     streams = weekly_tops[[1 + i*5]])
  data <- rbind(data, temp)
}

# get the information of the songs such as valence etc. to the existing dataset

data$popularity <- NA
data$danceability <- NA
data$energy <- NA
data$loudness <- NA
data$speechiness <- NA
data$acousticness <- NA
data$instrumentalness <- NA
data$valence <- NA
data$tempo <- NA
data$duration_ms <- NA

# add the infos
for (i in 1:nrow(songs)) {
  if_exists <- nrow(data[(data$track == songs$track[i]) & (data$artist == songs$artist[i]), ]) > 0
  if(if_exists) {
    data[(data$track == songs$track[i]) & (data$artist == songs$artist[i]), 8:17] <- songs[i, 4:13]
  } else {
    print(paste("Warning: not possible for song id", i, sep = " "))
  }
}

# save the data set

save(data, file = "complete.RData")
