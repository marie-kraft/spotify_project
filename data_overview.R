######## Create some overall plots and measurements ##########

library(ggplot2)
library(dplyr)
load("complete.RData")
load("songs_and_attributes.RData")

#### total weekly number of plays
total_streams_info <- data %>%
  group_by(week_of) %>%
  summarise(total_streams = sum(streams))

total_streams_info$total_streams_m <- total_streams_info$total_streams / 1000000
summary(total_streams_info) # for report

total_streams <- ggplot(total_streams_info) +
  geom_line(mapping = aes(x = week_of, y = total_streams_m)) +
  theme_minimal() +
  labs(x = "Date", y = "Number of streams (in millions)") +
  geom_vline(aes(xintercept = as.Date("2022-02-24")), color = "#0000FF") +
  geom_vline(aes(xintercept = as.Date("2022-09-21")), color = "#0000FF")
ggsave(plot = total_streams, filename = "plots/total_streams.pdf",
       height = 7.5, width = 15, units = "cm")

#### table for displaying the variables

table_vars <- data.frame(row.names = c("Min", "1st Qu.", "Median", "Mean",
                                       "3rd Qu.", "Max"))
table_vars$popularity <- NA
table_vars$danceability <- NA
table_vars$energy <- NA
table_vars$loudness <- NA
table_vars$speechiness <- NA
table_vars$acousticness <- NA
table_vars$instrumentalness <- NA
table_vars$valence <- NA
table_vars$tempo <- NA
table_vars$duration_ms <- NA

for (i in 1:10) {
  table_vars[c(1, 2, 3, 5, 6), i] <- quantile(songs[[i + 3]], c(0, 0.25, 0.5, 0.75, 1))
  table_vars[4, i] <- mean(songs[[i + 3]])
}

table_vars <- table_vars %>%
  mutate(popularity = round(popularity, 1),
         danceability = round(danceability, 3),
         energy = round(energy, 3),
         loudness = round(loudness, 1),
         speechiness = round(speechiness, 3),
         acousticness = round(acousticness, 5),
         instrumentalness = round(instrumentalness, 5),
         valence = round(valence, 3),
         tempo = round(tempo, 1),
         duration_ms = round(duration_ms, 0))
table_vars <- t(table_vars)

View(table_vars)
