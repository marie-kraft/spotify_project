######  Plot the Information of the songs with a nice ggplot ######

library(ggplot2)
library(dplyr)
library(ggpubr)

#### show the overall sound of the 930 songs ###

load("songs_and_attributes.RData")

### Popularity
pop_hist <- ggplot(data = songs) +
  geom_histogram(mapping = aes(x = popularity), fill = "white",
                 colour = "black", binwidth = 1) +
  xlim(-1, 101) +
  theme_minimal() +
  labs(x = "Popularity rating", y = "Frequency")
pop_hist
ggsave("plots/popularity_hist.pdf", width = 15, height = 8, units = "cm")

### Danceability
dance_hist <- ggplot(data = songs) +
  geom_histogram(mapping = aes(x = danceability), fill = "white",
                 colour = "black", binwidth = 0.01) +
  xlim(-0.01, 1.01) +
  theme_minimal() +
  labs(x = "Danceability", y = "Frequency")
dance_hist
ggsave("plots/danceability_hist.pdf", width = 15, height = 8, units = "cm")

### Energy
ggplot(data = songs) +
  geom_histogram(mapping = aes(x = energy), fill = "white",
                 colour = "black", binwidth = 0.01) +
  xlim(-0.01, 1.01) +
  theme_minimal() +
  labs(x = "Energy", y = "Frequency")
ggsave("plots/energy_hist.pdf", width = 15, height = 8, units = "cm")

### Loudness
ggplot(data = songs) +
  geom_histogram(mapping = aes(x = loudness), fill = "white",
                 colour = "black", binwidth = 1) +
  xlim(-61, 1) +
  theme_minimal() +
  labs(x = "Loudness (dB)", y = "Frequency")
ggsave("plots/loudness_hist.pdf", width = 15, height = 8, units = "cm")

### Speechiness
ggplot(data = songs) +
  geom_histogram(mapping = aes(x = speechiness), fill = "white",
                 colour = "black", binwidth = 0.01) +
  xlim(-0.01, 1.01) +
  geom_vline(aes(xintercept = 0.33), color = "#1DB954") +#under: music and other non-speech like tracks
  geom_vline(aes(xintercept = 0.66), color = "#1DB954") +#over: entirely spoken
  theme_minimal() +
  labs(x = "Speechiness", y = "Frequency")
ggsave("plots/speechiness_hist.pdf", width = 15, height = 8, units = "cm")

### Acousticness
ggplot(data = songs) +
  geom_histogram(mapping = aes(x = acousticness), fill = "white",
                 colour = "black", binwidth = 0.01) +
  xlim(-0.01, 1.01) +
  theme_minimal() +
  labs(x = "Acousticness", y = "Frequency")
ggsave("plots/acousticness_hist.pdf", width = 15, height = 8, units = "cm")

### Instrumentalness
ggplot(data = songs) +
  geom_histogram(mapping = aes(x = instrumentalness), fill = "white",
                 colour = "black", binwidth = 0.01) +
  xlim(-0.01, 1.01) +
  geom_vline(aes(xintercept = 0.5), color = "#1DB954") +#over: instrumental tracks
  theme_minimal() +
  labs(x = "Instrumentalness", y = "Frequency")
ggsave("plots/instrumentalness_hist.pdf", width = 15, height = 8, units = "cm")

# make a plot without zero values:
data_instrumental <- songs[!(songs$instrumentalness == 0), c("track", "artist", "instrumentalness")]
ggplot(data = data_instrumental) +
  geom_histogram(mapping = aes(x = instrumentalness), fill = "white",
                 colour = "black", binwidth = 0.01) +
  xlim(-0.01, 1.01) +
  geom_vline(aes(xintercept = 0.5), color = "#1DB954") +#over: instrumental tracks
  theme_minimal() +
  labs(x = "Instrumentalness", y = "Frequency")
ggsave("plots/instrumentalness_hist_2.pdf", width = 15, height = 8, units = "cm")

### Valence ###
valence_hist <- ggplot(data = songs) +
  geom_histogram(mapping = aes(x = valence), fill = "white",
                 colour = "black", binwidth = 0.01) +
  xlim(-0.01, 1.01) +
  theme_minimal() +
  labs(x = "Valence", y = "Frequency")
valence_hist
ggsave("plots/valence_hist.pdf", width = 15, height = 8, units = "cm")


## Tempo
ggplot(data = songs) +
  geom_histogram(mapping = aes(x = tempo), fill = "white",
                 colour = "black", binwidth = 1) +
  xlim(59, 221) +
  geom_vline(aes(xintercept = 65), color = "#1DB954") + #Adagio
  geom_vline(aes(xintercept = 109), color = "#1DB954") + #Allegro
  geom_vline(aes(xintercept = 168), color = "#1DB954") + #Presto
  geom_vline(aes(xintercept = 178), color = "#1DB954") + #Prestissimo
  theme_minimal() +
  labs(x = "Tempo (BPM)", y = "Frequency")
ggsave("plots/tempo_hist.pdf", width = 15, height = 8, units = "cm")

### Duration
#x axis breaks every minute
songs$duration_s <- songs$duration_ms / 1000
ggplot(data = songs) +
  geom_histogram(mapping = aes(x = duration_s), fill = "white",
                 colour = "black", binwidth = 5) +
  xlim(30, 500) +
  scale_x_continuous(breaks = c(60, 120, 180, 240, 300, 360, 420, 480)) +
  theme_minimal() +
  labs(x = "Duration (s)", y = "Frequency")
ggsave("plots/duration_hist.pdf", width = 15, height = 8, units = "cm")
