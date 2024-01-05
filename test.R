if (!requireNamespace("spotifyr", quietly = TRUE)) {
  install.packages("spotifyr")
}
library(spotifyr)

# Set up your Spotify API credentials
Sys.setenv(SPOTIFY_CLIENT_ID = "d795a6fb977a4c49ab04701b516505fa",
           SPOTIFY_CLIENT_SECRET = "b1282c4fc20748e0a6feb8694d9ee8a4")

# Get an access token
token <- get_spotify_access_token()

# Get the Spotify charts data for Ukraine
charts_data <- get_playlist("37i9dQZEVXbJiZcmkrIHGU?", authorization = token)
View(charts_data$tracks$items)
#charts_data <- get_spotify_charts(region = "ua", chart = "top200", date = Sys.Date())

# Print the names of the top 10 tracks
cat("Top 10 tracks in Ukraine:\n")
cat(charts_data$track_name[1:10], "\n")


days <- unlist(chart_daily())

lapply(days[1:3], function(i) chart_top200_daily("global", days = i))