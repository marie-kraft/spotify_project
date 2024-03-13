####### You need to get your Spotify API credentials ##############
# Go on Spotify for Developer, log in and follow the instructions
# https://developer.spotify.com/documentation/web-api/tutorials/getting-started

# load the spotifyr package
if (!requireNamespace("spotifyr", quietly = TRUE)) {
  install.packages("spotifyr")
}
library(spotifyr)

# Set up your Spotify API credentials
Sys.setenv(SPOTIFY_CLIENT_ID = "insertIDhere",
           SPOTIFY_CLIENT_SECRET = "insertSECREThere")

# Get an access token
token <- get_spotify_access_token()

# You can use this for example like this:
# get the Spotify charts data for Ukraine:
# the ID of this playlist is: 37i9dQZEVXbJiZcmkrIHGU?
charts_data <- get_playlist("37i9dQZEVXbJiZcmkrIHGU?", authorization = token)
View(charts_data$tracks$items)