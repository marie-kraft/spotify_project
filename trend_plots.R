##### Create trend plots for the interested vars ###########

# load data and packages
library(ggplot2) # for creating nice plots
library(dplyr) # for better data processing
library(modi) # for creating weighted quartiles
load("complete.RData")

#create a color palette for the plots
colors <- c("weighted mean" = "black", "weighted 1st qu." = "#1DB954",
            "weighted median" = "#006633", "weighted 3rd qu." = "#1DB954",
            "mean" = "#666666")

### create a function to make the plots:

tmp <- as.data.frame(data[["week_of"]])
colnames(tmp) <- "week_of"

# create a variable which indicates certain features
data$duration_s <- data$duration_ms / 1000
variable_infos <- data.frame(var = c(colnames(data)[8:16], "duration_s"),
                             axis = c("Popularity rating", "Danceability",
                                      "Energy", "Loudness (dB)", "Speechiness",
                                      "Acousticness", "Instrumentalness",
                                      "Valence", "Tempo (BPM)", "Duration (s)"),
                             low = c(40, 0.55, 0.5, -10, 0.0, 0.0, 0.0, 0.25, 80, 100),
                             high = c(90, 0.85, 0.85, -4, 0.27, 0.4, 0.12, 0.75, 160, 220))

# create a function, which produces trend plots for the variables
# with weighted quantiles etc.
trend_plots <- function(data, variable, varlist, n) {
  prepared <- aggregate(data[[variable]], list(data$week_of), FUN = mean)
  tmp$weighted <- NA
  tmp$lower <- NA
  tmp$median <- NA
  tmp$higher <- NA
  for (i in 1:52) {
    tmp$weighted[1:200 + (i - 1)*200] <- weighted.mean(x = data[[variable]][1:200 + (i - 1)*200],
                                                       w = (data$streams[1:200 + (i - 1)*200]))
    tmp$lower[1:200 + (i - 1)*200] <- weighted.quantile(x = data[[variable]][1:200 + (i - 1)*200],
                                                       w = (data$streams[1:200 + (i - 1)*200]),
                                                       prob = 0.25)
    tmp$median[1:200 + (i - 1)*200] <- weighted.quantile(x = data[[variable]][1:200 + (i - 1)*200],
                                                       w = (data$streams[1:200 + (i - 1)*200]),
                                                       prob = 0.5)
    tmp$higher[1:200 + (i - 1)*200] <- weighted.quantile(x = data[[variable]][1:200 + (i - 1)*200],
                                                       w = (data$streams[1:200 + (i - 1)*200]),
                                                       prob = 0.75)
  }
  
  plot <- ggplot(tmp) +
    geom_hline(aes(yintercept = quantile(data[[variable]], 0.25)), color = "#CCCCCC") +
    geom_hline(aes(yintercept = quantile(data[[variable]], 0.75)), color = "#CCCCCC") +
    geom_vline(aes(xintercept = as.Date("2022-02-24")), color = "#0000FF") +
    geom_vline(aes(xintercept = as.Date("2022-09-21")), color = "#0000FF") +
    geom_line(mapping = aes(x = week_of, y = weighted, color = "weighted mean"), size = 0.8) +
    geom_line(mapping = aes(x = week_of, y = lower, color = "weighted 1st qu."), size = 0.8) +
    geom_line(mapping = aes(x = week_of, y = median, color = "weighted median"), size = 0.8) +
    geom_line(mapping = aes(x = week_of, y = higher, color = "weighted 3rd qu."), size = 0.8) +
    geom_line(prepared, mapping = aes(x = Group.1, y = x, color = "mean"), size = 0.8) +
    theme_minimal() +
    labs(x = "Date", y = paste0(varlist[n, 2], " of TOP200 per week"), color = "Legend") +
    scale_color_manual(values = colors) +
    ylim(varlist[n, 3], varlist[n, 4])
  
  return(plot)
}

# do it for all the variables
for (j in 1:10) {
  tmp_plot <- trend_plots(data, variable_infos$var[j], variable_infos, j)
  ggsave(plot = tmp_plot, filename = paste0("plots/", variable_infos$var[j], "_trend.pdf"), width = 15, height = 10, units = "cm")
}
