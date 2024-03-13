##### Plots for the presentation

# load data and packages
library(ggplot2) # for creating nice plots
library(dplyr) # for better data processing
library(modi) # for creating weighted quartiles
load("songs_and_attributes.RData")
load("complete.RData")

### Valence plot overall ###
# create histogram
valence_hist <- ggplot(data = songs) +
  geom_histogram(mapping = aes(x = valence), fill = "white",
                 colour = "black", binwidth = 0.01) +
  xlim(-0.01, 1.01) +
  theme_minimal()
# create boxplot
valence_box <- ggplot(data = songs) +
  geom_boxplot(mapping = aes(x = valence), fill = "white",
               colour = "black", width = 0.1) +
  xlim(-0.01, 1.01) +
  theme_transparent()
# prepare annotation
valence_box_grop <- ggplotGrob(valence_box)
valence_table <- ggtexttable(round(summary(songs$valence), 2),
                             rows = c("Min", "1. Qu.", "Median", "Mean", "3. Qu.", "Max"),
                             theme = ttheme("blank", base_size = 8))
valence_text <- paste("A measure from 0.0 to 1.0 describing the musical",
                      "positiveness conveyed by a track. Tracks with",
                      "high valence sound more positive (e.g. happy,",
                      "cheerful, euphoric), while tracks with low",
                      "valence sound more negative (e.g. sad, depressed,",
                      "angry).", sep = " ")
valence_text <- ggparagraph(text = valence_text, face = "italic", size = 8, color = "black")
# add the boxplot to histogram
valence_hist + annotation_custom(grob = valence_box_grop, xmin = -0.085, xmax = 1.08,
                                 ymin = -(1/15)*30, ymax = 0) +
  annotation_custom(ggplotGrob(valence_table), xmin = 0.8, xmax = 1.05, ymin = 13, ymax = 22) +
  annotation_custom(ggplotGrob(valence_text), xmin = 0.55, xmax = 1.05, ymin = 22)



### Trend of valence in lineplot

prepared <- aggregate(data$valence, list(data$week_of), FUN = mean)
data$valence_weighted <- NA
data$valence_lower <- NA
data$valence_median <- NA
data$valence_higher <- NA

# create the weighted quartiles with the streams per week by total streams 
# of this week
for (i in 1:52) {
  data$valence_weighted[1:200 + (i - 1)*200] <- weighted.mean(x = data$valence[1:200 + (i - 1)*200],
                                                              w = (data$streams[1:200 + (i - 1)*200]))
  data[1:200 + (i - 1)*200, 19] <- weighted.quantile(x = data$valence[1:200 + (i - 1)*200],
                                                        w = (data$streams[1:200 + (i - 1)*200]),
                                                        prob = 0.25)
  data[1:200 + (i - 1)*200, 20] <- weighted.quantile(x = data$valence[1:200 + (i - 1)*200],
                                                        w = (data$streams[1:200 + (i - 1)*200]),
                                                        prob = 0.5)
  data[1:200 + (i - 1)*200, 21] <- weighted.quantile(x = data$valence[1:200 + (i - 1)*200],
                                                        w = (data$streams[1:200 + (i - 1)*200]),
                                                        prob = 0.75)
}

# prepare the legend
colors <- c("weighted mean" = "black", "weighted 1st qu." = "#1DB954",
            "weighted median" = "#006633", "weighted 3rd qu." = "#1DB954",
            "mean" = "#666666")
# make trend plot
ggplot(data) +
  geom_line(mapping = aes(x = week_of, y = valence_weighted, color = "weighted mean"), size = 0.8) +
  geom_line(mapping = aes(x = week_of, y = valence_lower, color = "weighted 1st qu."), size = 0.8) +
  geom_line(mapping = aes(x = week_of, y = valence_median, color = "weighted median"), size = 0.8) +
  geom_line(mapping = aes(x = week_of, y = valence_higher, color = "weighted 3rd qu."), size = 0.8) +
  geom_line(prepared, mapping = aes(x = Group.1, y = x, color = "mean"), size = 0.8) +
  theme_minimal() +
  labs(x = "Date", y = "Valence of TOP200 per week", color = "Legend") +
  scale_color_manual(values = colors) +
  scale_x_date(date_breaks = "2 months") +
  ylim(0.25, 0.7) +
  geom_hline(aes(yintercept = 0.31), color = "#CCCCCC") +
  geom_hline(aes(yintercept = 0.64), color = "#CCCCCC") +
  geom_vline(aes(xintercept = as.Date("2022-02-24")), color = "#0000FF") +
  geom_vline(aes(xintercept = as.Date("2022-09-21")), color = "#0000FF")
  
  
