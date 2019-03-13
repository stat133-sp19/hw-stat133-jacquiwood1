# Title: make-shot-charts-data-script
# Description: Make shot charts for each of the players
# Inputs: Iguodola, Green, Durant, Thompson, Curry data
# Outputs: shot chart for each player and a combined shot chart facetted by player

library(ggplot2)

# scatterplot
klay_scatterplot <- ggplot(data = thompson) +
  geom_point(aes(x = x, y = y, color = shot_made_flag))

library(jpeg)
library(grid)

# court image (to be used as background of plot)
court_file <- "../images/nba-court.jpg"

# create raste object
court_image <- rasterGrob(
  readJPEG(court_file),
  width = unit(1, "npc"),
  height = unit(1, "npc"))

# shot chart with court background
klay_shot_chart <- ggplot(data = thompson) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Klay Thompson (2016 season)') +
  theme_minimal()

#4.1

andre_shot_chart <- ggplot(data = iguodala) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Andre Iguodala (2016 season)') +
  theme_minimal()

pdf("../images/andre-iguodala-shot-chart.pdf",width=6.5,height=5) 
andre_shot_chart
dev.off() 

draymond_shot_chart <- ggplot(data = green) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Draymond Green (2016 season)') +
  theme_minimal()

pdf("../images/draymond-green-shot-chart.pdf",width=6.5,height=5) 
draymond_shot_chart
dev.off() 

kevin_shot_chart <- ggplot(data = durant) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Kevin Durant (2016 season)') +
  theme_minimal()

pdf("../images/kevin-durant-shot-chart.pdf",width=6.5,height=5) 
kevin_shot_chart
dev.off() 

pdf("../images/klay-thompson-shot-chart.pdf",width=6.5,height=5) 
klay_shot_chart
dev.off() 

stephen_shot_chart <- ggplot(data = curry) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Stephen Curry (2016 season)') +
  theme_minimal()

pdf("../images/stephen-curry-shot-chart.pdf",width=6.5,height=5) 
stephen_shot_chart
dev.off() 

# 4.2
gsw_shot_chart <- ggplot(data = shots_data) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: GSW (2016 season)') +
  theme_minimal() + facet_wrap(~ name)

pdf("../images/gsw-shot-chart.pdf",width=8,height=7,units="in") 
gsw_shot_chart
dev.off()

png("../images/gsw-shot-chart.png",width=8,height= 7,units= "in") 
gsw_shot_chart
dev.off()

