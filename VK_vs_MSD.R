matches = read.csv("matches.csv", stringsAsFactors = TRUE)
deliveries = read.csv("deliveries.csv", stringsAsFactors = TRUE)

df = merge(matches, deliveries, by.x = "id", by.y = "match_id")
df$umpire3 = NULL

vk = subset(df, df$batsman == "V Kohli")
msd = subset(df, df$batsman == "MS Dhoni")

#Not very useful!
vk_player_of_match = subset(matches, matches$player_of_match == "V Kohli")
msd_player_of_match = subset(matches, matches$player_of_match == "MS Dhoni")
vk_player_of_match_count = table(vk_player_of_match$season) 
msd_player_of_match_count = table(msd_player_of_match$season)
barplot(vk_player_of_match_count)
barplot(msd_player_of_match_count)

#Plotting Kohli Vs Dhoni runs by season
vk_season = aggregate(batsman_runs ~ season, data = vk, FUN = sum)
colnames(vk_season) = c("season", "runs_kohli")
msd_season = aggregate(batsman_runs ~ season, data = msd, FUN = sum)
colnames(msd_season) = c("season", "runs_dhoni")
vk_msd_season = merge(vk_season, msd_season)
vk_msd_season$season = as.factor(vk_msd_season$season)

library(ggplot2)

library(reshape2)
vk_msd_season_long = melt(vk_msd_season) #Transforms the data frame to one, with dhoni/kohli as factors
ggplot(vk_msd_season_long, aes(x = season, y = value, fill = variable)) + #x_axis is season, y_axis is value(runs), diff_factor is variable(kohli/dhoni)
  geom_bar(stat="identity", position = "dodge") + #dodge means place bars side-to-side
  scale_fill_manual(values = c("red","yellow")) + #scale_fill_manual for barplots, scale_color_manual for line/scatter plots
  ggtitle("Kohli vs Dhoni -- Runs by Seasons") +
  labs(x = "Season", y = "Runs")
