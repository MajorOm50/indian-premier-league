matches = read.csv("matches.csv", stringsAsFactors = TRUE)
deliveries = read.csv("deliveries.csv", stringsAsFactors = TRUE)

df = merge(matches, deliveries, by.x = "id", by.y = "match_id")
df$umpire3 = NULL
df$season = as.factor(df$season)

vk = subset(df, df$batsman == "V Kohli")
msd = subset(df, df$batsman == "MS Dhoni")

# #Not very useful!
# vk_player_of_match = subset(matches, matches$player_of_match == "V Kohli")
# msd_player_of_match = subset(matches, matches$player_of_match == "MS Dhoni")
# vk_player_of_match_count = table(vk_player_of_match$season) 
# msd_player_of_match_count = table(msd_player_of_match$season)
# barplot(vk_player_of_match_count)
# barplot(msd_player_of_match_count)

#Plotting Kohli Vs Dhoni runs by season
vk_season = aggregate(batsman_runs ~ season, data = vk, FUN = sum)
colnames(vk_season) = c("season", "runs_kohli")
msd_season = aggregate(batsman_runs ~ season, data = msd, FUN = sum)
colnames(msd_season) = c("season", "runs_dhoni")
vk_msd_season = merge(vk_season, msd_season)

library(ggplot2)

library(reshape2)
vk_msd_season_long = melt(vk_msd_season) #Transforms the data frame to one, with dhoni/kohli as factors
ggplot(vk_msd_season_long, aes(x = season, y = value, fill = variable)) + #x_axis is season, y_axis is value(runs), diff_factor is variable(kohli/dhoni)
  geom_bar(stat="identity", position = "dodge") + #dodge means place bars side-to-side
  scale_fill_manual(values = c("red","yellow")) + #scale_fill_manual for barplots, scale_color_manual for line/scatter plots
  ggtitle("Kohli vs Dhoni -- Runs by Seasons") +
  labs(x = "Season", y = "Runs")

library(RColorBrewer)
#Dismissal Analysis
vk_dismissal = subset(vk, vk$player_dismissed == "V Kohli")[,c("season", "dismissal_kind")]
vk_dismissal_long = melt(vk_dismissal)
ggplot(vk_dismissal_long, aes(x = season, y = ..count.. , fill = dismissal_kind)) + #x_axis is season, y_axis is value(runs), diff_factor is variable(kohli/dhoni)
  geom_bar(stat="count") + #dodge means place bars side-to-side
  ggtitle("Kohli -- Dismissals by Seasons") +
  labs(x = "Season", y = "Dismissal Kind")+
  scale_fill_brewer(palette = "Set2")

msd_dismissal = subset(msd, msd$player_dismissed == "MS Dhoni")[,c("season", "dismissal_kind")]
msd_dismissal_long = melt(msd_dismissal)
ggplot(msd_dismissal_long, aes(x = season, y = ..count.. , fill = dismissal_kind)) + #x_axis is season, y_axis is value(runs), diff_factor is variable(kohli/dhoni)
  geom_bar(stat="count") + #dodge means place bars side-to-side
  ggtitle("Dhoni -- Dismissals by Seasons") +
  labs(x = "Season", y = "Dismissal Kind")+
  scale_fill_brewer(palette = "Set2")

#Dismissal counts -- by seasons
vk_dismissal_count = as.data.frame(table(vk_dismissal$season))
colnames(vk_dismissal_count) = c("season", "vk_dismissal")
msd_dismissal_count = as.data.frame(table(msd_dismissal$season))
colnames(msd_dismissal_count) = c("season", "msd_dismissal")
vk_msd_dismissal = merge(vk_dismissal_count, msd_dismissal_count)

vk_msd_dismissal_long = melt(vk_msd_dismissal)
ggplot(vk_msd_dismissal_long, aes(x = season, y = value, fill = variable)) + #x_axis is season, y_axis is value(runs), diff_factor is variable(kohli/dhoni)
  geom_bar(stat="identity", position = "dodge") + #dodge means place bars side-to-side
  scale_fill_manual(values = c("red","yellow")) + #scale_fill_manual for barplots, scale_color_manual for line/scatter plots
  ggtitle("Kohli vs Dhoni -- Dismissals by Seasons") +
  labs(x = "Season", y = "Dismissals")

#Share of runs made
ggplot(msd, aes(x = factor(1), fill = factor(total_runs))) + geom_bar(width = 1)+
  coord_polar(theta = "y")+
  ggtitle("Dhoni's Run Share")+
  labs(x = "",y = "")+
  scale_fill_discrete(guide_legend(title = "Run Color"))

ggplot(vk, aes(x = factor(1), fill = factor(total_runs))) + geom_bar(width = 1)+
  coord_polar(theta = "y")+
  ggtitle("Kohli's Run Share")+
  labs(x="", y="")+
  scale_fill_discrete(guide_legend(title = "Run Color"))
  
#Favorite/Worst team
vk$opposition = vk$team1
vk$opposition[vk$opposition == "Royal Challengers Bangalore"] = vk$team2[vk$opposition == "Royal Challengers Bangalore"]
vk_fav_team = aggregate(batsman_runs ~ opposition, data = vk, FUN = sum)
vk_fav_team[with(vk_fav_team, order(-batsman_runs)),]

vk_balls_faced = as.data.frame(table(vk$opposition))
colnames(vk_balls_faced) = c("opposition", "balls_faced")

vk_fav_team = merge(vk_fav_team, vk_balls_faced)

vk_fav_team$average_against_team = vk_fav_team$batsman_runs/vk_fav_team$balls_faced
vk_fav_team[with(vk_fav_team, order(-average_against_team)),c("opposition", "average_against_team")]

msd$opposition = msd$team1
msd$opposition[msd$opposition == "Chennai Super Kings"] = msd$team2[msd$opposition == "Chennai Super Kings"]
msd$opposition[msd$opposition == "Rising Pune Supergiants"] = msd$team2[msd$opposition == "Rising Pune Supergiants"]

msd_fav_team = aggregate(batsman_runs ~ opposition, data = msd, FUN = sum)
msd_fav_team[with(msd_fav_team, order(-batsman_runs)),]

msd_balls_faced = as.data.frame(table(msd$opposition))
colnames(msd_balls_faced) = c("opposition", "balls_faced")

msd_fav_team = merge(msd_fav_team, msd_balls_faced)

msd_fav_team$average_against_team = msd_fav_team$batsman_runs/msd_fav_team$balls_faced
msd_fav_team[with(msd_fav_team, order(-average_against_team)),c("opposition", "average_against_team")]

#Favorite/Worst bowler
vk_fav_bowler = aggregate(batsman_runs ~ bowler, data = vk, FUN = sum)

vk_balls_faced_bowler = as.data.frame(table(vk$bowler))
colnames(vk_balls_faced_bowler) = c("bowler", "balls_faced")
vk_fav_bowler = merge(vk_fav_bowler, vk_balls_faced_bowler)

vk_fav_bowler = subset(vk_fav_bowler, vk_fav_bowler$balls_faced >= 20) #Take only bowlers who've bowled more than 20 deliveries

vk_fav_bowler$average_against_bowler = vk_fav_bowler$batsman_runs/vk_fav_bowler$balls_faced
head(vk_fav_bowler[with(vk_fav_bowler, order(-average_against_bowler)),c("bowler", "average_against_bowler")])
tail(vk_fav_bowler[with(vk_fav_bowler, order(-average_against_bowler)),c("bowler", "average_against_bowler")])

msd_fav_bowler = aggregate(batsman_runs ~ bowler, data = msd, FUN = sum)

msd_balls_faced_bowler = as.data.frame(table(msd$bowler))
colnames(msd_balls_faced_bowler) = c("bowler", "balls_faced")
msd_fav_bowler = merge(msd_fav_bowler, msd_balls_faced_bowler)

msd_fav_bowler = subset(msd_fav_bowler, msd_fav_bowler$balls_faced >= 20) #Take only bowlers who've bowled more than 20 deliveries

msd_fav_bowler$average_against_bowler = msd_fav_bowler$batsman_runs/msd_fav_bowler$balls_faced
head(msd_fav_bowler[with(msd_fav_bowler, order(-average_against_bowler)),c("bowler", "average_against_bowler")])
tail(msd_fav_bowler[with(msd_fav_bowler, order(-average_against_bowler)),c("bowler", "average_against_bowler")])

#Favorite Non-Striker
msd_fav_non_striker = aggregate(batsman_runs ~ non_striker, data = msd, FUN = sum)
head(msd_fav_non_striker[with(msd_fav_non_striker, order(-batsman_runs)),])

vk_fav_non_striker = aggregate(batsman_runs ~ non_striker, data = vk, FUN = sum)
head(vk_fav_non_striker[with(vk_fav_non_striker, order(-batsman_runs)),])
