matches = read.csv("matches.csv", stringsAsFactors = TRUE)
deliveries = read.csv("deliveries.csv", stringsAsFactors = TRUE)

df = merge(matches, deliveries, by.x = "id", by.y = "match_id")
df$season = as.factor(df$season)

vk = subset(df, df$batsman == "V Kohli")
msd = subset(df, df$batsman == "MS Dhoni")

#Plotting Kohli Vs Dhoni runs by season
vk_season = aggregate(batsman_runs ~ season, data = vk, FUN = sum)
colnames(vk_season) = c("season", "runs_kohli")
msd_season = aggregate(batsman_runs ~ season, data = msd, FUN = sum)
colnames(msd_season) = c("season", "runs_dhoni")
vk_msd_season = merge(vk_season, msd_season)

library(ggplot2)
library(reshape2)
library(RColorBrewer)

vk_msd_season_long = melt(vk_msd_season) #Transforms the data frame to one, with dhoni/kohli as factors
ggplot(vk_msd_season_long, aes(x = season, y = value, fill = variable)) + 
  geom_bar(stat="identity", position = "dodge") + #dodge -- place bars side-to-side
  scale_fill_manual(values = c("red","yellow")) + #scale_fill_manual for barplots
  ggtitle("Kohli vs Dhoni -- Runs by Seasons") +
  labs(x = "Season", y = "Runs")

#Dismissal Analysis
vk_dismissal = subset(vk, vk$player_dismissed == "V Kohli")[,c("season", "dismissal_kind")]
vk_dismissal_long = melt(vk_dismissal)
ggplot(vk_dismissal_long, aes(x = season, y = ..count.. , fill = dismissal_kind)) +
  geom_bar(stat="count") + 
  ggtitle("Kohli -- Dismissals by Seasons") +
  labs(x = "Season", y = "Dismissal Kind")+
  scale_fill_brewer(palette = "Set2")

msd_dismissal = subset(msd, msd$player_dismissed == "MS Dhoni")[,c("season", "dismissal_kind")]
msd_dismissal_long = melt(msd_dismissal)
ggplot(msd_dismissal_long, aes(x = season, y = ..count.. , fill = dismissal_kind)) + 
  geom_bar(stat="count") + 
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
ggplot(vk_msd_dismissal_long, aes(x = season, y = value, fill = variable)) + 
  geom_bar(stat="identity", position = "dodge") + 
  scale_fill_manual(values = c("red","yellow")) + 
  ggtitle("Kohli vs Dhoni -- Dismissals by Seasons") +
  labs(x = "Season", y = "Dismissals")

#Share of runs made 
msd_share = as.data.frame(table(msd$total_runs))
#pos = cumsum(msd_share$Freq) - msd_share$Freq/2

ggplot(msd_share, aes(x = factor(1), y = msd_share$Freq, fill = factor(msd_share$Var1))) + 
  geom_bar(stat = "identity")+
  coord_polar(theta = "y")+
  ggtitle("Dhoni's Run Share")+
  labs(x = "",y = "")+
  scale_fill_discrete(guide_legend(title = "Run Color"))
  #geom_text(aes(x= factor(1), y=pos, label = factor(msd_share$Freq)), size=3)

vk_share = as.data.frame(table(vk$total_runs))
#pos = cumsum(vk_share$Freq) - vk_share$Freq/2

ggplot(vk_share, aes(x = factor(1), y = vk_share$Freq, fill = factor(vk_share$Var1))) + 
  geom_bar(stat = "identity")+
  coord_polar(theta = "y")+
  ggtitle("Kohli's Run Share")+
  labs(x="", y="")+
  scale_fill_discrete(guide_legend(title = "Run Color"))
  #geom_text(aes(x= factor(1), y=pos, label = factor(vk_share$Freq)), size=3)

#Favorite/Worst team
vk$opposition = vk$team1
vk$opposition[vk$opposition == "Royal Challengers Bangalore"] = vk$team2[vk$opposition == "Royal Challengers Bangalore"]
vk_fav_team = aggregate(batsman_runs ~ opposition, data = vk, FUN = sum)
vk_fav_team[with(vk_fav_team, order(-batsman_runs)),]

vk_balls_faced = as.data.frame(table(vk$opposition))
colnames(vk_balls_faced) = c("opposition", "balls_faced")

vk_fav_team = merge(vk_fav_team, vk_balls_faced)

vk_fav_team$average_runs_per_ball = vk_fav_team$batsman_runs/vk_fav_team$balls_faced
vk_fav_team[with(vk_fav_team, order(-average_runs_per_ball)),c("opposition", "average_runs_per_ball")]

msd$opposition = msd$team1
msd$opposition[msd$opposition == "Chennai Super Kings"] = msd$team2[msd$opposition == "Chennai Super Kings"]
msd$opposition[msd$opposition == "Rising Pune Supergiants"] = msd$team2[msd$opposition == "Rising Pune Supergiants"]

msd_fav_team = aggregate(batsman_runs ~ opposition, data = msd, FUN = sum)
msd_fav_team[with(msd_fav_team, order(-batsman_runs)),]

msd_balls_faced = as.data.frame(table(msd$opposition))
colnames(msd_balls_faced) = c("opposition", "balls_faced")

msd_fav_team = merge(msd_fav_team, msd_balls_faced)

msd_fav_team$average_runs_per_ball = msd_fav_team$batsman_runs/msd_fav_team$balls_faced
msd_fav_team[with(msd_fav_team, order(-average_runs_per_ball)),c("opposition", "average_runs_per_ball")]

#Favorite/Worst bowler
vk_fav_bowler = aggregate(batsman_runs ~ bowler, data = vk, FUN = sum)

vk_balls_faced_bowler = as.data.frame(table(vk$bowler))
colnames(vk_balls_faced_bowler) = c("bowler", "balls_faced")
vk_fav_bowler = merge(vk_fav_bowler, vk_balls_faced_bowler)

vk_fav_bowler = subset(vk_fav_bowler, vk_fav_bowler$balls_faced >= 20) #Consider bowlers with more than 20 deliveries

vk_fav_bowler$average_against_bowler = vk_fav_bowler$batsman_runs/vk_fav_bowler$balls_faced

head(vk_fav_bowler[with(vk_fav_bowler, order(-average_against_bowler)),c("bowler", "average_against_bowler")])
tail(vk_fav_bowler[with(vk_fav_bowler, order(-average_against_bowler)),c("bowler", "average_against_bowler")])

msd_fav_bowler = aggregate(batsman_runs ~ bowler, data = msd, FUN = sum)

msd_balls_faced_bowler = as.data.frame(table(msd$bowler))
colnames(msd_balls_faced_bowler) = c("bowler", "balls_faced")
msd_fav_bowler = merge(msd_fav_bowler, msd_balls_faced_bowler)

msd_fav_bowler = subset(msd_fav_bowler, msd_fav_bowler$balls_faced >= 20) #Consider bowlers with more than 20 deliveries

msd_fav_bowler$average_against_bowler = msd_fav_bowler$batsman_runs/msd_fav_bowler$balls_faced

head(msd_fav_bowler[with(msd_fav_bowler, order(-average_against_bowler)),c("bowler", "average_against_bowler")]) 
tail(msd_fav_bowler[with(msd_fav_bowler, order(-average_against_bowler)),c("bowler", "average_against_bowler")])

#Favorite Non-Striker
msd_fav_non_striker = aggregate(batsman_runs ~ non_striker, data = msd, FUN = sum)
head(msd_fav_non_striker[with(msd_fav_non_striker, order(-batsman_runs)),])

vk_fav_non_striker = aggregate(batsman_runs ~ non_striker, data = vk, FUN = sum)
head(vk_fav_non_striker[with(vk_fav_non_striker, order(-batsman_runs)),])

#Favorite Venue
msd_fav_venue = aggregate(batsman_runs ~ venue, data = msd, FUN = sum)
head(msd_fav_venue[with(msd_fav_venue, order(-batsman_runs)),])

vk_fav_venue = aggregate(batsman_runs ~ venue, data = vk, FUN = sum)
head(vk_fav_venue[with(vk_fav_venue, order(-batsman_runs)),])


#Pacing the innings
vk_over_runs = aggregate(batsman_runs ~ over, data = vk, FUN = sum)
vk_overs_faced = as.data.frame(table(vk$over))
colnames(vk_overs_faced) = c("over", "vk_freq")
vk_over_runs = merge(vk_over_runs, vk_overs_faced)
vk_over_runs$vk_strike_rate = (vk_over_runs$batsman_runs/vk_over_runs$vk_freq)*100

msd_over_runs = aggregate(batsman_runs ~ over, data = msd, FUN = sum)
msd_overs_faced = as.data.frame(table(msd$over))
colnames(msd_overs_faced) = c("over", "msd_freq")
msd_over_runs = merge(msd_over_runs, msd_overs_faced)
msd_over_runs$msd_strike_rate = (msd_over_runs$batsman_runs/msd_over_runs$msd_freq)*100

vk_msd_strike_rate = merge(vk_over_runs[,c("over", "vk_strike_rate")], msd_over_runs[,c("over", "msd_strike_rate")])
vk_msd_strike_rate$over = as.factor(vk_msd_strike_rate$over)
vk_msd_strike_rate_long = melt(vk_msd_strike_rate)
ggplot(vk_msd_strike_rate_long, aes(over, value, group = variable, col = variable)) + 
  geom_point() + geom_smooth()+
  ggtitle("Kohli vs Dhoni -- Strike Rate by Over") +
  labs(x = "Over", y = "Strike Rate")

vk_msd_freq = merge(vk_over_runs[,c("over", "vk_freq")], msd_over_runs[,c("over", "msd_freq")])
vk_msd_freq$over = as.factor(vk_msd_freq$over)
vk_msd_freq_long = melt(vk_msd_freq)
ggplot(vk_msd_freq_long, aes(over, value, group = variable, col = variable)) + 
  geom_point() + geom_smooth()+
  ggtitle("Kohli vs Dhoni -- Number of Overs faced") +
  labs(x = "Over", y = "Count")

