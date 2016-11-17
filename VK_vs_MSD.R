matches = read.csv("matches.csv", stringsAsFactors = TRUE)
deliveries = read.csv("deliveries.csv", stringsAsFactors = TRUE)

df = merge(matches, deliveries, by.x = "id", by.y = "match_id")
df$umpire3 = NULL

vk = subset(df, df$batsman == "V Kohli")
msd = subset(df, df$batsman == "MS Dhoni")

vk_player_of_match = subset(matches, matches$player_of_match == "V Kohli")
msd_player_of_match = subset(matches, matches$player_of_match == "MS Dhoni")
vk_player_of_match_count = table(vk_player_of_match$season) 
msd_player_of_match_count = table(msd_player_of_match$season)
barplot(vk_player_of_match_count)
barplot(msd_player_of_match_count)

vk_season = aggregate(batsman_runs ~ season, data = vk, FUN = sum)
msd_season = aggregate(batsman_runs ~ season, data = msd, FUN = sum)
plot(vk_season$season, vk_season$batsman_runs, type = "o")
plot(msd_season$season, msd_season$batsman_runs, type = "o")
