player_data <- read.csv("total_rookies_stats.csv")


#Box Plot of Rookies Score Tier vs Team WL%
ppg_values <- c(player_data$PPG)
WL <- c(player_data$W.L.)
WL_breaks <- c(0,0.25,0.5,0.75,1)
WL_plot <- cut(WL,WL_breaks,,right = FALSE)
score_tiers <-c("Bench(0-10)", "Role (10-15)", "Starter (15-20)", "Star(20+)")
breaks <- c(0,10,15,20,100)
ppg_plot <- cut(ppg_values, breaks,score_tiers, right = FALSE)
ppg_plot
plot(ppg_plot,WL, ylim = c(0,1))

#Average PPG by rookie based on team 
tapply(player_data$PPG, player_data$Team, mean)

#Rookies under the age of 21 their rookie season
subset(player_data$Player, player_data$Age<21)


#Lakers rookies
lakers_players <- subset(player_data, Team == "Los Angeles Lakers")
lakers_players$Player

#Average RPG for rookies by team
tapply(player_data$RPG, player_data$Team, mean)

#Rookies that played more than 50 games
active_rookies <- subset(player_data, G > 50)
active_rookies$Player

#Table of Teams and number of players at each age
table(player_data$Team, player_data$Age)

#Rookies who are 19 years old
age_19_rookies <- subset(player_data, Age == 19)
age_19_rookies$Player





#Bar Graph of top 10 rookie scorers
top_scorers <- head(player_data[order(-player_data$PPG), ], 10)
top_scorers
barplot(top_scorers$PPG,names.arg = top_scorers$Player, main="Top 10 Rookie Scorers", ylab="Points Per Game",ylim=c(0,30), col="lightblue",las = 2,cex.names = 0.65)

#Scatter Plot of top 30 rookie scorers vs Team WL%
player_data$production <- c(player_data$PPG+player_data$APG+player_data$RPG)
top_production <- head(player_data[order(-player_data$production), ], 30)
plot(top_production$production, top_production$W.L., main ="W/L Ratio For Top 30 Players Production", xlab ="Players Production (PPG+RPG+APG)", ylab ="W/L Ratio",ylim = c(0,1),xlim =c(30,45), col="maroon")
text(top_production$production[top_production$W.L.>0.65] ,top_production$W.L.[top_production$W.L.>0.65], labels = top_production$Player[top_production$W.L.>0.65], pos = 4, cex = 0.8)

#Box Plot for Rookie PTS+REB+AST vs W/L Ratio(Potential not use)
pra_values <- player_data$PPG+player_data$RPG+player_data$APG
production_tiers = score_tiers <-c("Bench(0-10)", "Role (10-20)", "Starter (20-30)", "Star(30+)")
breaks <- c(0,10,20,30,100)
pra_plot <- cut(pra_values,breaks,production_tiers,right = FALSE)
plot(pra_plot,WL,xlab = "Player Production(Points+Rebounds+Assists)", ylab ="W/L Ratio", color = "red")

#Average number of assists by age
tapply(player_data$APG, player_data$Age, mean)


#Knicks players age vs production
knicks_players <- subset(player_data, Team == "New York Knicks")
knicks_players$Player
knicks_production <- knicks_players$PPG+knicks_players$APG+knicks_players$RPG
knicks_age <- knicks_players$Age
plot(knicks_age,knicks_production, main = "Knick Rookie Production vs Age Drafted", xlab = "Player Age",ylab = "Production (PPG+RPG+APG)")
text(knicks_age[knicks_production>30],knicks_production[knicks_production>30], labels = knicks_players$Player[knicks_production >30],pos=4, cex = 0.8,col = "maroon")

#Team vs average draft age
mean_age <- tapply(player_data$Age,player_data$Team,mean)
mean_age
mean_age <- sort(mean_age,decreasing = TRUE)
barplot(mean_age-20, ylim = c(0,5), main = "Mean Rookie Age by Teams", ylab = "Age Over 20 Years Old", col = "navy",las = 2,cex.names = 0.7)

#Top 15 rookie production
player_data$production <- c(player_data$PPG+player_data$APG+player_data$RPG)
top_production <- head(player_data[order(-player_data$production), ], 15)
top_production$production
top_production$Player
barplot(top_production$production,names.arg = top_production$Player, main="Top 15 Rookie Production", ylab="Production",ylim=c(0,50), col="lightblue",las = 2,cex.names = 0.6)

#share of top 10 rookie production
top_share <- head(top_production[order(-top_production$Share), ], 15)
barplot(top_share$Share,names.arg = top_share$Player,main = "Share of ROTY Votes Recieved by Top 15 Rookie Production", ylab = "Rookie Of The Year Voting Share",col = "beige",las = 2,cex.names = 0.6)
