

# Load libraries
library(ggplot2)
library(dplyr)
library(car)
library(lmtest)
library(normtest)
library(tseries)
library(GGally)

salaries_players <-read.csv("./data/salaries_player.csv", stringsAsFactors = F)


stats_players <-read.csv("./data/logs_players.csv", stringsAsFactors = F)


player_data <-read.csv("./data/players.csv", stringsAsFactors = F)


teams_data <-read.csv("./data/nba_teams.csv", stringsAsFactors = F)


stats_aggr <- stats_players %>%
  group_by(namePlayer) %>%
  summarize(winrate = mean(isWin == TRUE),
            minutes = mean(minutes, na.rm=TRUE),
            pts = mean(pts, na.rm=TRUE),
            ast = mean(ast, na.rm=TRUE),
            plusminus = mean(plusminus, na.rm=TRUE),
            blk = mean(blk, na.rm=TRUE),
            stl = mean(stl, na.rm=TRUE),
            pctFG3 = mean(pctFG3, na.rm=TRUE),
            tov = mean(tov, na.rm=TRUE),
            pctFG = mean(pctFG, na.rm=TRUE),
            oreb = mean(oreb, na.rm=TRUE),
            dreb = mean(dreb, na.rm=TRUE),
            pf = mean(pf, na.rm=TRUE),
            treb = mean(treb, na.rm=TRUE),
            fg2m = mean(fg2m, na.rm=TRUE),
            fg2a = mean(fg2a, na.rm=TRUE),
            fga = mean(fga, na.rm=TRUE),
            fgm = mean(fgm, na.rm=TRUE),
            ftm = mean(ftm, na.rm=TRUE))%>%
  ungroup()


merged = merge(salaries_players, stats_aggr, by.x="Player", by.y="namePlayer")

merged = merge(x = stats_aggr, y = salaries_players[ , c("Player", "Salary")], by.x="namePlayer", by.y="Player")

merged = merge(merged, player_data[ , c("namePlayer", "countSeasons", "nameTeam", "idLeagueOtherExperience")], by.x="namePlayer", by.y="namePlayer")

merged = merge(merged, teams_data[ , c("nameTeam", "idConference", "idDivision")], by.x="nameTeam", by.y="nameTeam")

merged$idConference <- as.factor(merged$idConference)

merged$idDivision <- as.factor(merged$idDivision)

merged$idLeagueOtherExperience <- as.factor(merged$idLeagueOtherExperience)

merged$logsalary <- log(merged$Salary)
merged$sqrtsalary <- sqrt(merged$Salary)


##ggpairs(merged, columns = c("sqrtsalary", "blk", "minutes", "pts", "ast",  "idConference" ))


