
if (!require("ggplot2"))
  install.packages("ggplot2")
if (!require("dplyr"))
  install.packages("dplyr")
if (!require("ElemStatLearn"))
  install.packages("car")
if (!require("lmtest"))
  install.packages("lmtest")
if (!require("normtest"))
  install.packages("normtest")
if (!require("tseries"))
  install.packages("tseries")
if (!require("GGally"))
  install.packages("GGally")

# Load libraries
library(ggplot2)
library(dplyr)
library(car)
library(lmtest)
library(normtest)
library(tseries)
library("GGally")

setwd("C:/workspace/University/DataProcesses/NBA_Project")

salaries_players <-
  read.delim(
    './data/salaries_player.csv',
    sep = ",",
    header = TRUE,
    dec = "."
  )

stats_players <-
  read.delim(
    './data/logs_players.csv',
    sep = ",",
    header = TRUE,
    dec = "."
  )

player_data <-
  read.delim(
    './data/players.csv',
    sep = ",",
    header = TRUE,
    dec = "."
  )


teams_data <-
  read.delim(
    './data/nba_teams.csv',
    sep = ",",
    header = TRUE,
    dec = "."
  )

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

merged = merge(merged, player_data[ , c("namePlayer", "countSeasons", "nameTeam")], by.x="namePlayer", by.y="namePlayer")

merged = merge(merged, teams_data[ , c("nameTeam", "idConference", "idDivision")], by.x="nameTeam", by.y="nameTeam")

merged$idConference <- as.factor(merged$idConference)

merged$idDivision <- as.factor(merged$idDivision)

merged$logsalary <- log(merged$Salary)

#ggpairs(merged, columns = c("logsalary","treb", "blk", "fgm", "fga",  "winrate", "minutes", "pts", "ast", "countSeasons" ))

ggplot() + 
  geom_point(data=merged, aes(x=ast, y=pts, colour=Salary)) + 
  theme(legend.position="right") + 
  scale_colour_gradient(low="#19E719", high="#E71919")

#
ggplot() + 
  geom_point(data=merged, aes(x=countSeasons, y=minutes, colour=log(Salary) )) + 
  theme(legend.position="right") + 
  scale_colour_gradient(low="#19E719", high="#E71919")

lm1<-lm(log(Salary) ~ minutes + countSeasons  
         data=merged, x=TRUE, y=TRUE)x
summary(lm1)


