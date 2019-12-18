library(knitr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(devtools)
library(rvest)
library(patchwork)

library(caret)
set.seed(2403)

salaries_players <- read.csv('data/salaries_player.csv', stringsAsFactors = F) %>%
  group_by(Player) %>%
  summarise(Salary = as.integer(mean(Salary)))
info_players <- read.csv('data/players.csv', stringsAsFactors = F)
stats_players <- read.csv('data/logs_players.csv', stringsAsFactors = F)

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

merged_player_tables <- info_players %>%
  filter(yearSeasonFirst < 2019) %>%
  left_join(salaries_players, by = c('namePlayer' = 'Player')) %>%
  select(namePlayer, countSeasons, Salary) %>%
  filter(!is.na(Salary))

player_seasons <- info_players %>% select(namePlayer, countSeasons)

ggplot(merged_player_tables, aes(x = countSeasons, y = sqrt(Salary))) +
  geom_point() +
  stat_summary(aes(y = sqrt(Salary), group = 1), fun.y = mean, colour = 'red', geom = 'line', group = 1)

reduced_aggr_data <- stats_aggr %>%
  select(namePlayer, minutes, pts, ast, treb, blk, stl) %>%
  left_join(salaries_players, by = c('namePlayer' = 'Player')) %>%
  left_join(player_seasons) %>%
  filter(!is.na(countSeasons)) %>%
  mutate(sqrt_salary = sqrt(Salary))

logistic_model <- glm(sqrt_salary ~ minutes + pts + ast + treb + blk + stl, data = reduced_aggr_data)
summary(logistic_model)
plot(logistic_model)

trainIndex <- createDataPartition(reduced_aggr_data$sqrt_salary, p = .85, list = FALSE, times = 1)

training_set <- reduced_aggr_data[ trainIndex, ]
test_set <- reduced_aggr_data[ -trainIndex, ]

basic_fit <- train(sqrt_salary ~ ., data = reduced_aggr_data, method = "knn")
basic_preds <- predict(basic_fit, test_set)

test_predicted <- test_set %>% mutate(predictions = basic_preds)

mae_value <- MAE(basic_preds, test_set$sqrt_salary)