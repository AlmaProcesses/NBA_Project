library(dplyr)
library(ggplot2)

source("utils/question_2_func.R")

matches <- read.csv("data/regularseason1819_games.csv", stringsAsFactors = F)
teams <- read.csv("data/nba_teams.csv", stringsAsFactors = F)

teams <- teams %>% mutate(conference = sapply(idConference, conference_classifier))
matches <- matches %>% select(-urlTeamSeasonLogo) %>%
  rowwise() %>%
  mutate(conferenceMatch = type_match(teams, slugTeam, slugOpponent)) %>%
  ungroup()

match_conference_winner = matches %>% 
  group_by(idGame) %>% 
  summarise(winner = first(slugTeamWinner),
            loser = first(slugTeamLoser),
            conferenceMatch = first(conferenceMatch)) %>%
  rowwise() %>%
  mutate(conferenceWinner = teams[teams$slugTeam == winner, "conference"]) %>%
  ungroup()
