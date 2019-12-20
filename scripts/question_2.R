library(dplyr)
library(ggplot2)
library(patchwork)

source("utils/question_2_func.R")

matches <- read.csv("data/regularseason1819_games.csv", stringsAsFactors = F)
teams <- read.csv("data/nba_teams.csv", stringsAsFactors = F)

teams <- teams %>% mutate(conference = sapply(idConference, conference_classifier))
matches <- matches %>% select(-urlTeamSeasonLogo) %>%
  rowwise() %>%
  mutate(conferenceMatch = type_match(teams, slugTeam, slugOpponent)) %>%
  ungroup()

match_conference_winner <- matches %>%
  group_by(idGame) %>% 
  summarise(winner = first(slugTeamWinner),
            loser = first(slugTeamLoser),
            conferenceMatch = first(conferenceMatch)) %>%
  rowwise() %>%
  mutate(conferenceWinner = teams[teams$slugTeam == winner, "conference"]) %>%
  ungroup()

match_team_winner <- match_conference_winner %>%
  group_by(winner) %>%
  tally() %>%
  rename(count = n) %>%
  ungroup()

match_team_conference_win <- match_conference_winner %>%
  group_by(winner) %>%
  count(conferenceMatch) %>%
  rename(count = n) %>%
  ungroup()

isSameConferenceMatch <- match_team_conference_win %>%
  rowwise() %>%
  mutate(isSame = if(conferenceMatch != "MIXED") "SAME" else "MIXED") %>%
  left_join(teams, by = c("winner" = "slugTeam")) %>%
  select(winner, conferenceMatch, count, isSame, conference)

g_east <- isSameConferenceMatch %>% filter(conference == "EAST") %>%
  ggplot(aes(fill = isSame, y = count, x = winner)) +
  geom_bar(position = "dodge", stat = "identity") +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  ylim(0, 45) +
  facet_wrap(~conference)
g_west <- isSameConferenceMatch %>% filter(conference == "WEST") %>%
  ggplot(aes(fill = isSame, y = count, x = winner)) +
  geom_bar(position = "dodge", stat = "identity") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  ylim(0, 45) +
  scale_fill_discrete(name = "Conference", labels = c("Opposite", "Same")) +
  facet_wrap(~conference)
patchwork <- g_east + g_west
patchwork + plot_annotation(
  title = "Wins per team grouped by conference during the Regular Season",
  theme = theme(plot.title = element_text(hjust = 0.45))
)