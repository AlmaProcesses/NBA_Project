# NBA Project Proposal

## Domain of interest

Sports are one of the best data providers to perform awesome analysis. Despite football (also named soccer in the USA) is the king sport in Spain, we think the sport which fits better for out project is basketball, because its usage of statistics in a standarized way and the huge ammount of different measures available.

Despite Spain won the last FIBA World Cup, some of our top players ([including the MVP](https://www.marca.com/en/more-sports/2019/09/15/5d7e56b346163f20308b4621.html)) are based in the best league in the world, the NBA.

The NBA is not only the best basketball league in the world, it also follows a public data policy which allows us to gather data related with almost any aspect of the league, from salaries to historical draft data. Moreover, the NBA regular season has 82 games per team which give us a total of 1230 games with at least 16 different players playing each match. Almost all the data related with the game itself is available via an open official API (the API is not documented anyways).


Because there's so many data available, we have chosen to focus our project on different NBA datasets for the **2018-2019 season**.

### Other data drive projects related with our domain

There are a lot of different projects related with the NBA perfomed by sports journalists, the NBA, tipsters (people who gives betting picks)...

We selected the following 3 projects:

- [Goodbye, Mid-Range Shot](https://flowingdata.com/2019/01/15/goodbye-mid-range-shot/): This project states how Golden State Warriors, commanded by Steve Kerr, shifted from Mid-Range shots to 3-pts shots and how all the league is moving with them.
- [Using Data Science to Predict the Next NBA MVP](https://towardsdatascience.com/using-data-science-to-predict-the-next-nba-mvp-30526e0443da): This project models the data more correlated with the MVP award given each year by over one hundred sports media members. For 2019 it failed and gave James Harden as the most probable pick for MVP award with 25.4% of chances, but also gave 24.0% to Giannis Antetokounmpo, the actual [winner of the 2019 MVP Award](https://www.nba.com/article/2019/04/12/kia-mvp-ladder-final-edition-2018-19-season?collection=mvp-ladder).
- [Using Machine Learning to Find the 8 Types of Players in the NBA](https://fastbreakdata.com/classifying-the-modern-nba-player-with-machine-learning-539da03bb824): This project uses machine learning to classify the NBA players on 8 different types and not the classic 5 ones, using [K-Means clustering](https://towardsdatascience.com/understanding-k-means-clustering-in-machine-learning-6a6e67336aa1).

### Data-Driven Questions

 1. Are the Draft Position and the Player Performance correlated?
 2. Do exist differences between East and West Conferences match performance?  
 3. Do the salaries have a direct relationship with the player statistics? Is there any pattern?

## Finding Data

### Draft Dataset

This dataset contains information about the 2018 Draft players Team in the NBA.

It have been obtained from the official [NBA API](https://stats.nba.com/) using the R package [nbastatR](http://asbcllc.com/nbastatR/).

This dataset consists of one csv file. It's named [draft.csv](../master/data/draft.csv), and it is a list of the teams of the draft players sorted by the pick overall number and it shows the destination teams. It has 60 observations and 15 features.

| Column | Type | Description |
| :------| :----| :-----------|
| `yearDraft` | num | The year when the player was drafted |
| `numberPickOverral` | num | The order of the pick |
| `numberRound` | num | The round of the pick |
| `numberRoundPick` | num | The order of the pick in its round |
| `namePlayer` | chr | The complete name of the player |
| `slugTeam` | chr | The abbreviation of the team which did the pick |
| `nameOrganizationFrom` | chr | Name of the college or team the player comes from |
| `typeOrganizationFrom` | chr | Type of organization the player comes from |
| `idPlayer` | num | The id of the player on the NBA API |
| `idTeam` | num | The id of the team on the NBA API |
| `nameTeam` | chr | The complete name of the team |
| `cityTeam` | chr | The city where the team is based (for GSW is not Oakland nor San Francisco) |
| `teamName` | chr | The name of the team |
| `slugOrganizationTypeFrom` | chr | The abbreviation of `typeOrganizationFrom`|
| `locationOrganizationFrom` | chr | The country of the origin organization if this is outside the USA |

This dataset can answer the first question. 

### Match Statistics Dataset

This dataset contains statistics about the NBA games during the 2018-19 season.

It have been obtained from [nbastatR](http://asbcllc.com/nbastatR/reference/index.html "nBastaR").

This dataset consists of one csv file. It's named [regularseason1819_games.csv](../master/data/regularseason1819_games.csv). It has 2460 observations and 47 features.

| Column | Type | Description |
| :------| :----| :-----------|
| `yearSeason` | num | The year when the statistics were taken |
| `slugSeason`| num | The season of the match |
| `slugLeague`| chr | League name abbreviation|
| `typeSeason`| chr | Type of season (always `Regular Season`) |
| `dateGame`| date | Date the match was played |
| `idGame`| num | Id of the match on the NBA API|
| `numberGameTeamSeason`| num | Match order for the team |
| `nameTeam`| num | Name of the team |
| `idTeam`| num | Id of the team on the NBA API|
| `isB2B`| logi | The match was a back to back |
| `isB2BFirst`| logi | The match was the first back to back |
| `isB2BSecond`| logi | The match was the second back to back |
| `locationGame`| chr | Location where the match was played |
| `slugMatchup`| chr | Abbreviation of the match |
| `slugTeam`| chr | Abbreviation of the team |
| `countDaysRestTeam`| num | Days the team had for resting before the match |
| `countDaysNextGameTeam`| num | Days before the next team's match |
| `slugOpponent`| chr | Abbreviation of the opponent team |
| `slugTeamWinner`| chr | Abbreviation of the winner team |
| `slugTeamLoser`| chr | Abbreviation of the defeated team |
| `outcomeGame`| chr | The abbreviation of the result (`W|L`) |
| `isWin`| logi | True if the team won the match |
| `fgmTeam`| num | Field goals made by the team |
| `fgaTeam`| num | Field goals attempted by the team |
| `pctFGTeam`| num | % of field goals made vs attempted |
| `fg3mTeam`| num | 3-pts field goals made by the team |
| `fg3aTeam`| num | 3-pts field goals attempted by the team |
| `pctFG3Team`| num | % of 3-pts field goals made vs attempted |
| `pctFTTeam`| num | % of free throws scored |
| `hasVideo`| logi |  |
| `fg2mTeam`| num | 2-pts field goals made by the team |
| `fg2aTeam`| num | 2-pts field goals attempted by the team |
| `pctFG2Team`| num | % of 2-pts field goals attempted by the team |
| `minutesTeam`| num | Minutes played by the team|
| `ftmTeam`| num | Free throws made by the team |
| `ftaTeam`| num | Free throws attempted by the team |
| `orebTeam`| num | Offensive rebounds of the team |
| `drebTeam`| num | Defensive rebounds of the team |
| `trebTeam`| num | Total rebounds of the team |
| `astTeam`| num | Number of assists of the team |
| `stlTeam`| num | Number of steals of the team |
| `blkTeam`| num | Number of blocks of the team |
| `tovTeam`| num | Number of turnovers of the team |
| `pfTeam`| num | Number of personal fouls of the team |
| `ptsTeam`| num | Points scored by the team |
| `plusminusTeam`| num | Difference of points scored with the other team |
| `urlTeamSeasonLogo`| chr | Url with the logo of the team |

This dataset can answer the second and third questions. 

### Salaries Dataset

This dataset contains information about the salaries of the players of each Team in the NBA.

It have been obtained from [Hoopshype.com](https://hoopshype.com/salaries/2018-2019/ "Hoopshype page for 2018 - 2019 salaries") and extracted using a web scraping [script](../master/utils/player_salary_scrapper.R) which recolects the data from the page of the salaries for the 2018-2019 season.

This dataset consists of two csv files. The first one, [salaries_team.csv](../master/data/salaries_team.csv), is a list of the teams of the NBA and the sum of their players' salaries. It has 30 observations and 4 features.

| Column | Type | Description |
| :------| :----| :-----------|
| `nameTeam` | chr | Name of the team |
| `idTeam` | num | Id of the team |
| `slugTeam` | chr | The abbreviation of `nameTeam` |
| `salaries` | num | Sum of the salaries of the players of the team |

The second one, [salaries_player.csv](../master/data/salaries_player.csv), is a list of the players of all the teams of the NBA and his salaries. It has 621 observations and 3 features.

| Column | Type | Description |
| :------| :----| :-----------|
| `Player` | chr | Name of the player |
| `Salary` | num | Player's salary |
| `Team` | chr |  Name of the team where the player plays |

This dataset can answer the third question. 
