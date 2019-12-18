conference_classifier <- function(conference_id) {
  if (conference_id == 1) "EAST" else "WEST"
}

type_match <- function(dataframe, localTeamSlug, awayTeamSlug) {
  localConf <- dataframe[dataframe$slugTeam == localTeamSlug, "conference"]
  awayConf <- dataframe[dataframe$slugTeam == awayTeamSlug, "conference"]
  if (awayConf == localConf) localConf else "MIXED"
}
