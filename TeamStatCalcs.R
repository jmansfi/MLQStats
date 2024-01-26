library(data.table)

game <- fread("C:/Users/Fam/Downloads/Stats DB Tables - GAME.csv")
teamstats <- fread("C:/Users/Fam/Downloads/teamstats.csv")
team <- fread("C:/Users/Fam/Downloads/Stats DB Tables - TEAM.csv")
team[City == "New York City", City := "New York"]

teams <- game[, c("GameID", "HomeTeam", "AwayTeam")]
teamstatsfull <- merge(teamstats, teams, by = "GameID")

setnames(teamstatsfull, c("Goal Allowed"), c("GoalAllowed"))

teamstatsfull[TeamID == HomeTeam, Opp := AwayTeam]
teamstatsfull[TeamID == AwayTeam, Opp := HomeTeam]

teamstatsfull[, HomeTeam := NULL]
teamstatsfull[, AwayTeam := NULL]

teamstatsfull[is.na(GoalAllowed), GoalAllowed := 0]
teamstatsfull[GoalAllowed == "", GoalAllowed := "0"]
teamstatsfull[GoalAllowed == "\" \"", GoalAllowed := "0"]
teamstatsfull[, GoalAllowed := as.numeric(GoalAllowed)]

teamstatsag <- data.table(table(teamstatsfull$TeamID, teamstatsfull$Bludgers, teamstatsfull$GoalAllowed))
setnames(teamstatsag, c("V1", "V2", "V3", "N"), c("Team", "Dodgeballs", "Goal", "Count"))
goalperdef <- dcast(teamstatsag, Team + Goal ~ Dodgeballs, value.var = "Count")
goalsagainst <- goalperdef[Goal == 1]
goalsagainst[, Goal := NULL]
setnames(goalsagainst, c("0", "1", "2"), c("ScoredOnWZero", "ScoredOnWOne", "ScoredOnWTwo"))
stops <- goalperdef[Goal == 0]
stops[, Goal := NULL]
setnames(stops, c("0", "1", "2"), c("StopsWZero", "StopsWOne", "StopsWTwo"))
teamdef <- merge(goalsagainst, stops, by = "Team")

oppstatsag <- data.table(table(teamstatsfull$Opp, teamstatsfull$Bludgers, teamstatsfull$GoalAllowed))
setnames(oppstatsag, c("V1", "V2", "V3", "N"), c("Team", "Dodgeballs", "Goal", "Count"))
goalperoff <- dcast(oppstatsag, Team + Goal ~ Dodgeballs, value.var = "Count")
goals <- goalperoff[Goal == 1]
goals[, Goal := NULL]
setnames(goals, c("0", "1", "2"), c("ScoresAgainstZero", "ScoresAgainstOne", "ScoresAgainstTwo"))
stopsagainst <- goalperoff[Goal == 0]
stopsagainst[, Goal := NULL]
setnames(stopsagainst, c("0", "1", "2"), c("StopAgainstZero", "StopAgainstOne", "StopAgainstTwo"))

teamoff <- merge(goals, stopsagainst, by = "Team")

full <- merge(teamdef, teamoff, by = "Team")

full[, DefZero := round(StopsWZero/(ScoredOnWZero + StopsWZero)*100, 2)]
full[, DefOne := round(StopsWOne/(ScoredOnWOne + StopsWOne)*100, 2)]
full[, DefTwo := round(StopsWTwo/(ScoredOnWTwo + StopsWTwo)*100, 2)]
full[, OffZero := round(ScoresAgainstZero/(ScoresAgainstZero + StopAgainstZero)*100, 2)]
full[, OffOne := round(ScoresAgainstOne/(ScoresAgainstOne + StopAgainstOne)*100, 2)]
full[, OffTwo := round(ScoresAgainstTwo/(ScoresAgainstTwo + StopAgainstTwo)*100, 2)]
full[, DefTotal := round((StopsWZero + StopsWOne + StopsWTwo)/(StopsWZero + StopsWOne + StopsWTwo + ScoredOnWZero + ScoredOnWOne + ScoredOnWTwo)*100, 2)]
full[, OffTotal := round((ScoresAgainstZero + ScoresAgainstOne + ScoresAgainstTwo)/(ScoresAgainstZero + ScoresAgainstOne + ScoresAgainstTwo + StopAgainstZero + StopAgainstOne + StopAgainstTwo)*100, 2)]

team[, fullname := paste(City, Name)]
teammerge <- team[, c("TeamID", "fullname")]

teammelt <- full[, c("Team", "DefZero", "DefOne", "DefTwo", "OffZero", "OffOne", "OffTwo", "DefTotal", "OffTotal")]

output <- melt(teammelt, measure.vars=patterns(Offense = "^Off", Defense = "^Def"), value.factors = TRUE, id.vars = "Team")
setnames(output, "variable", "dodgeballs")
output[, dodgeballs := as.character(dodgeballs)]
output[dodgeballs == "1", dodgeballs := "0"]
output[dodgeballs == "2", dodgeballs := "1"]
output[dodgeballs == "3", dodgeballs := "2"]
output[dodgeballs == "4", dodgeballs := "Total"]

output <- merge(output, teammerge, by.x = "Team", by.y = "TeamID", all.x = TRUE, all.y = FALSE)

TeamGoalTotal <- teamstatsfull[!is.na(Goal), .(Goals = .N), by = TeamID]
TeamShotTotal <- teamstatsfull[, .(Shots = sum(Shots)), by = TeamID]

TeamGoalsShots <- merge(TeamGoalTotal, TeamShotTotal, by = "TeamID")
TeamGoalsShots[, shotperc := paste0(round(Goals/Shots, 3)*100, "%")]

TeamDriveTotal <- teamstatsfull[, .(Drives = .N), by = TeamID]
TeamControlTotal <- teamstatsfull[Bludgers == 2, .(Control = .N), by = TeamID]

TeamDriveControl <- merge(TeamDriveTotal, TeamControlTotal, by = "TeamID")
TeamDriveControl[, controlperc := paste0(round(Control/Drives, 3)*100, "%")]

TeamPercents <- merge(TeamGoalsShots, TeamDriveControl, by = "TeamID")

output <- merge(output, TeamPercents[,c("TeamID", "shotperc", "controlperc")], by.x = "Team", by.y = "TeamID", all.x = TRUE, all.y = FALSE)

fwrite(output, "C:/Users/Fam/Documents/R/teamstat.csv")