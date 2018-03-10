source('~/R/CHS2018-Haymarket-Test-Data/ggplot Test Script.R')

#get list of teams in data
teams = unique(originalEventDataSheet$"Team Number")
teams <- teams[order(teams)]

relevantStatistics = c("Team Number", "Crossed Line", "A scale success", "A scale 2 cubes", "A switch success",
                       "A switch 2 cube", "Scale Auto", "Switch Auto", "Teleop Switch", "Vault Cubes", "Total Scale",
                       "Total Switch", "Total Balance", "Total Cubes", "A Switch No attempt", "A scale no attempt")
exclusion = c("No Show", "Dead")

excludedMatches = teams
excludedMatches <- paste("Team", excludedMatches, sep = " ")

originalEventDataSheet <- cbind(originalEventDataSheet, "Scale Auto" = originalEventDataSheet$`A scale success`
                                +2*originalEventDataSheet$`A scale 2 cubes`)
originalEventDataSheet <- cbind(originalEventDataSheet, "Switch Auto" = originalEventDataSheet$`A switch success`
                                +2*originalEventDataSheet$`A switch 2 cube`)

# Total Scale = A scale success + Scale Delivered
# Teleop Switch = Red Switch Delivered + Blue Switch Delivered
# Total Switch = A switch success + Teleop Switch
# Total Balance = Total Scale + Total Switch
# Total Cubes = Total Balance + Vault Cubes

originalDataWithNewColumns <- sumManyColumns(originalEventDataSheet, cbind(c("Scale Auto", "Red Switch Delivered",
                                                                             "Switch Auto", "Total Scale",
                                                                             "Vault Cubes"),
                                                                           c("Scale Delivered", "Blue Switch Delivered",
                                                                             "Teleop Switch", "Total Switch",
                                                                             "Total Balance")),
                                             c("Total Scale", "Teleop Switch", "Total Switch",
                                               "Total Balance", "Total Cubes"))

#Remove irrelevant columns and remove unwanted rows.
cleanedData <- mainDataFilter(data = originalDataWithNewColumns, relevant = relevantStatistics)

mergeMethods <- subset(cleanedData, cleanedData[, "Team Number"] == 0)
mergeMethods[1,"Team Number"] <- "mean"
mergeMethods[1,"Crossed Line"] <- "mean"
mergeMethods[1,"A scale success"] <- "mean"
mergeMethods[1,"A switch success"] <- "mean"
mergeMethods[1,"A scale 2 cubes"] <- "mean"
mergeMethods[1,"A switch 2 cube"] <- "mean"
mergeMethods[1,"Scale Auto"] <- "mean"
mergeMethods[1,"Switch Auto"] <- "mean"
mergeMethods[1,"Teleop Switch"] <- "q3"
mergeMethods[1,"Vault Cubes"] <- "q3"
mergeMethods[1,"Total Scale"] <- "q3"
mergeMethods[1,"Total Switch"] <- "q3"
mergeMethods[1,"Total Balance"] <- "q3"
mergeMethods[1,"Total Cubes"] <- "q3"
mergeMethods[1,"A Switch No attempt"] <- "count0"
mergeMethods[1,"A scale no attempt"] <- "count0"
mergeMethods[1,"Match Number"] <- "count"
mergeMethods[1,"No Show"] <- "count1"
mergeMethods[1,"Dead"] <- "count1"

rowRemovedData = subset(cleanedData, cleanedData[, "Team Number"] == 0)
for (i in 1:length(teams)) {
  teamData <- subset(cleanedData, cleanedData[,"Team Number"] == teams[i])
  #print(teamData)
  #print(nrow(teamData))
  for (matchRow in 1:nrow(teamData)) {
    if (max(teamData[matchRow, exclusion]) == 1) {
      excludedMatches[i] <- paste(excludedMatches[i], teamData[matchRow, "Match Number"], sep = ", ")
    } else {
      rowRemovedData <- rbind(rowRemovedData, teamData[matchRow,])
    }
  }
}

mergedData <- subset(rowRemovedData, rowRemovedData[, "Team Number"] == 0)
for (i in 1:length(teams)) {
  teamData <- subset(rowRemovedData, rowRemovedData[,"Team Number"] == teams[i])
  mergedTeamRow <- subset(rowRemovedData, rowRemovedData[,"Team Number"] == 0)
  
  for(col in 1:ncol(mergedTeamRow)) {
    coln = colnames(mergedTeamRow)[col]
    if (mergeMethods[1,coln] == "mean") {
      mergedTeamRow[1,col] <- mean(teamData[,col])
    } else if (mergeMethods[1,coln] == "median") {
      mergedTeamRow[1,col] <- median(teamData[,col])
    } else if (mergeMethods[1,coln] == "sum") {
      mergedTeamRow[1,col] <- sum(teamData[,col])
    } else if (mergeMethods[1,coln] == "q3") {
      mergedTeamRow[1,col] <- summary(teamData[,col])[5]
    } else if (mergeMethods[1,coln] == "count") {
      mergedTeamRow[1,col] <- length(teamData[, col])
    } else if (mergeMethods[1,coln] == "count0") {
      mergedTeamRow[1,col] <- length(teamData[teamData[,col] == 1, col])
    } else if (mergeMethods[1,coln] == "count1") {
      mergedTeamRow[1,col] <- length(teamData[teamData[,col] == 1, col])
    } else {
      stop(paste("unknown merge method for col", coln))
    }
  }
  mergedData <- rbind(mergedData, mergedTeamRow)
}

mergedData <- cbind(mergedData, "Excluded Matches" = str_count(excludedMatches, ","))
mergedData <- cbind(mergedData, "Scale Auto when Attempted" = mergedData$`Scale Auto` * mergedData$`Match Number`
                    / (mergedData$`Match Number` - mergedData$`A scale no attempt`))
mergedData <- cbind(mergedData, "Switch Auto when Attempted" = mergedData$`Switch Auto` * mergedData$`Match Number`
                    / (mergedData$`Match Number` - mergedData$`A Switch No attempt`))