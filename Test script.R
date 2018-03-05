sheet = Haymarket_Week_1_Data
id = "Team Number"
matchCount = "Number of Matches"
stats = c("Crossed Line", "A switch success", "A scale success", "Red Switch Delivered", "Blue Switch Delivered", "Scale Delivered", "Vault Cubes", "Scale Dropped")

#Pull wanted data from sheet
pullData <- function() {
  dataMatrix <- data.matrix(cleanNA(sheet[c(id, stats)]))
  return (dataMatrix)
}

#Takes a 2d tibble and cleans up NA datapoints and sets them to 0
cleanNA <- function(inputData) {
  data <- inputData
  for (i in 1:length(data)) {
    for (j in 1:length(data[[i]])) {
      if (is.na(data[[i]][j])) {
        print(noquote(paste("W: NA at ", as.character(i), ", ", as.character(j), " set to 0.", sep = "")))
        data[[i]][j] <- 0
      }
    }
  }
  return(data)
}

#Merge match data for each team
mergeData <- function() {
  allData = pullData()
  sortedData = allData[order(allData[,id]),] #sort data by team numbers
  mergedData = data.matrix(c(sortedData[1,id], 1, sortedData[1,stats])) #adds a column for number of matches recorded, initialized at 1
  mergedData <- t(mergedData) #transposes the matrix
  colnames(mergedData) <- c(id, matchCount, stats) #sets names for all the columns (specifically the matchCount one)
  for (entryIndex in 1:length(sortedData[,id])) { #for each line of data
    alreadyMerged = FALSE #initialize variable, which represents whether the merged matrix already has the team or not
    for (existingEntryIndex in 1:length(mergedData[,id])) { #for every entry alread merged
      if (!alreadyMerged & mergedData[existingEntryIndex,id] == sortedData[entryIndex, id]) { #if the data has not already been merged, and if the new data entry matches the team number of an existing entry
        mergedData[existingEntryIndex,matchCount] = mergedData[existingEntryIndex, matchCount] + 1 #increment number of matches
        for (statistic in stats) { #for every statistic that's being recorded
          mergedData[existingEntryIndex,statistic] = mergedData[existingEntryIndex, statistic] + sortedData[entryIndex, statistic] #add the values from the new match to the existing merged information
        }
        alreadyMerged = TRUE #state that the new row was merged into the data
      }
    }
    if (!alreadyMerged) { #if the row was never merged (the team is not yet in the merged dataset)
      mergedData <- rbind(mergedData, c(sortedData[entryIndex,id],1, sortedData[entryIndex,stats])) #add the new row to the dataset
    }
  }
  return(mergedData)
}

#Averages the values per match. averageValue would be the number of matches, and dataRange is a list containing the names of the columns to average.
averageData <- function(dataMatrix, averageValue = matchCount, dataRange = stats) {
  temp <- dataMatrix
  for (i in 1:length(temp[,1])) {
    for (j in dataRange) {
      temp[i,j] = temp[i,j] / dataMatrix[i, averageValue]
    }
  }
  return(temp)
}

#Gets a data column and sorts teams based on their performance in that column
sortTeams<- function(dataMatrix, dataColumn, sortingCriterion = dataColumn, ascending = FALSE) {
  relevantData <- cbind(dataMatrix[,id], dataMatrix[,dataColumn])
  colnames(relevantData) <- c(id, dataColumn)
  relevantData <- relevantData[order(relevantData[,sortingCriterion], decreasing = !ascending),]
  return(relevantData)
}

#Sums columns and adds the result to the input matrix
sumColumns <- function(dataMatrix, sumColumns, sumColumnName) {
  newCol = dataMatrix[,sumColumns[1]]
  for (i in 2:length(sumColumns)) {
    newCol = newCol + dataMatrix[, sumColumns[i]]
  }
  returnMatrix <- dataMatrix
  returnMatrix <- cbind(dataMatrix, newCol)
  colnames(returnMatrix) <- setdiff(c(colnames(returnMatrix), sumColumnName), "newCol")
  return(returnMatrix)
}

#Make a bar graph with bars going up and down
dualBarGraph <- function(dataMatrix, upData, downData, title = c(upData, downData), xlabel = "Team Number",
                         ylabel, ybounds = c(min(negatedData), max(dataMatrix[,upData])),
                         xnames = dataMatrix[,id], colorsUp, colorsDown, legendText, legendLocation = "topright",
                         yticks = ybounds[1]:ybounds[2]) {
  negatedData <- -1*dataMatrix[,downData]
  barplot(t(dataMatrix[,upData]),
          main=title, xlab = xlabel, ylab = ylabel, names.arg = xnames, col = colorsUp,
          las = 2, yaxt = "n", ylim = ybounds, beside=FALSE)
  barplot(t(negatedData), add=T, col=colorsDown, yaxt = "n")
  axis(side=2, at=yticks)
  legend(legendLocation, legend = legendText, fill = c(colorsUp, colorsDown))
}

dualBarGraph(scaleSwitchData, c("A scale success", "Scale Delivered"), c("A switch success", "Switch Delivered"),
             ylabel = "# of Cubes", colorsUp = c("goldenrod1", "red"), colorsDown = c("cyan", "blue"),
             legendText = c("Autonomous Scale", "Teleop Scale", "Autonomous Switch", "Teleop Switch"),
             ybounds = c(-5,5), title = "Overall Average Cubes Delivered to Switch and Scale per Team")