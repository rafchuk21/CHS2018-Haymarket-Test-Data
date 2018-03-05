sheet = Haymarket_Week_1_Data
id = "Team Number"
matchCount = "Number of Matches"
stats = c("Crossed Line", "A switch success", "A scale success", "Red Switch Delivered", "Blue Switch Delivered", "Scale Delivered", "Vault Cubes")

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
averageData <- function(dataMatrix, averageValue, dataRange) {
  temp <- dataMatrix
  for (i in 1:length(temp[,1])) {
    for (j in dataRange) {
      temp[i,j] = temp[i,j] / dataMatrix[i, averageValue]
    }
  }
  return(temp)
}