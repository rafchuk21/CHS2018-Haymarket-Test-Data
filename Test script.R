sheet = Haymarket_Week_1_Data
id = "Team Number"
matchCount = "Number of Matches"
stats = c("Crossed Line", "A switch success", "A scale success", "Red Switch Delivered", "Blue Switch Delivered", "Scale Delivered", "Vault Cubes")

#Pull wanted data from sheet
pullData <- function() {
  dataMatrix <- data.matrix(sheet[c(id, stats)])
  return (dataMatrix)
}

#Merge match data for each team
mergeData <- function() {
  allData = pullData()
  sortedData = allData[order(allData[,id]),]
  mergedData = data.matrix(c(sortedData[1,id], 1, sortedData[1,stats]))
  print(mergedData)
  mergedData <- t(mergedData)
  colnames(mergedData) <- c(id, matchCount, stats)
  print(mergedData)
  i <- 2
  for (entryIndex in 1:length(sortedData[,id])) {
    alreadyMerged = FALSE
    for (existingEntryIndex in 1:length(mergedData[,id])) {
      print(existingEntryIndex)
      if (!alreadyMerged & mergedData[existingEntryIndex,id] == sortedData[entryIndex, id]) {
        mergedData[existingEntryIndex,matchCount] = mergedData[existingEntryIndex, matchCount] + 1
        for (statistic in stats) {
          mergedData[existingEntryIndex,statistic] = mergedData[existingEntryIndex, statistic] + sortedData[entryIndex, statistic]
        }
        alreadyMerged = TRUE
      }
    }
    if (!alreadyMerged) {
      print("never merged")
      mergedData <- rbind(mergedData, c(sortedData[entryIndex,id],1, sortedData[entryIndex,stats]))
    }
  }
  return(mergedData)
}
