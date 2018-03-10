library(readxl)
library(ggplot2)
library(reshape2)
library(stringr)

if (!exists("originalEventDataSheet")) {
  originalEventDataSheet <- read_excel("~/R/CHS2018-Haymarket-Test-Data/Haymarket Week 1 Data.xlsx")
}

AND <- function(...) {
  conditions = list(...)
  ret <- TRUE
  for (i in conditions) {
    ret <- ret & i
  }
  return(ret)
}

cleanData <- function(dataSet = originalEventDataSheet, relevantData) {
  ##print(relevantData)
  ret = dataSet[,relevantData]
  ret <- cleanNA(ret)
  #colnames(ret) <- gsub(x = colnames(ret), pattern = " ", replacement = "")
  return(ret)
}

#if a row has a "1" in any of its dataExclusionConditions columns, it is removed from the dataset
excludeRows <- function(dataSet, dataExclusionConditions) {
  ret = dataSet
  i <- 1
  while(i <= nrow(ret)) {
    for (condition in dataExclusionConditions) {
      if (ret[i,condition] > 0) {
        #print("cond true")
        ret <- ret[-1*i,]
        i <- i-1
        break
      }
    }
    i <- i+1
  }
  return(ret)
}

cleanNA <- function(inputData) {
  data <- inputData
  for (i in 1:length(data)) {
    for (j in 1:length(data[[i]])) {
      if (is.na(data[[i]][j])) {
        #print(noquote(paste("W: NA at ", as.character(i), ", ", as.character(j), " set to 0.", sep = "")))
        data[[i]][j] <- 0
      }
    }
  }
  return(data)
}

sortData <- function(dataSet, sortingCriteria, ascending = TRUE) {
  #sortingCriteria <- gsub(x = sortingCriteria, pattern = " ", replacement = "")
  return(dataSet[do.call("order", c(dataSet[sortingCriteria], decreasing = !ascending)),])
}

mainDataFilter <- function(data = originalEventDataSheet, id = "Team Number", relevant = relevantStatistics,
                           sorting = c("Team Number", "Match Number"), exclusion = c("No Show", "Dead"),
                           asc = TRUE) {
  
  relevant = unique(c(relevant, sorting, exclusion))
  
  ret <- data #start
  ret <- cleanNA(ret) #set NA to 0
  ret <- cleanData(dataSet = ret, relevantData = relevant) #only keep relevant columns
  ret <- sortData(dataSet = ret, sortingCriteria = sorting, ascending = asc) #sort data by given columns (Team Number, Match Number)
  #ret <- excludeRows(dataSet = ret, dataExclusionConditions = exclusion) #excludes rows with given conditions (No show, Dead, etc)
  #ret <- mergeRows(dataSet = ret, identifier = id, aggregateMethod)
  return(ret)
}

returnInput <- function(input) {
  return(input)
}

quartile <- function(dataSet, numQuartile = 3) {
  if (numQuartile == 0) {
    return(min(dataSet))
  } else if (numQuartile == 1) {
    return(summary(dataSet)[2])
  } else if (numQuartile == 2) {
    return(median(dataSet))
  } else if (numQuartile == 3) {
    return(summary(dataSet)[5])
  } else if (numQuartile == 4) {
    return(max(dataSet))
  }
}

numListToString <- function(dataSet) {
  for (i in 1:length(dataSet)) {
    dataSet[i] <- toString(dataSet[i])
    while(nchar(dataSet[i]) < 4) {
      dataSet[i] <- paste("0", dataSet[i], sep = "")
    }
  }
  return (dataSet)
}

sumColumns <- function(dataSet, columnsToSum, newColName) {
  newCol <- dataSet[,columnsToSum[1]]
  for (i in 2:length(columnsToSum)) {
    newCol <- newCol + dataSet[,columnsToSum[i]]
  }
  dataSet <- cbind(dataSet, newCol)
  colnames(dataSet) <- setdiff(c(colnames(dataSet), newColName), "newCol")
  return(dataSet)
}

sumManyColumns <- function(dataSet, columnsToSumSet, newColNameSet) {
  #print(columnsToSumSet)
  #print(newColNameSet)
  if (ncol(columnsToSumSet) != 2) {
    columnsToSumSet <- t(columnsToSumSet)
  }
  
  if (nrow(columnsToSumSet) != length(newColNameSet)) {
    stop("inconsistent input lengths")
  }

  for (i in 1:nrow(columnsToSumSet)) {
    dataSet <- cbind(sumColumns(dataSet, columnsToSum = columnsToSumSet[i,], newColName = newColNameSet[i]))
  }
  return (dataSet)
}

dualBarGraph <- function(dataSet, idColumn = "Team Number", upColumns, downColumns, sortColumns, xlabel = idColumn,
                         ylabel = "# Cubes", title, ylimits = c(min(-1*downColumns), max(upColumns)),
                         xlimits = c(min(dataSet[,idColumn]), max(dataSet[,idColumn])),
                         legendText = c(upColumns, downColumns), yticks = ybounds[1]:ybounds[2]) {
  basePlot <- ggplot(cdt1, aes(x = factor(numListToString(dataSet[,idColumn]),
                                          levels = numListToString[dataSet[, idColumn]])))
  dataSet[,downColumns] = -1 * dataSet[,downColumns]
  filteredDataSet <- mainDataFilter(data = dataSet, relevant = unique(c(idColumn, upColumns, downColumns, sortColumns)),
                                    sorting = sortColumns, asc = FALSE)
  #print(filteredDataSet)
  filteredDataSet[, idColumn] <- numListToString(filteredDataSet[, idColumn])
  meltedFilteredData <- melt(filteredDataSet, id.vars = idColumn)
  
}