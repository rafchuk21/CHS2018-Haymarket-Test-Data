library(readxl)
library(ggplot2)
library(reshape2)
library(stringr)

if (!exists("originalEventDataSheet")) {
  originalEventDataSheet <- read_excel("~/R/CHS2018-Haymarket-Test-Data/Haymarket Week 1 Data.xlsx")
}

cleanData <- function(dataSet = originalEventDataSheet, relevantData) {
  ret = dataSet[,relevantData]
  ret <- cleanNA(ret)
  colnames(ret) <- gsub(x = colnames(ret), pattern = " ", replacement = "")
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
  sortingCriteria <- gsub(x = sortingCriteria, pattern = " ", replacement = "")
  return(dataSet[do.call("order", c(dataSet[sortingCriteria], decreasing = !ascending)),])
}