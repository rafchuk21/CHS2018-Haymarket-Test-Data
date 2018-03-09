source('~/R/CHS2018-Haymarket-Test-Data/ggplot Test Script.R')
relevantStatistics = c("Team Number", "Crossed Line", "A scale success", "A switch success",
                       "Teleop Switch", "Vault Cubes", "Total Scale", "Total Switch", "Total Balance",
                       "Total Cubes")
originalDataWithNewColumns <- sumManyColumns(originalEventDataSheet, cbind(c("A scale success", "Red Switch Delivered",
                                                                             "A switch success", "Total Scale",
                                                                             "Vault Cubes"),
                                                                           c("Scale Delivered", "Blue Switch Delivered",
                                                                             "Teleop Switch", "Total Switch",
                                                                             "Total Balance")),
                                             c("Total Scale", "Teleop Switch", "Total Switch",
                                               "Total Balance", "Total Cubes"))