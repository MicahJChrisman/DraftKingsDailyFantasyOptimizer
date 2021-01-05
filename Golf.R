setwd(dir = "D:/Coding Projects/Draft Kings")
source("D:/Coding Projects/Draft Kings/Optimization.R")

golf = function(playerData){

  
  totalSalary <- 50000
  rosterSize <- 6
  
  
  model <- modelGeneric(playerData, totalSalary, rosterSize)
  
  return(model)
}