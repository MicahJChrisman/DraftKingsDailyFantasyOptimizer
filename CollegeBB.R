setwd(dir = "D:/Coding Projects/Draft Kings")
source("D:/Coding Projects/Draft Kings/Optimization.R")

collegeBB = function(playerData){

  
  totalSalary <- 50000
  rosterSize <- 8
  
  
  model <- modelGeneric(playerData, totalSalary, rosterSize) %>%
    add_collegeBB_roster(playerData)
  
  return(model)
}

add_collegeBB_roster = function(model, playerData){
  n <- nrow(playerData)
  
  is_pos = function(pos){
    function(i){
      as.integer(pos == playerData$Pos[i])
    }
  }
  guards <- is_pos("G")
  forwards <- is_pos("F")
  guardForwards <- is_pos("G/F")
  
  model %>%
    add_constraint(sum_expr(colwise(forwards(i) + guardForwards(i)) * x[i], i = 1:n) >=3) %>%
    add_constraint(sum_expr(colwise(guards(i) + guardForwards(i)) * x[i], i = 1:n) >=3)
}


