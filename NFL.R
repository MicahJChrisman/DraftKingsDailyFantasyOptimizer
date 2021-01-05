setwd(dir = "D:/Coding Projects/Draft Kings")
source("D:/Coding Projects/Draft Kings/Optimization.R")

NFL = function(playerData){
  
  totalSalary <- 50000
  rosterSize <- 9
  
  
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
  QB <- is_pos("QB")
  RB <- is_pos("RB")
  WR <- is_pos("WR")
  TE <- is_pos("TE")
  DST <- is_pos("DST")
  
  
  model %>%
    add_constraint(sum_expr(colwise(QB(i)) * x[i], i = 1:n) >=1) %>%
    add_constraint(sum_expr(colwise(RB(i)) * x[i], i = 1:n) >=2) %>%
    add_constraint(sum_expr(colwise(WR(i)) * x[i], i = 1:n) >=3) %>%
    add_constraint(sum_expr(colwise(TE(i)) * x[i], i = 1:n) >=1) %>%
    add_constraint(sum_expr(colwise(DST(i)) * x[i], i = 1:n) >=1) %>%

    add_constraint(sum_expr(colwise(RB(i) + WR(i) + TE(i)) * x[i], i = 1:n) >=7)
}


