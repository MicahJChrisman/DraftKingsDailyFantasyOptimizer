setwd(dir = "D:/Coding Projects/Draft Kings")
source("D:/Coding Projects/Draft Kings/Optimization.R")

NBA = function(playerData){

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
  PG <- is_pos("PG")
  SG <- is_pos("SG")
  SF <- is_pos("SF")
  PF <- is_pos("PF")
  C <- is_pos("C")
  
  
  model %>%
    
    #starting positions
    add_constraint(sum_expr(colwise(PG(i)) * x[i], i = 1:n) >=1) %>%
    add_constraint(sum_expr(colwise(SG(i)) * x[i], i = 1:n) >=1) %>%
    add_constraint(sum_expr(colwise(SF(i)) * x[i], i = 1:n) >=1) %>%
    add_constraint(sum_expr(colwise(PF(i)) * x[i], i = 1:n) >=1) %>%
    add_constraint(sum_expr(colwise(C(i)) * x[i], i = 1:n) >=1) %>%
    
    #guards 
    add_constraint(sum_expr(colwise(SG(i) +PG(i)) * x[i], i = 1:n) >=2) %>%
    
    #forwards
    add_constraint(sum_expr(colwise(SF(i) + PF(i)) * x[i], i = 1:n) >=2)
}


