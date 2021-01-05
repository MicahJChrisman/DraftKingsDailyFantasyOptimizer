library(dplyr)
library(ROI)
library(ROI.plugin.glpk)
library(ompr)
library(ompr.roi)

setwd(dir = "D:/Coding Projects/Draft Kings")

playerData <- read.csv(file.path(paste0("C:/Users/mchri/Downloads/CBB DK Projections.csv")),
                       header = TRUE, sep=",", stringsAsFactors = FALSE)

modelGeneric = function(data, salary, rosterSize, type = c("binary", "continuous")){
  rowNum <- nrow(data)
  sal <- function(i){
    as.numeric(gsub(",","",data[["Salary"]][i]))
  }
  type <- match.arg(type)
  if(type == "binary"){
    baseMod <- MILPModel() %>%
      add_variable(x[i], i = 1:rowNum, type = "binary")
  }else{
    baseMod <- MILFModel() %>%
      add_variable(x[i], i = 1:rowNum, type = "continuous", lb = 0, ub = 0)
  }
  
  baseMod %>%
    add_salary(rowNum, sal, salary) %>%
    add_rosterSize(rowNum, rosterSize)
}

add_salary = function(model, n, sal, salary){
  add_constraint(model, sum_expr(colwise(sal(i)) * x[i], i = 1:n) <= salary)
}

add_rosterSize = function(model, n, rosterSize){
  add_constraint(model, sum_expr(x[i], i = 1:n) == rosterSize)
}

add_existing = function(model, roster){
  lengthRoster <- length(roster) - 1
  add_constraint(model, sum_expr(x[i], i = roster) <= lengthRoster)
}

modelOptimized = function(data, model, lineups = 1, solver = c("glpk", "symphony", "cbc")){
  numRows <- nrow(data)
  results <- vector("list", lineups)
  solver <- match.arg(solver)
  
  for(i in 1:lineups){
    result <- optim(data, model, solver)
    
    roster <- result$roster
    results[[i]] <- roster
    roster_rowID <- result$rosterID
    # rosterID <- roster$player_id
    model <- add_existing(model, roster_rowID)
    
  }
  
  return(results)
}

optim = function(data, model, solver = c("glpk", "symphony", "cbc")){
  numRows <- nrow(data)
  fantasyPoints <- function(i){
    data[["FPTs"]][i]
  }
  model <- set_generic_objective(model, numRows, fantasyPoints)
  solver <- match.arg(solver)
  result <- solve_model(model, with_ROI(solver = solver))
  type <- model$variables$x$type
  solution <- get_solution(result, x[i])
  
  if(type == "binary"){
    matches <- solution[solution[["value"]] == 1,]
    matches <- matches$i
    lineup <- tibble::as_tibble(data[matches,])
  }else{
    lineup <- data
    lineup[["x"]] <- solution[["value"]]
  }
  
  structure(
    list(
      result = result,
      roster = lineup,
      rosterID = matches
    ),
    class = "lineup"
  )
}

set_generic_objective <- function(model, numRows, fanPoints) {
  set_objective(model, sum_expr(colwise(fanPoints(i)) * x[i], i = 1:numRows))
}


