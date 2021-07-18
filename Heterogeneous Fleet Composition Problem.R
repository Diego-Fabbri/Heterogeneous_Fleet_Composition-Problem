#Set your own working directory
setwd("C:/Users/diego/Documents/R/Projects/GitHub_Projects/Optimization/Heterogeneous Fleet Composition Problem")

# Import lpSolve package
library(lpSolve)

#Import required packages (ompr)
library(dplyr)
library(ROI)
library(ROI.plugin.symphony)
library(ompr)
library(ompr.roi)

#Set fixed cost 
cf <- c(350,351,352)

#Set variable cost 
cv <- c(150,151,152)

#Set hiring cost 
ch <- c(800,801,802)

#Set number of periods
N <- 52
n <- N

#Set number of vehicle classes
m <- 3

#Set number of required vehicles
v_ <- matrix(c(12,	15,	16,	17,	17,	18,	20,	20,	21,	22,	24,	22,	20,	18,	17,	16,	14,	13,	13,	14,	15,	16,	17,	19,	21,	22,	23,	22,	24,	26,	27,	28,	30,	32,	32,	30,	29,	28,	26,	25,	25,	24,	22,	22,	19,	20,	18,	17,	16,	16,	14,	13, 13,	16,	17,	18,	18,	19,	21,	21,	22,	23,	25,	23,	21,	19,	18,	17,	15,	14,	14,	15,	16,	17,	18,	20,	22,	23,	24,	23,	25,	27,	28,	29,	31,	33,	33,	31,	30,	29,	27,	26,	26,	25,	23,	23,	20,	21,	19,	18,	17,	17,	15,	14, 14,	17,	18,	19,	19,	20,	22,	22,	23,	24,	26,	24,	22,	20,	19,	18,	16,	15,	15,	16,	17,	18,	19,	21,	23,	24,	25,	24,	26,	28,	29,	30,	32,	34,	34,	32,	31,	30,	28,	27,	27,	26,	24,	24,	21,	22,	20,	19,	18,	18,	16,	15), ncol = m, byrow = FALSE)

#Get max value of v
v_max <- c(max(v_[,1]),max(v_[,2]),max(v_[,3]))
v_max

condition <- TRUE

for(k in 1:m){
  if(cf[k] + cv[k] >= ch[k]){
    print(paste("Cost condition does not hold for class: ", k))
    condition <- FALSE
  }
}




if(condition == TRUE) {
  print("Cost condition holds")
  
  #Build Model
  Model <- MIPModel() %>%
    add_variable(x[k, t], k= 1:m, t = 1:n, type = "binary") %>% #define variables
    add_variable(y[k, t], k= 1:m, t = 1:n, type = "integer", lb = 0, ub = v_max[k]) %>%
    add_variable(v[k], type = "integer", lb = 0, ub = v_max[k], k = 1:m) %>%
    set_objective(sum_expr(N*cf[k]*v[k], k = 1:m) +
                  sum_expr(cv[k]*v_[t, k]*x[k, t], t = 1:n, k = 1:m) +
                  sum_expr(cv[k]*v[k] -cv[k]*y[k, t], t = 1:n, k = 1:m) + 
                  sum_expr(ch[k]*v_[t, k] -ch[k]*v_[t, k]*x[k, t] -ch[k]*v[k] + ch[k]*y[k, t], t = 1:n, k = 1:m),
                  "min") %>% #define objective
    add_constraint(v[k] >= v_[t, k] -v_max[k] + v_max[k]*x[k, t], k= 1:m, t = 1:n) %>% #define constraints
    add_constraint(v[k] <= v_[t, k] +v_max[k]*x[k, t], k= 1:m, t = 1:n) %>%
    add_constraint(y[k, t] <= v_[t, k], k= 1:m, t = 1:n) %>%
    add_constraint(y[k, t] <= v_max[k]*x[k, t], k= 1:m, t = 1:n) %>%
    add_constraint(y[k, t] >= v[k] -v_max[k] + v_max[k]*x[k, t], k= 1:m, t = 1:n) %>%
    solve_model(with_ROI(solver = "symphony", verbosity = 1))
  
  #Model summary
  ##Status
  print(paste("Model status is:", Model$status))
  
  ##Objective Function
  print(paste("Objective value:", objective_value(Model)))
  
  #Variables
  ## X variables
    for (c in 1:m) {
      tmp <- get_solution(Model, v[k]) %>%
        filter(variable == "v", k == c) %>%
        select(value)
      print(paste("v[", c, "] = ", tmp))
      
    }
  
      
}else{
  print("Cost condition does not hold")
}
