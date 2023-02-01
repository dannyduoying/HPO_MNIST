### Replicate EGO on Branin
library(DiceKriging)
library(DiceOptim)
library(dplyr)
library(ggplot2)
library(tictoc)

## Graph
rect_dat <- function(dat,color='black'){
  lower = apply(dat,2,min)
  upper = apply(dat,2,max)
  rect(xleft = lower[1],
       xright = upper[1],
       ybottom = lower[2],
       ytop = upper[2],
       border = color)
}

## Scale Design
scale_design <- function(design,lower,upper){
  unitcube <- TRUE
  
  if (any(apply(design, 2, min) <= 0)){
    unitcube <- FALSE
  }
  if (any(apply(design, 2, max) >= 1)){
    unitcube <- FALSE
  }
  
  scaled_design <- matrix(ncol = ncol(design),nrow = nrow(design))
  
  if(unitcube){
    # Mapping [0,1]^d to [lower,upper]
    for (i in 1:ncol(design)) {
      scaled_design[,i] <- (upper[i]-lower[i])*design[,i]+lower[i]
    }
  }
  else{
    # mapping level space (1,2,...,n) to [lower,upper]
    design <- (design-0.5)/nrow(design)
    for (i in 1:ncol(scaled_design)) {
      scaled_design[,i] <- (upper[i]-lower[i])*design[,i]+lower[i]
    }
  }
  return(scaled_design)
}

### Optimization Algorithm

## Input
# Initial Design
# Function
# Lower bound
# Upper bound
# Max Iteration
# Percent at each Iteration
# Method

## Output
# Added Points
# Location of Optimum
# Value of Optimum
# Total Runs

GO_Optim <- function(initial_design, FUN,
                     lower, upper, maxit = 5,
                     percent = 0.25, method = "TREGO",
                     minimize = TRUE) {
  
  d <- ncol(initial_design)
  design <- scale_design(initial_design, lower, upper) # scaling to current space
  output <- apply(design,1,FUN)
  
  dat <- cbind.data.frame(design,output)
  
  ## Initial Points Graph
  # points(dat[,1],dat[,2],pch=16,col='grey')
  
  ## Then fit the initial kriging model
  fitted_model <- DiceKriging::km(formula = ~1,
                                  design = design, 
                                  response = output,
                                  covtype = "matern5_2",
                                  control = list(trace=FALSE))
  
  check_l <- lower
  check_u <- upper
  
  tic.clearlog()
  
  ## Iterate for max iteration
  for (i in 1:maxit) {
    
    tic(i)
    
    if (method == "TREGO") {
      oGO <- TREGO.nsteps(model = fitted_model, fun = FUN,
                          nsteps = 5, 
                          lower = check_l, upper = check_u,
                          control = NULL,
                          kmcontrol = list(trace=FALSE))
    } else if (method == "EGO") {
      oGO <- fastEGO.nsteps(model = fitted_model, fun = FUN,
                            nsteps = 5, 
                            lower = check_l, upper = check_u,
                            control = list(trace=FALSE))
    }
    
    fitted_model <- oGO$lastmodel
    
    ## Graph
    # points(oGO$par[,1],oGO$par[,2],pch=16,col='blue')
    
    dat_temp <- cbind(oGO$par,oGO$value)
    colnames(dat_temp) <- colnames(dat)
    top_dat <- rbind(dat, dat_temp) %>%
      arrange(output) %>%
      head(max(ceiling((nrow(dat)+nrow(dat_temp)) * percent),3))
    
    dat <- top_dat
    additional_points[[i]] <- dat_temp
    
    diff <- apply(top_dat[,-ncol(top_dat)],2,max) - apply(top_dat[,-ncol(top_dat)],2,min)
    new_lower <- top_dat[1,-ncol(top_dat)] - diff/2
    new_upper <- top_dat[1,-ncol(top_dat)] + diff/2
    
    ## Shift if out of bound
    if (sum(new_lower < lower) > 0) {
      index <- which(new_lower < lower)
      new_lower[index] <- lower[index]
      new_upper[index] <- lower[index] + diff[index]
    }
    if (sum(new_upper > upper) > 0) {
      index <- which(new_upper > upper)
      new_upper[index] <- upper[index]
      new_lower[index] <- upper[index] - diff[index]
    }
    
    check_l <- c()
    check_u <- c()
    for (i in 1:d) {
      check_l <- c(check_l, new_lower[1,i])
      check_u <- c(check_u, new_upper[1,i])
    }
    # rect(xleft = check_l[1], xright = check_u[1], ybottom = check_l[2], ytop = check_u[2], border = "red")
    
    toc(log=TRUE,quiet=TRUE)
  }
  
  log.txt <- tic.log(format=TRUE)
  log.lst <- tic.log(format = FALSE)
  
  tic.clearlog()
  
  added_points <- do.call(rbind,additional_points)
  par <- dat[1,1:d]
  value <- dat[1,d+1]
  total_run <- nrow(added_points) + nrow(initial_design)
  
  return(list(added_points = added_points,
              par = par,
              value = value,
              total_run = total_run))
}
