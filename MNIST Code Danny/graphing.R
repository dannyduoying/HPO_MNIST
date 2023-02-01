### Replicate EGO on Branin
library(DiceKriging)
library(DiceOptim)
library(dplyr)
library(tidyr)
library(ggplot2)
source('algo.R')

## Graph
n.grid <- 20
x.grid <- y.grid <- seq(0,1,length=n.grid)
design.grid <- expand.grid(x.grid, y.grid)
response.grid <- apply(design.grid, 1, goldsteinPrice)
z.grid <- matrix(response.grid, n.grid, n.grid)
contour(x.grid, y.grid, z.grid, 40)
x1 = c(0.5, 0.25)
points(t(x1), pch=19, col="red")
title("Fonction de Goldstein price")

## Specify Function and Initial Design
#FUN <- DiceKriging::branin
#FUN1 <- DiceKriging::goldsteinPrice
#FUN <- DiceKriging::hartman3
#FUN <- DiceKriging::hartman6
FUN <- NN_optimize
reps <- 10
its <- 5
d <- 2
design_list <- replicate(reps, lhs::maximinLHS(n=10*d, k=d), 
                         simplify = FALSE)
lower <- c(0,0)
upper <- c(0.01,0.5)
nrun <- 5
additional_points <- list()
min <- 3
initial_run <- nrow(design_list[[1]])

grid_params <- crossing(
  percent = c(0.50),
  maxit = 1:its,
  design_number = 1:reps,
  method = c("EGO", "TREGO")) %>%
  as.data.frame()

iters <- nrow(grid_params)

## Original TREGO and EGO
df_TREGO <- data.frame()
df_EGO <- data.frame()

for (i in 1:its) {
  maxit <- i * nrun
  
  for (j in 1:reps) {
    design <- scale_design(design_list[[j]], lower, upper) # scaling to current space
    output <- apply(design,1,FUN)
    
    ## Using this fitted model for both TREGO and EGO
    fitted_model <- DiceKriging::km(formula = ~1,
                                    design = design, 
                                    response = output,
                                    covtype = "matern5_2",
                                    control = list(trace=FALSE))
    
    oTREGO <- TREGO.nsteps(model = fitted_model, fun = FUN,
                           nsteps = maxit, 
                           lower = lower, upper = upper,
                           control = NULL,
                           kmcontrol = list(trace=FALSE))
    df_TREGO[i,j] <- min(oTREGO$value)
    
    oEGO <- fastEGO.nsteps(model = fitted_model, fun = FUN,
                           nsteps = maxit, 
                           lower = lower, upper = upper,
                           control = list(trace=FALSE))
    df_EGO[i,j] <- min(oEGO$value)
    
    print(paste0("On iteration ", i, " repetition ", j))
  }
  
  print(paste0("Finished iteration ", i))
}

## Proposed Algorithm
df_optim <- data.frame()

for (i in 1:iters) {
  print(paste0("Proposed Algo on iteration ", i))
  
  outcome <- GO_Optim(initial_design = design_list[[grid_params[i,3]]], 
                      FUN = FUN, lower = lower, upper = upper, 
                      maxit = grid_params[i,2],
                      method = grid_params[i,4], percent = grid_params[i,1])
  
  df_optim[i,1] <- outcome$value
  df_optim[i,2] <- outcome$par[1]
  df_optim[i,3] <- outcome$par[2]
}

df_optim_2 <- cbind(grid_params, df_optim)

## Default
## Learning rate 0.005
## Dropout 0.25
df_default <- data.frame()
for (i in 1:10) {
  df_default[i,1] <- FUN(c(0.005, 0.25))
}
default <- -mean(df_default$V1)

## Graph
df_trego_final <- df_TREGO %>%
  mutate(Runs = (1:nrow(df_TREGO))*5 + initial_run,
         Value = (V1+V2+V3+V4+V5+V6+V7+V8+V9+V10)/10,
         Method = "TREGO",
         Percent = "Full") %>%
  select(Runs, Value, Method, Percent)
SD_trego <- apply(df_TREGO, 1, sd)
df_trego_final2 <- cbind(df_trego_final, SD_trego)
colnames(df_trego_final2) <- c("Runs", "Value", "Method",
                               "Percent", "SD")

df_ego_final <- df_EGO %>%
  mutate(Runs = (1:nrow(df_EGO))*5 + initial_run,
         Value = (V1+V2+V3+V4+V5+V6+V7+V8+V9+V10)/10,
         Method = "EGO",
         Percent = "Full") %>%
  select(Runs, Value, Method, Percent)
SD_ego <- apply(df_EGO, 1, sd)
df_ego_final2 <- cbind(df_ego_final, SD_ego)
colnames(df_ego_final2) <- c("Runs", "Value", "Method",
                               "Percent", "SD")

df_optim_final <- df_optim_2 %>%
  group_by(percent, maxit, method) %>%
  summarise(Value = mean(V1),
            SD = sd(V1)) %>%
  mutate(Runs = maxit*5 + initial_run,
         Method = method,
         Percent = as.character(percent)) %>%
  ungroup() %>%
  select(Runs, Value, Method, Percent, SD)

### Comparing our algo with EGO and TREGO
rbind(df_trego_final2,df_ego_final2,df_optim_final) %>%
  ggplot(aes(x = as.character(Runs), y = -Value, group= Percent,colour= Percent)) +
  geom_point() + 
  geom_line() +
  geom_errorbar(aes(ymin = -Value, ymax = -Value),
                width = .1) +
  ylim(c(0.975,0.985)) +
  theme_bw() +
  labs(x = "Runs", y = "Accuracy") +
  facet_wrap(vars(Method), ncol = 2) +
  labs(title = "HPO for MNIST using Learning Rate and Dropout") +
  geom_hline(yintercept = default, linetype = "dashed")
  
### Comparing with error bars
### Question, sd or se?
  rbind(df_trego_final2,df_ego_final2,df_optim_final) %>%
    ggplot(aes(x = as.character(Runs), y = -Value, group= Percent,colour= Percent)) +
    geom_point() + 
    geom_line() +
    geom_errorbar(aes(ymin = -Value-SD, ymax = -Value+SD),
                  width = .1) +
    ylim(c(0.975,0.985)) +
    theme_bw() +
    labs(x = "Runs", y = "Accuracy") +
    facet_wrap(vars(Method), ncol = 2) +
    labs(title = "HPO for MNIST using Learning Rate and Dropout") +
  geom_hline(yintercept = default, linetype = "dashed")
  
### Graphing the accuracy between final outcome of learning rate and dropout
df_optim %>%
  mutate(acc = round(V1, digits = 3)) %>%
  ggplot(aes(x = `1`, y = `2`, group = acc, colour = acc)) +
  geom_point() +
  theme_bw() +
  ylim(c(0, 0.5)) +
  xlim(c(0, 0.01)) +
  labs(title = "MNIST accuracy from Final Learning Rate and Dropout", 
       x = "Learning Rate",
       y = "Dropout")

### Graphing the iterations between final outcome of learning rate and dropout
df_optim_2 %>%
  ggplot(aes(x = `1`, y = `2`, group = as.factor(maxit), 
             colour = as.factor(maxit), shape = as.factor(maxit))) +
  geom_point() +
  theme_bw() +
  ylim(c(0, 0.5)) +
  xlim(c(0, 0.01)) +
  labs(title = "Learning Rate and Dropout at Different Iterations", 
       x = "Learning Rate",
       y = "Dropout")