## Trying parallel computing

library(parallel)
library(doParallel)
source('mnist.R')
source('algo.R')

n_core <- detectCores()
registerDoParallel((n_core))

### Trying foreach
system.time({
  foreach(i=1:20) %dopar% {
    sqrt(i)
  }
})[3]


### Try MNIST with Parallel Compute
reps <- 10
d <- 2
lower <- c(0,0)
upper <- c(0.01,0.5)
FUN <- NN_optimize
design_list <- replicate(reps, lhs::maximinLHS(n=10*d, k=d), 
                         simplify = FALSE)

initial_design <- design_list[[1]]
design <- scale_design(initial_design, lower, upper) # scaling to current space
output <- apply(design,1,FUN)

### Parallel for initial design
foreach(i=1:20) %dopar% {
  design[i,]
}

