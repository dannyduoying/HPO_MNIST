### R code from vignette source 'shortintro.Rnw'

###################################################
### code chunk number 1: check
###################################################
RNGkind()


###################################################
### code chunk number 2: example1
###################################################
RNGkind()
library(randtoolbox)
set.generator("MersenneTwister", initialization="init2002", resolution=53, seed=1)
str(get.description())
RNGkind()
runif(10)


###################################################
### code chunk number 3: undo
###################################################
set.generator("default")
RNGkind()


###################################################
### code chunk number 4: example1
###################################################
setSeed(1)
congruRand(10, mod = 2^31-1, mult = 16807, incr = 0)


