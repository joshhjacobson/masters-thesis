

## Sanity check: fit beta params and visualization with true uniform distribution.

library(fitdistrplus)
library(dplyr)


## 1. fit beta params to 1000 uniform samples of 5000 realizations each, 
## make box plots
set.seed(10)

sim_std_unif <- function(N) {
  udat <- runif(N, 0, 1)
  return(fitdist(udat,'beta')$estimate)
}

params1 <- data.frame(t(replicate(1000, sim_std_unif(5000))))
write.table(params1, file='data/params1.RData')
params1 <- read.table('data/params1.RData')

pdf("~/GitHub/random-fields/images/checks/beta_param_box1.pdf")
par(mfrow = c(1, 2))
boxplot(params1$shape1, main="shape1")
boxplot(params1$shape2, main="shape2")
dev.off()


## 2. repeat same experiment as in (1), but map values to same discrete 
## domain as actual ranks { seq(1/24, 23/24, 1/12) }
set.seed(20)

map_rank <- function(x) {
  for(r in seq(1/24, 23/24, 1/12)) {
    if(abs(r-abs(x)) < 1/24) { return(r) }
  }
}

sim_disc_unif <- function(N) {
  udat <- runif(5000, 0, 1)
  udat <- sapply(udat, map_rank)
  return(fitdist(udat,'beta')$estimate)
}

params2 <- data.frame(t(replicate(1000, sim_disc_unif(5000))))
write.table(params2, file='data/params2.RData')
params2 <- read.table('data/params2.RData')

pdf("~/GitHub/random-fields/images/checks/beta_param_box2.pdf")
par(mfrow = c(1, 2))
boxplot(params2$shape1, main="shape1")
boxplot(params2$shape2, main="shape2")
dev.off()



## 3. take actual ranks from rank_tab where s1=s2, tau=0 and randomize within bins 
## uniformly to mimic continuous r.v., then fit beta params and look at hist
set.seed(30)

rank_tab <- read.table('data/rank_tab.RData')

spread_rank <- function(r) {
  return(runif(1, r-1/24, r+1/24))
}

fit_cont_ranks <- function(s) {
  df <- rank_tab %>%
    filter(s1==s, s2==s, tau==0) %>%
    mutate(rank = (rank-0.5)/12) %>%
    mutate(cont_rank = sapply(rank, spread_rank))
  return(fitdist(df$cont_rank,'beta')$estimate)
}

params3 <- data.frame(t(sapply(seq(1,4,0.5), fit_cont_ranks)))

pdf("~/GitHub/random-fields/images/checks/beta_param_box3.pdf")
par(mfrow = c(1, 2))
boxplot(params3$shape1, main="shape1")
boxplot(params3$shape2, main="shape2")
dev.off()


## looks like fitting the beta params to discrete ranks was the issue!


