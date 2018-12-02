
## Process and combine exceedence data to use for plotting.
nam <- "data/exceed_dat_s"
s_1 <- seq(1,3.5,0.5)
tau <- seq(0,4,0.5)
N <- 5000

## Compute rank of an observation's mean in the distribution
## of the ensemble means
rank_obs <- function(means) {
  # means: observation and ensemble means with obs listed first
  r <- rank(means, ties.method = "random")[[1]] 
  return(r)
}

## Compute Scheuerer statistic for a given column of rank data
scheuerer_stat <- function(col) {
  s_stat <- sum(sapply(col, function(rank) abs(rank-6.5))) / length(col)
}


## init scheuerer cube and ranks array
ss_cube <- array(dim = c(7,11,9))
rank_arr <- array(dim = c(N, 11, 9, 7))


## loop over s_1
for (ii in 1:length(s_1)) {
  print(paste("s1 = ", s_1[ii]))
  s_2 <- seq(0.5*s_1[ii],1.5*s_1[ii],0.1*s_1[ii])
  ## init rank_cube array
  rank_cube <- array(dim = c(N, 11, 9))
  ## read in data
  load(paste(nam, s_1[ii], ".RData", sep=""))
  ## loop over s_2
  for (jj in 1:length(s_2)) {
    ## loop over tau
    for (t in 1:length(tau)) {
      ## rank each realization
      rank_cube[,jj,t] <- apply(arr_dat[,,t,jj], 1, rank_obs)
    }
  }
  
  ## fill in ranks array
  rank_arr[,,,ii] <- rank_cube
  
  ## fill in scheuerer table (slice of ss cube)
  ## loop over tau
  for (t in 1:length(tau)) {
    ## compute ss for each column in s_2
    ss_cube[ii,,t] <- apply(rank_cube[,,t], 2, scheuerer_stat)
  }
 
  rm(arr_dat, rank_cube)
}

## flatten data
library(reshape2)
library(dplyr)
library(scales)

ss_tab <- melt(ss_cube, value.name='exceedence',
           varnames=c('s1_idx', 's2_idx', 'tau_idx')) %>% 
      mutate(., 
             s1=rescale(s1_idx, to=c(1,4)),
             ratio=rescale(s2_idx, to=c(0.5,1.5)),
             tau=rescale(tau_idx, to=c(0,4))
            ) %>%
      select(., s1, ratio, tau, exceedence)

rank_tab <- melt(rank_arr, value.name='rank',
                 varnames=c('N', 's2_idx', 'tau_idx', 's1_idx')) %>%
            mutate(., 
                   s1=rescale(s1_idx, to=c(1,4)),
                   s2=rescale(s2_idx, to=c(0.5,1.5))*s1,
                   tau=rescale(tau_idx, to=c(0,4))
            ) %>%
            select(., s1, s2, tau, N, rank)
  

## build plots
source("~/GitHub/random-fields/functions/plot_scheuerer_s1.R")
source("~/GitHub/random-fields/functions/plot_ranks.R")

## scheuerer stats
pdf("~/GitHub/random-fields/images/scheuerer_charts_s1.pdf")
for (t in tau){
  plot_scheuerer_s1(t, ss_tab)
}
dev.off()

## rank hists
pdf("~/GitHub/random-fields/images/rank_hists.pdf")
for (s1 in s_1){
  for (t in tau){
    print(paste("s1 =", s1, ", tau =", t))
    plot_ranks(s1, t, rank_tab)
  }
}
dev.off()
