

## Another sanity check -- marginal calibration
## Using simulated data, analyze pointwise (0,0) rank statistics for rho = 0, 0.8

## NOTE: independent of s_2 so using same scale

set.seed(7332) # ties between rank broken at random

library(ggplot2)
library(grid)
library(gridExtra)
source("~/GitHub/random-fields/functions/rank_pointwise.R")

## load data
load("~/GitHub/random-fields/data/fields/fields_data_s44.RData")
dat_rho08 <- fields_data

load("~/GitHub/random-fields/data/fields_rho0/fields_data_rho0_s44.RData")
dat_rho00 <- fields_data


## calculate pointwise rank stats
rank_dat <- data.frame(row.names = 1:length(dat_rho00))
rank_dat <- cbind(rank_dat,  col = sapply(dat_rho08, rank_pointwise))
rank_dat <- cbind(rank_dat,  col = sapply(dat_rho00, rank_pointwise))
names(rank_dat) <- c("rho=0.8", "rho=0")


## plot histograms for each rho
hplots <- list()
for (i in 1:ncol(rank_dat))
  local({
    i <- i
    p <- ggplot(rank_dat, aes(rank_dat[,i])) +
      geom_histogram(binwidth = 1, fill="darkblue", color="white", size=0.25) +
      theme(plot.title = element_text(hjust = 0.5)) +
      labs(x="",y="",title=names(rank_dat)[i]) +
      scale_x_continuous(breaks=seq(0,12,2), limits=c(0,12)) 
    
    hplots[[i]] <<- p  
  })

## arrange plots in grid
quartz()
grid.arrange(
  arrangeGrob(grobs=hplots, ncol=2, 
              bottom=textGrob("observation rank at (0,0)"), 
              left=textGrob("count", rot=90))
)