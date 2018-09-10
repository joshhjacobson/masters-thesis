

## Build historgrams from special case rank data

library(ggplot2)
library(grid)
library(gridExtra)

## Load both short range and random noise, put in df
load("/Volumes/Passport/Forecasting/big_dat/rank_data/short_range_ranks.RData")
short_range <- rank_dat
rm(rank_dat)

load("/Volumes/Passport/Forecasting/big_dat/rank_data/random_noise_ranks.RData")
random_noise <- rank_dat
rm(rank_dat)

rank_dat <- data.frame(short_range, random_noise)


## Plot histograms
pdf("~/GitHub/random-fields/images/special_case/hists/rank_hists_rho0_tau0_ens200_n4K.pdf")

hplots <- list()
for (i in 1:ncol(rank_dat))
  local({
    i <- i
    p <- ggplot(rank_dat, aes(rank_dat[,i])) +
      geom_histogram(binwidth = 10, fill="darkblue", color="white", size=0.25) +
      theme(plot.title = element_text(hjust = 0.5)) +
      labs(x="",y="",title=names(rank_dat)[i]) +
      scale_x_continuous(breaks=seq(0,201,25), limits=c(-20,220)) 
    
    hplots[[i]] <<- p  
  })

## arrange plots in grid
grid.arrange(
  arrangeGrob(grobs=hplots, ncol=2, 
              bottom=textGrob("observation rank, concat left"), 
              left=textGrob("count", rot=90),
              top="Special Case: rho=0, tau=0, n=200, 4K samples" )
)

dev.off()
