

## Using simulated data, analyze rank statistics for each threshold (tau)
## in a range from 0-4 and build a grid of histograms for all tau

set.seed(7332) # ties between rank broken at random

# library(RandomFields)
library(ggplot2)
library(grid)
library(gridExtra)
source("~/GitHub/random-fields/functions/rank_obs.R")
source("~/GitHub/random-fields/functions/get_xi.R")

s_2 <- seq(1,6,0.5)
nam <- paste("fields_data_s4", s_2, sep = "")
# nam <- paste("fields_data_rho0_s4", s_2, sep = "")

## get list of true xi values for each range
xi_list <- get_xi(s_2, rho=0.8)

## load and plot data
pdf("~/GitHub/random-fields/images/hists/rank_hists_rho08_n1000-new-hope.pdf")

for (ii in 1:length(nam)){
  
  print(paste("range param: ", s_2[ii], sep = ""))
  
  # load(paste("~/GitHub/random-fields/data/fields_rho0/", nam[ii], ".RData", sep = ""))
  # data <- fields_data
  load(paste("/Volumes/Passport/Forecasting/fields/", nam[ii], ".RData", sep=""))
  data <- fields_data
  
  ## collect rank data on tau values
  tau <- seq(-3.5, 4, 0.5)
  tau_rank_dat <- data.frame(row.names = 1:length(data))
  for(t in tau) {
    print(t)
    tau_rank_dat <- cbind(tau_rank_dat, 
                          col = sapply(data, rank_obs, tau=t))
  }
  names(tau_rank_dat) <- tau
  
  
  ## plot histograms for each tau
  hplots <- list()
  for (i in 1:ncol(tau_rank_dat))
    local({
      i <- i
      p <- ggplot(tau_rank_dat, aes(tau_rank_dat[,i])) +
        geom_histogram(binwidth = 1, fill="darkblue", color="white", size=0.25) +
        theme(plot.title = element_text(hjust = 0.5)) +
        labs(x="",y="",title=paste("tau = ", tau[i], sep = "")) +
        scale_x_continuous(breaks=seq(0,12,2), limits=c(0,13)) 
      
      hplots[[i]] <<- p  
    })
  
  ## arrange plots in grid
  grid.arrange(
    arrangeGrob(grobs=hplots, ncol=4, 
                bottom=textGrob("observation rank"), 
                left=textGrob("count", rot=90),
                top=paste("s2=", s_2[ii], ", xi=", xi_list[ii], sep = ""))
  )
}

dev.off()






