

## Using simulated data, analyze rank statistics for each threshold (tau)
## in a range from 0-4 and build a grid of histograms for all tau

set.seed(7332) # ties between rank broken at random

# library(RandomFields)
library(ggplot2)
library(grid)
library(gridExtra)
source("~/GitHub/random-fields/functions/rank_obs.R")

s_2 <- seq(1,6,0.5)
# nam <- paste("fields_data_s4", s_2, sep = "")
nam <- paste("fields_data_rho0_s4", s_2, sep = "")

## get true xi values for each range
get_xi <- function(range) {
  # range (list of 2): scale parameters for observation and ensemble; c(s_1, s_2)
  s_1 <- range[1]
  s_2 <- range[2]
  
  ## parameters
  smooth <- c(1.5, 1.5, 1.5)            #nu: smoothnes / differentiability
  rng <- c(s_1, sqrt(s_1*s_2), s_2)     #range: s = 1/a  
  var <- c(1, 1)                        #variances
  rho <- 0.8                            #rho: percent cross correlation 
  
  ## model
  model_biwm <- RandomFields::RMbiwm(nu = smooth, s = rng, cdiag = var, rhored = rho)
  cov_mat <- RandomFields::RFcov(model_biwm, x=0)
  return(round(cov_mat[1,1,2], 4))
}

xi_list <- c()
for (r in 1:length(s_2)) {
  xi_list[r] <- get_xi(c(4,s_2[r]))
}

## load and plot data
pdf("~/GitHub/random-fields/images/hists/rank_hists_rho00_n1000-new.pdf")

for (ii in 1:length(nam)){
  
  print(paste("range param: ", s_2[ii], sep = ""))
  
  # load(paste("~/GitHub/random-fields/data/fields_rho0/", nam[ii], ".RData", sep = ""))
  # data <- fields_data
  load(paste("/Volumes/My\ Passport/Forecasting\ Data/fields_rho0/", nam[ii], ".RData", sep=""))
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
        scale_x_continuous(breaks=seq(0,12,2), limits=c(0,12)) 
      
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






