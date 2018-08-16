

## Another sanity check -- marginal calibration
## Using simulated data, analyze pointwise (0,0) rank statistics for rho = 0, 0.8

## NOTE: independent of s_2 so using same scale

set.seed(7332) # ties between rank broken at random

library(ggplot2)
library(grid)
library(gridExtra)
source("~/GitHub/random-fields/functions/rank_pointwise.R")
source("~/GitHub/random-fields/functions/get_xi.R")

s_2 <- seq(1,6,0.5)
# nam <- paste("fields_data_s4", s_2, sep = "")
nam <- paste("fields_data_rho0_s4", s_2, sep = "")

## get list of true xi values for each range
xi_list <- get_xi(s_2)

## setup dataframe, note number of rows
rank_dat <- data.frame(row.names = 1:1000)

## preform computations
for (ii in 1:length(nam)){
  
  print(paste("range param: ", s_2[ii], sep = ""))
  
  load(paste("/Volumes/My\ Passport/Forecasting\ Data/fields_rho0/", nam[ii], ".RData", sep=""))
  data <- fields_data
  
  ## collect pointwise rank data on range in new column
  rank_dat <- cbind(rank_dat,  col = sapply(data, rank_pointwise))
}

names(rank_dat) <- paste("s2=", s_2, sep="")


## plot histograms for each rho
hplots <- list()
for (i in 1:ncol(rank_dat))
  local({
    i <- i
    p <- ggplot(rank_dat, aes(rank_dat[,i])) +
      geom_histogram(binwidth = 1, fill="darkblue", color="white", size=0.25) +
      theme(plot.title = element_text(hjust = 0.5)) +
      labs(x="",y="",title=paste("s2=", s_2[i], ", xi=", xi_list[i], sep = "")) +
      scale_x_continuous(breaks=seq(0,12,2), limits=c(0,12)) 
    
    hplots[[i]] <<- p  
  })

## arrange plots in grid
pdf("~/GitHub/random-fields/images/hists/pointwise_rank_hists_rho00_n1000.pdf")
grid.arrange(
  arrangeGrob(grobs=hplots, ncol=3, 
              bottom=textGrob("observation rank at (0,0)"), 
              left=textGrob("count", rot=90))
)
dev.off()
