

## Analyze distribution of rank between observation and ensemble mean
## with rho = 0 for 1K samples at a range of thresholds 

## partition tasks by working envionments
local <- TRUE
remote <- FALSE

if(local) {
  
  source("~/GitHub/random-fields/functions/get_data.R")
  
  ii <- 1 #iterator
  range <- seq(1, 6, 1)
  rho_rng_dat <- list()
  for(s2 in range) {
    print(paste("range param: ", s2, sep=""))
    rho_rng_dat[[ii]] <- get_data(15, c(4,s2))
    ii <- ii + 1
  }
  
  # save(rho_rng_dat, file = "rho_rng_dat.RData")
}

if(local) {
  
  library(ggplot2)
  library(grid)
  library(gridExtra)
  source("~/GitHub/random-fields/functions/rank_obs.R")
  
  # load("~GitHub/random-fields/data/tests/rho_rng_dat.RData")
  dat <- rho_rng_dat
  
  # obtain data frame with range as columns and obs rank as rows
  range <- seq(1, 6, 1)
  rank_test_tbl <- data.frame(row.names = 1:length(dat[[1]]))
  for (ii in range) {
    rank_test_tbl <- cbind(rank_test_tbl,
                           do.call("rbind", lapply(dat[[ii]], rank_obs, tau=0)))
  }
  names(rank_test_tbl) <- paste("s2=", 1:length(range), sep = "")
  
  
  ## plot histograms for each range param
  hplots <- list()
  for (i in 1:ncol(rank_test_tbl))
    local({
      i <- i
      p <- ggplot(rank_test_tbl, aes(rank_test_tbl[,i])) +
        geom_histogram(binwidth = 1, fill="darkblue", color="white", size=0.25) +
        theme(plot.title = element_text(hjust = 0.5)) +
        labs(x="",y="",title=paste("s2 = ", range[i], sep = "")) +
        scale_x_continuous(breaks=seq(0,3,1), limits=c(0,3))
      
      hplots[[i]] <<- p  
    })
  
  ## arrange plots in grid
  quartz()
  grid.arrange(
    arrangeGrob(grobs=hplots, ncol=3, 
                bottom=textGrob("observation rank"), 
                left=textGrob("count", rot=90))
  )
  
}
