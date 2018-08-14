
## Calculate mean statistics for each realization column and rbind to dataframe
source("~/GitHub/random-fields/functions/plot_stats.R")

tau <- 0
s_2 <- seq(1,6,0.5)
nam <- paste("rho0_s4", s_2, sep = "")
# nam <- paste("rho0_s4", s_2, sep = "")

mean_stat <- function(data, tau) {
  # data: observation and ensemble in cols of data frame
  # tau: threshold
  return(colMeans(data > tau))
}


pdf("~/GitHub/random-fields/images/dists/means_dist_rho00_tau00_n1000.pdf")

for (ii in 1:length(nam)){
  
  print(paste("range param: ", s_2[ii], sep = ""))
  
  load(paste("/Volumes/My\ Passport/Forecasting\ Data/fields_rho0/fields_data_",
             nam[ii], ".RData", sep=""))
  data <- fields_data

  ## store mean stat data as rows of new data frame
  field_means <- do.call("rbind", lapply(data, mean_stat, tau=tau))
  save(field_means,
       file = paste("/Volumes/My\ Passport/Forecasting\ Data/mean_dat/field_means_",
                    nam[ii], ".RData", sep = ""))
  
  ## plot column distributions
  plot_stats(field_means, nam[ii])
}
dev.off()


