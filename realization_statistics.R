
## Calculate mean statistics for each realization column and rbind to dataframe
source("~/GitHub/random-fields/functions/plot_stats.R")

tau <- 1
s_2 <- seq(1,6,0.5)
nam <- paste("s4", s_2, sep = "")
# nam <- paste("rho0_s4", s_2, sep = "")

mean_stat <- function(data, tau) {
  # data: observation and ensemble in cols of data frame
  # tau: threshold
  return(colMeans(data > tau))
}


pdf("~/GitHub/random-fields/images/transformed/dists/means_dist_rho08_tau01_n1000.pdf")

for (ii in 1:length(nam)){
  
  print(paste("range param: ", s_2[ii], sep = ""))
  
  load(paste("/Volumes/My\ Passport/Forecasting\ Data/transformed/fields/exp_fields_",
             nam[ii], ".RData", sep=""))
  data <- exp_dat

  ## store mean stat data as rows of new data frame
  exp_field_means <- do.call("rbind", lapply(data, mean_stat, tau=tau))
  save(exp_field_means,
       file = paste("/Volumes/My\ Passport/Forecasting\ Data/transformed/mean_dat/exp_field_means_",
                    nam[ii], ".RData", sep = ""))
  
  ## plot column distributions
  plot_stats(exp_field_means, paste(nam[ii], "_tau01", sep=""))
}
dev.off()


