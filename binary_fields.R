

## Script to plot ensemble data at a given threshold (tau)

library(ggplot2)
source("~/GitHub/random-fields/functions/plot_binary.R")

s_2 <- seq(1,6,0.5)
# nam <- paste("fields_data_rho0_s4", s_2, sep = "")
nam <- paste("exp_fields_rho0_s4", s_2, sep = "")

for (ii in 1:length(nam)){
  
  print(paste("range param: ", s_2[ii], sep = ""))
  
  # load(paste("~/GitHub/random-fields/data/fields_rho0/", nam[ii], ".RData", sep=""))
  # data <- fields_data
  load(paste("/Volumes/My\ Passport/Forecasting\ Data/transformed/fields_rho0/", nam[ii], ".RData", sep=""))
  data <- exp_dat
  
  ## save plots to pdf for visual analysis
  # pdf(paste("~/GitHub/random-fields/images/fields/", nam[ii], "_tau00.pdf", sep=""))
  pdf(paste("~/GitHub/random-fields/images/transformed/fields/", nam[ii], "_tau01.pdf", sep=""))
  for(i in 1:20) {
    plot_binary(data[[i]], tau = 1)
    # plot_binary(data[[i]], tau = -log(0.5))
  }
  # lapply(data, plot_binary, tau = 1)
  dev.off()
  
}



