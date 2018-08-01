

## Script to plot ensemble data at a given threshold (tau)

library(ggplot2)
source("~/GitHub/random-fields/functions/plot_binary.R")

s_2 <- seq(1,6,0.5)
nam <- paste("fields_data_s4", s_2, sep = "")

for (ii in 1:length(nam)){
  
  print(paste("range param: ", s_2[ii], sep = ""))
  
  load(paste("~/GitHub/random-fields/data/fields/", nam[ii], ".RData", sep=""))
  data <- fields_data
  
  ## save plots to pdf for visual analysis
  pdf(paste("~/GitHub/random-fields/images/fields/", nam[ii], "_tau00.pdf", sep=""))
  for(i in 1:20) {
    plot_binary(data[[i]], tau = 0)
  }
  # lapply(data, plot_binary, tau = 1)
  dev.off()
  
}



