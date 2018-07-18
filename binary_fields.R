

## Script to plot ensemble data at a given threshold (tau)

library(ggplot2)
source("~/GitHub/random-fields/functions/plot_binary.R")

load("~/GitHub/random-fields/data/fields_data_s42.RData")
data <- fields_data

## save plots to pdf for visual analysis
pdf('~/GitHub/random-fields/images/binary_fields_s42_tau00.pdf')
for(i in 1:20) {
  plot_binary(data[[i]], tau = 0)
}
# lapply(data, plot_binary, tau = 1)
dev.off()


