

## Script to plot ensemble data at a given threshold (tau)

library(ggplot2)
source("~/GitHub/random-fields/functions/plot_binary.R")

# load("~/GitHub/random-fields/data/fields_data_us_xi075_n11.RData")
data <- fields_data_ds_xi075_n11

## save plots to pdf for visual analysis
pdf('~/GitHub/random-fields/images/binary_fields_ds_xi075_tau01.pdf')
# for(i in 1:length(data)) {
#   plot_binary(data[[i]], tau = 1)
# }
lapply(data, plot_binary, tau = 1)
dev.off()


