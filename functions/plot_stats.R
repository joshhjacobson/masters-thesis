
## Function to plot distribution of realization statistics

library(dplyr)
library(tidyr)
library(ggplot2)

plot_stats <- function(dat) {
  
  #dat: data frame (or similar) with columns to be plotted
  
  len <- length(dat[1,]) - 1
  mdat <- as_tibble(colMeans(dat))
  mdat$mems <- c("obs", paste("f", 1:len, sep = ""))
  
  dat %>%
    as_tibble(.) %>%
    gather(., factor_key = TRUE) %>%
    ggplot(., aes(x = value)) +
      geom_density(size = 0.8, colour = "#542788") +
      facet_wrap(vars(key)) +
      geom_vline(data=mdat, aes(xintercept=value),
                 linetype="dashed", size = 0.2, colour = "#fd8d3c") +
      labs(x = "statistic")
    
}



