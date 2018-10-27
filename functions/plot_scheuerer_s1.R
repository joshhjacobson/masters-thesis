
## Produce line charts to analyze trends in Scheuerer statistic for a given tau
## over varying s_2 and over increasing s_1

library(dplyr)
library(ggplot2)

plot_scheuerer_s1 <- function(t, s_dat) {
  
  # t: threshold exceedence level
  # s_dat: scheuerer_dat table built in transform_scheuerer_dat.R
  
  df <- filter(s_dat, rowid == t)
  p <- ggplot(df, aes(ratio, value, group=factor(s1))) + 
    geom_line(aes(color=factor(s1)), size=0.8) +
    scale_colour_brewer(direction=-1, name="s_1") +
    labs(x="s2/s1",y="Scheuerer Statistic",
         title=paste("Deviation from uniformity for tau=", t, sep = "")) +
    theme_minimal() +
    # theme(plot.title = element_text(size=12),
    #       axis.title = element_text(size = 12),
    #       legend.title = element_text(size = 12))
    theme(title = element_text(size = 10))
  
  print(p)
}
