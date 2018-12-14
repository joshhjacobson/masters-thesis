
## Produce line charts to analyze trends in Scheuerer statistic for a given tau
## over varying s_2 and over increasing s_1

library(dplyr)
library(ggplot2)

plot_scheuerer_s1 <- function(t, s_dat) {
  
  # t: threshold exceedence level
  # s_dat: rect table built in process_data.R
  
  df <- filter(s_dat, tau == t)
  p <- ggplot(df, aes(log(ratio), exceedence, group=factor(s1))) + 
    geom_line(aes(color=factor(s1)), size=0.8) +
    scale_colour_brewer(direction=-1, name="s_1") +
    ylim(2.5,4.25) +
    labs(x="log ratio (s2/s1)",y="Scheuerer Statistic",
         title=paste("Deviation from uniformity for tau=", t, sep = "")) +
    theme_minimal() +
    # theme(plot.title = element_text(size=12),
    #       axis.title = element_text(size = 12),
    #       legend.title = element_text(size = 12))
    theme(title = element_text(size = 10))
  
  print(p)
}
