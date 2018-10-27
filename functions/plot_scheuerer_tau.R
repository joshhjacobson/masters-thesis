
## Produce line charts to analyze trends in Scheuerer statistic for a given s_1
## over varying s_2 and at different threshold excedence levels

library(dplyr)
library(ggplot2)

plot_scheuerer_tau <- function(s_1, s_dat) {
  
  # s_1: range parameter of observation
  # s_dat: scheuerer_dat table built in transform_scheuerer_dat.R
  
  df <- filter(s_dat, s1 == s_1)
  p <- ggplot(df, aes(ratio, value, group=factor(rowid))) + 
    geom_line(aes(color=factor(rowid)), size=0.8) +
    scale_colour_manual(name="Threshold",
                        values=c("#1b9e77","#d95f02","#7570b3","#e7298a",
                                 "#66a61e","#e6ab02","#a6761d","#666666", "#0a437a")) +
    labs(x="s2/s1",y="Scheuerer Statistic",
         title=paste("Deviation from uniformity for s1=", s_1, sep = "")) +
    theme_minimal() +
    # theme(plot.title = element_text(size=12),
    #       axis.title = element_text(size = 12),
    #       legend.title = element_text(size = 12))
    theme(title = element_text(size = 10))
  
  print(p)
}
