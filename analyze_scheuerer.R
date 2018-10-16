
## Load all rank_stats and produce chart for each s_1 
source("~/GitHub/random-fields/functions/plot_scheuerer.R")

s_1 <- seq(1,2.5,0.5)
nam <- paste("rank_stats_s", s_1, sep = "")

pdf("~/GitHub/random-fields/images/scheuerer_charts.pdf")
for (ii in 1:length(nam)){
  print(nam[ii])
  ## load rank_stats
  load(paste("~/GitHub/random-fields/data/", nam[ii], ".RData", sep = ""))
  ## produce chart
  plot_scheuerer(s_1[ii], rank_stats)
  ## remove rank_stats
  rm(rank_stats)
}

dev.off()
