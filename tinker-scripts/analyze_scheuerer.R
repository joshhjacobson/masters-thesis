
## Load all rank_stats and produce chart for each s_1 
source("~/GitHub/random-fields/functions/plot_scheuerer_tau.R")
source("~/GitHub/random-fields/functions/plot_scheuerer_s1.R")

s_1 <- seq(1,4,0.5)
tau <- seq(0,4,0.5)
scheuerer_dat <- read.table("~/GitHub/random-fields/data/scheuerer_dat.RData")

# pdf("~/GitHub/random-fields/images/scheuerer_charts_tau.pdf")
# for (s1 in s_1){
#   plot_scheuerer_tau(s1, scheuerer_dat)
# }

pdf("~/GitHub/random-fields/images/scheuerer_charts_s1.pdf")
for (t in tau){
  plot_scheuerer_s1(t, scheuerer_dat)
}

dev.off()
