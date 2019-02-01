## Plot a grid of rank hists for each (s1,s2) pair, organized by s2

library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)

plot_ranks <- function(s_1, t, rank_tab, rand_count_tab) {
  
  # s1: range of obs field
  # t: threshold exceedence level
  # rank_tab: rect table built in process_data.R
  
  s_2 <- seq(0.5*s_1, 1.5*s_1, 0.1*s_1)
  
  hplots <- list()
  for (ii in 1:length(s_2))
    local({
      i <- ii
      #errors here due to environment issue?
      df <- filter(rank_tab, s1==s_1 & s2==s_2[i] & tau==t)
      rp <- filter(rand_count_tab, s1==s_1 & s2==s_2[i] & tau==t)
      p <- ggplot(df, aes(rank)) +
        geom_histogram(binwidth = 1, fill="darkblue", color="white", size=0.25) +
        theme(plot.title = element_text(hjust = 0.5, size=10)) +
        labs(x="",y="",title=paste("s2 = ", s_2[i], ", ", rp['r_percent'], "%", sep = "")) +
        scale_x_continuous(breaks=seq(0,12,2), limits=c(0,13))

      hplots[[i]] <<- p
    })
  
  ## arrange plots in grid
  grid.arrange(
    arrangeGrob(grobs=hplots, ncol=4, 
                bottom=textGrob("observation rank"), 
                left=textGrob("count", rot=90),
                top=paste("s1=", s_1, ", tau=", t, sep = ""))
  )
}