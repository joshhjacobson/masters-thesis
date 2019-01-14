## Plot a grid of rank hists for each (s1,s2) pair, organized by s2
## overlay respective beta distribution

library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)

plot_ranks_beta <- function(s_1, t, rank_tab, fit_tab) {
  
  # s1: range of obs field
  # t: threshold exceedence level
  # rank_tab: rect table built in process_data.R
  # fit_tab: rect table built in process_data.R
  
  s_2 <- seq(0.5*s_1, 1.5*s_1, 0.1*s_1)
  
  hplots <- list()
  for (ii in 1:length(s_2))
    local({
      i <- ii
      #errors here due to dplyr::filter for some i's
      
      ## get beta params and build dist
      params <- filter(fit_tab, s1==s_1 & ratio==s_2[i]/s_1 & tau==t)
      dist <- dbeta(seq(0,1,length=50), params$a, params$b)
      dat <- data.frame(x=seq(1,12,length=length(dist)), dist)
      
      df <- filter(rank_tab, s1==s_1 & s2==s_2[i] & tau==t)
      p <- ggplot(df, aes(rank)) +
        geom_histogram(aes(y = stat(count / sum(count))*10), # hack to get similar scales
                       binwidth = 1, fill="steelblue", color="white", size=0.25) +
        geom_line(data = dat, aes(x=x, y=dist), color="orange") +
        theme(plot.title = element_text(hjust = 0.5)) +
        labs(x="",y="",title=paste("s2 = ", s_2[i], sep = "")) +
        scale_x_continuous(breaks=seq(0,12,2), limits=c(0,13))
      
      hplots[[i]] <<- p
    })
  
  ## arrange plots in grid
  grid.arrange(
    arrangeGrob(grobs=hplots, ncol=4, 
                bottom=textGrob("observation rank"), 
                left=textGrob("dist", rot=90),
                top=paste("s1=", s_1, ", tau=", t, sep = ""))
  )
  
}