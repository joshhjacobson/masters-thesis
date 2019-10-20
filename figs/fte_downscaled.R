
## Produce FTE histograms for downscaled GSDM data

library(lubridate)
library(dplyr)
library(reshape2)
library(ggplot2)


# Threshold exceedence ranking --------------------------------------------

## import fields_dat dim(lat x lon x time x mem)
load("../data/gsdm/gsdm_downscaled_fields.RData")

## compute the mean threshold exceedence of fields_df at array of thresholds
## and return analysis ranks
exceed_ranks <- function(dat_arr, tau){
  # dat_arr: 3d array (lon x lat x member)
  # tau: vector of thresholds
  ranks <- array(dim= length(tau))
  for (i in 1:length(tau)) {
    m <- apply(dat_arr, 3, function(field) mean(as.vector(field) > tau[i]))
    if(length(unique(m)) != 0) { 
      # exclude exact ties
      ranks[i] <- rank(m, ties.method = "random")[1]
    } else {
      ranks[i] <- NULL
    }
  }
  return(ranks)
}

## iterate over time, compute ranks at different thresholds
tau <- c(5,10,20)
ranks_df <- data.frame(t(apply(field_dat, 3, exceed_ranks, tau=tau))) # (day x tau)


# FTE Histograms ----------------------------------------------------------

## stratify days by month and build histograms for each month-threshold pair
names(ranks_df) <- paste('tau', tau, sep='')

m <- c(1, 4, 7, 10) # Jan, Apr, Jul, Oct
dates <- seq.Date(as.Date('2002-01-02'), as.Date('2015-12-30'), by='day')
date_idx <- (month(dates) %in% m)

facet_labs <- c(
  '1' = "January",
  '4' = "April",
  '7' = "July",
  '10' = "October",
  'tau5' = "tau = 5",
  'tau10' = "tau = 10",
  'tau20' = "tau = 20"
)

png("fte_downscaled.png", units="in", height=6.2, width=8, res=200, pointsize=10)

ranks_df %>%
  mutate(month = month(dates[date_idx])) %>%
  melt(id.vars='month', variable.name='tau', value.name='rank') %>%
  ggplot(aes(x=rank)) +
  geom_histogram(aes(y=..density..), bins=12, fill="black", color="white") +
  scale_y_continuous(breaks=scales::pretty_breaks(n=2)) +
  facet_grid(tau ~ month, labeller=as_labeller(facet_labs)) +
  labs(y="", x="") +
  theme_bw() +
  theme(legend.title = element_blank(),
        strip.background = element_blank(),
        text = element_text(color="black"),
        strip.text= element_text(size=12, face="bold"),
        axis.text = element_text(size=9),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major = element_line(linetype="dashed", size=0.3),
        aspect.ratio = 1/1,
        plot.margin = unit(c(0,0,0,0), "cm"))

dev.off()


