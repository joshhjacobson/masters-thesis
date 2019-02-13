
## Script to create figures for project presentation

## import libraries
library(RandomFields)
library(dplyr)
library(grid)
library(gridExtra)
library(ggplot2)
require(RColorBrewer)
library(cowplot)

## import functions
source("~/GitHub/random-fields/functions/grid_arrange_shared_legend.R")

## set standard grid
x <- y <- seq(-20, 20, 0.2)



# 1 -----------------------------------------------------------------------

model_biwm <- RMbiwm(nu = c(1.5, 1.5, 1.5), s = c(2, sqrt(2*3), 3), cdiag = c(1, 1), rhored = 0.8)
fields <- RFsimulate(model_biwm, x, y)
fields <- data.frame(fields$variable1, fields$variable2)

## plot
dat <- expand.grid(x = x, y = y)
fplots <- list()
for (i in 1:ncol(fields))
  local({
    i <- i
    # update data being plotted
    dat$z <- fields[,i] 
    p <- ggplot(dat, aes(x, y)) +
      geom_raster(aes(fill = z)) +
      scale_fill_gradientn(name = "Surface", colours = brewer.pal(9, "Blues")) +
      theme_void() +
      theme(plot.title = element_text(hjust = 0.5), # center title
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            text = element_text(size=12))
    
    if (i == 1) {
      p <- p + labs(x="",y="",title="Observation")
    } else {
      p <- p + labs(x="",y="",title="Ensemble Mean")
    }
    # add each plot into plot list
    fplots[[i]] <<- p  
  })

pdf('/Users/joshjacobson/Desktop/obs_mean.pdf')
plot_grid(plotlist = fplots, ncol=2)
dev.off()


# 2 -----------------------------------------------------------------------

build_ensemble <- function(rng, rhored, xi=0.8, smooth=c(1.5, 1.5, 1.5), var=c(1, 1), n=11) {
  
  # rng (list of 3): scale parameters for observation and ensemble; c(s_1, gmean, s_2), s=1/a
  # rhored: percent cross correlation
  # xi (float): weight ratio between ensemble mean and perturbation
  # smooth: smoothnes or differentiability (nu)
  # var: variances
  # x, y (arrays): field grid points
  # n (num): number of ensemble members
  
  ## grid vectors
  x <- y <- seq(-20, 20, 0.2)
  
  ## model
  model_biwm <- RMbiwm(nu = smooth, s = rng, cdiag = var, rhored = rhored)
  fields <- RFsimulate(model_biwm, x, y)
  
  ## ensemble perturbation
  model_whittle <- RMwhittle(nu = smooth[3], notinvnu = TRUE,
                             scale = rng[3], var = var[2])
  
  omega <- RFsimulate(model_whittle, x, y, n=n)
  omega <- as.matrix(data.frame(omega))
  
  ensemble_mean <- fields$variable2
  ensemble_mean <- replicate(n, ensemble_mean)
  
  ## NOTE: xi is set as 0.8, rhored is now adjusted above s.t. (true_rho = xi) holds
  ## weight ratio between ensemble mean and variance (force xi = true_rho)
  
  ensemble <- xi*ensemble_mean + sqrt(1-xi^2)*omega
  
  ## realization
  realization <- data.frame(fields$variable1, ensemble)
  # names(realization) <- c("obs", paste("f", 1:n, sep = ""))
  
  return(realization)
}

fields <- build_ensemble(rng=c(1.5, sqrt(1.5*3.25), 3.25), rhored=0.8, xi=0.6, n=2)

## format data as xyz dataframe 
dat <- expand.grid(x = x, y = y)
dat["z"] <- fields[,1]

l.min <- floor(min(fields[,1]))
l.max <- ceiling(max(fields[,1]))

## collect each plot in a list
fplots <- list()

## build plot list
for (i in 1:ncol(fields))
  local({
    i <- i
    # update data being plotted
    dat$z <- fields[,i]
    p <- ggplot(dat, aes(x, y)) +
      geom_raster(aes(fill = z > 1)) +
      scale_fill_manual(values = c("TRUE" = "#253494", "FALSE" = "#F0F0F0")) +
      theme_void() +
      theme(plot.title = element_text(hjust = 0.5, size=10), # center title
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            legend.position="none") 
    
    if (i == 1) {
      p <- p + labs(x="",y="",title="Observation")
    } else {
      p <- p + labs(x="",y="",title=paste("M", i-1, sep = ""))
    }
    # add each plot into plot list
    fplots[[i]] <<- p  
  })

# pdf('/Users/joshjacobson/Desktop/forecast.pdf')
plot_grid(plotlist = fplots, nrow=1)
# dev.off()



# Ranks -------------------------------------------------------------------

df <- dplyr::filter(rank_tab, s1==4 & s2==3.2 & tau==0)
ggplot(df, aes(rank)) +
  geom_histogram(binwidth = 1, fill="darkblue", color="white", size=0.25) +
  theme(plot.title = element_text(hjust = 0.5, size=14)) +
  labs(x="",y="",title="s1=4, s2=3.2, 0%") +
  scale_x_continuous(breaks=seq(0,12,2), limits=c(0,13))

params <- filter(cont_fit_tab, s1==4 & ratio==3.2/4 & tau==0)
dist <- dbeta(seq(0,1,length=50), params$a, params$b)
dat <- data.frame(x=seq(1,12,length=length(dist)), dist)

ggplot(df, aes(rank)) +
  geom_histogram(aes(y = stat(count / sum(count))*10), # hack to get similar scales
                 binwidth = 1, fill="darkblue", color="white", size=0.25) +
  geom_line(data = dat, aes(x=x, y=dist), color="orange") +
  theme(plot.title = element_text(hjust = 0.5, size=14)) +
  labs(x="",y="",title="s1=4, s2=3.2, 0%") +
  scale_x_continuous(breaks=seq(0,12,2), limits=c(0,13))


