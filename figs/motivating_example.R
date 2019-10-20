
## Script to produce figures for publication

library(RandomFields)
library(fields)
library(tidyverse)
library(gridExtra)
library(RColorBrewer)



# Setup -------------------------------------------------------------------

## Functions to numerically determine rho value that produces desired xi
rho_root <- function(rho, xi, smooth, rng, var) {
  model <- RMbiwm(nu = smooth, s = rng, cdiag = var, rhored = rho)
  return (RFcov(model, x=0)[1,1,2] - xi)
}
rhored_search <- function(xi, smooth, rng, var) {
  # xi (float): desired weight ratio between ensemble mean and perturbation
  # NOTE: other model parameters passed to RMbiwm() are assumed to be set and constant
  if(rng[1]==rng[3]){
    return(xi)
  } else{
    rhored <- uniroot(rho_root, c(xi, 1), xi=xi, smooth=smooth, rng=rng, var=var)$root
    return (rhored)
  }
}


## grid
x <- y <- seq(-20, 20, 0.2)


# Fig 1 -------------------------------------------------------------------

# motivating figure with a verification field and three forecast fields, 
# one with correct spatial structure, one with close structure, and one with mediocre structure

## model params
a1 <- 2
a2 <- c(a1, 1.5*a1, 0.9*a1)
xi <- 0.8; smooth <- c(1.5, 1.5, 1.5); var <- c(1, 1)

fields <- data.frame(array(dim=c(length(x)*length(y), 4)))

## simulate the verification and various ensemble means
seeds <- c(15, 20, 10) 
for(i in 1:length(a2)){
  rng <- c(a1, sqrt(a1*a2[i]), a2[i])
  rho <- rhored_search(xi, smooth, rng, var)
  
  ## model
  set.seed(22)
  model_biwm <- RMbiwm(nu=smooth, s=rng, cdiag=var, rhored=rho)
  sim <- RFsimulate(model_biwm, x, y)
  
  ## ensemble perturbation
  set.seed(seeds[i])
  model_whittle <- RMwhittle(nu=smooth[3], notinvnu=TRUE,
                             scale=rng[3], var=var[2])
  omega <- RFsimulate(model_whittle, x, y)
  
  fcast <- xi*sim$variable2 + sqrt(1-xi^2)*omega$variable1
  
  ## set verification field using simulation with "correct" structure
  if(i==1){
    fields[,1] <- sim$variable1
  }
  fields[,(i+1)] <- fcast
  rm(rng, rho, sim, omega, fcast, model_biwm, model_whittle)
} 


## plot the fields
dat <- expand.grid(x = x, y = y)
dat["z"] <- fields[,1]


png("motivating_example.png", units="in", width=8, height=2.2, res=220, pointsize=9)
par(mfrow=c(1,4))
par(mar=c(3,3,4,2)+0.1)
for(i in 1:ncol(fields)){
  dat$z <- fields[,i]
  if (i == 1) {
    quilt.plot(dat, col=brewer.pal(9, "BuPu"), nx=200, ny=200, 
               zlim = range(fields[,1]) + c(-0.1, 0.1), add.legend=FALSE,
               main="Verification")
  } else {
    quilt.plot(dat, col=brewer.pal(9, "BuPu"), nx=200, ny=200, 
               zlim = range(fields[,1]) + c(-0.1, 0.1), add.legend=FALSE,
               main=paste("Forecast", i-1, sep=" "))
  }
}
dev.off()



## ggplot method (continuous scale)
fplots <- list()
for (i in 1:ncol(fields))
  local({
    i <- i
    dat$z <- fields[,i]
    p <- ggplot(dat, aes(x, y)) +
      geom_raster(aes(fill = z)) +
      scale_fill_gradientn(name = "Surface",
                           colours = brewer.pal(9, "BuPu"),
                           limits = range(fields[,1]) + c(-0.1, 0.1) ) +
      theme_void() +
      theme(plot.title = element_text(hjust = 0.5), # center title
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            legend.position = 'none',
            text = element_text(size=10))

    if (i == 1) {
      p <- p + labs(x="",y="",title="Verification")
    } else {
      p <- p + labs(x="",y="",title=paste("Field", i-1, sep=" "))
    }
    fplots[[i]] <<- p
  })



# pdf('figure1.pdf', width=12, height=6)
# ggarrange(plotlist=fplots, common.legend=TRUE, legend='none')
# grid.arrange(arrangeGrob(grobs=fplots, nrow=1))
# dev.off()



# Figure 2 ----------------------------------------------------------------
rm(list=ls())

## A: a2 = 0.5*a1
## binary verification and three ensemble members with FTE histogram 


## Simulate the verification and ensemble
## model params
a1 <- 2
a2 <- 3
xi <- 0.8; smooth <- c(1.5, 1.5, 1.5); var <- c(1, 1)

rng <- c(a1, sqrt(a1*a2), a2)
rho <- rhored_search(xi, smooth, rng, var)

# model
set.seed(0)
model_biwm <- RMbiwm(nu=smooth, s=rng, cdiag=var, rhored=rho)
sim <- RFsimulate(model_biwm, x, y)

## ensemble perturbation
model_whittle <- RMwhittle(nu=smooth[3], notinvnu=TRUE,
                           scale=rng[3], var=var[2])
omega <- RFsimulate(model_whittle, x, y, n=3)
omega <- as.matrix(data.frame(omega))

ensemble_mean <- replicate(3, sim$variable2)
ensemble <- xi*ensemble_mean + sqrt(1-xi^2)*omega

fields <- data.frame(sim$variable1, ensemble)


## Visualize
dat <- expand.grid(x = x, y = y)
dat["z"] <- fields[,1]

fplots <- list()

## binary fields
for (i in 1:ncol(fields))
  local({
    i <- i
    # update data being plotted
    dat$z <- fields[,i]
    p <- ggplot(dat, aes(x, y)) +
      geom_raster(aes(fill = z > 0)) +
      scale_fill_manual(values = c("TRUE" = "#08306B", "FALSE" = "#F7FBFF")) +
      theme_bw() +
      theme(plot.title = element_text(hjust=0.5, size=12), # center title
            # axis.text.x = element_blank(),
            # axis.text.y = element_blank(),
            # axis.ticks = element_blank(),
            panel.grid = element_blank(),
            panel.border = element_blank(),
            legend.position="none") 
    
    if (i == 1) {
      p <- p + labs(x="",y="",title="Verification")
    } else {
      p <- p + labs(x="",y="",title=paste("Ens. Member", i-1))
    }
    fplots[[i]] <<- p  
  })

## rank hist (with full ties dropped)
# rank_tab <- read.table('data/rank_tab.RData')
df <- rank_tab %>% filter(s1==a1, s2==a2, tau==0)
p <- ggplot(df, aes(rank)) +
  geom_histogram(binwidth = 1, fill="gray10", color="white", size=0.2) +
  scale_x_continuous(breaks=seq(0,12,2), limits=c(0,13), expand=c(0.01,0.01)) +
  scale_y_continuous(expand = c(0.01,50)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size=12),
        axis.text = element_text(size=10),
        panel.grid = element_blank()) +
  labs(x="Rank",y="",title="FTE histogram")

fplots[[5]] <- p  

pdf('ver_ens_hist_23.pdf', width=12, height=4)
grid.arrange(arrangeGrob(grobs=fplots, nrow=1))
dev.off()




# Figure 3 ----------------------------------------------------------------
rm(list=ls())

## Beta param plot for a1 = 2
cont_fit_tab <- read.table('data/cont_fit_tab.RData')

a1 = 2

df <- cont_fit_tab %>% filter(tau==0, s1==a1)
param_a <- df %>% dplyr::select(s1, ratio, a) %>% mutate(value = a, param='a')
param_b <- df %>% dplyr::select(s1, ratio, b) %>% mutate(value = b, param='b')

pdf('beta_params.pdf', width=6, height=6)
ggplot(data=NULL, aes(x=log(ratio), y=value, color=param)) +
  geom_line(data=param_a, size=0.8) +
  geom_line(data=param_b, size=0.8) +
  scale_colour_manual(values=c(a="salmon", b="steelblue"), 
                      name="Param.", labels=c('shape1', 'shape2')) +
  labs(x="log ratio (a2/a1)", y="shape parameter") +
  theme_bw() +
  theme(strip.text.x = element_blank(),
        strip.background = element_rect(colour="white", fill="white"),
        legend.position=c(0.85,0.15)
  )
dev.off()
  






#
