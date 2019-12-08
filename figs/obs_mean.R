
## example of obs and ens. mean
library(RandomFields)
library(fields)
library(tidyverse)
library(gridExtra)
library(RColorBrewer)

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


x <- y <- seq(-20, 20, 0.2)
a1 <- 2
a2 <- 2
xi <- 0.8; smooth <- c(1.5, 1.5, 1.5); var <- c(1, 1)

## simulate the verification and various ensemble means
rng <- c(a1, sqrt(a1*a2), a2)
rho <- rhored_search(xi, smooth, rng, var)

## model
set.seed(10)
model_biwm <- RMbiwm(nu=smooth, s=rng, cdiag=var, rhored=rho)
sim <- RFsimulate(model_biwm, x, y)


## plot the fields
dat <- expand.grid(x = x, y = y)

png("obs_mean.png", units="in", width=6, height=3.2, res=280, pointsize=9)
par(mfrow=c(1,2))
par(mar=c(3,3,4,2)+0.1)

dat["z"] <- sim$variable1
quilt.plot(dat, col=brewer.pal(9, "BuPu"), nx=200, ny=200, 
           zlim = range(sim$variable1) + c(-0.1, 0.1), add.legend=FALSE,
           main="Analysis")

dat["z"] <- sim$variable2
quilt.plot(dat, col=brewer.pal(9, "BuPu"), nx=200, ny=200, 
           zlim = range(sim$variable1) + c(-0.1, 0.1), add.legend=FALSE,
           main="Ensemble Mean")

dev.off()


## full set 
demo_ens_sim <- function(a1, a2) {
  ## grid
  x <- y <- seq(-20, 20, 0.2)
  ## model params
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
  omega <- RFsimulate(model_whittle, x, y, n=11)
  omega <- as.matrix(data.frame(omega))
  
  ensemble_mean <- replicate(11, sim$variable2)
  ensemble <- xi*ensemble_mean + sqrt(1-xi^2)*omega
  
  fields <- data.frame(sim$variable1, ensemble)
  return(fields)
} 


plot_binary <- function(fields, tau=1, inc_lab=TRUE) {
  
  # fields: observation and ensemble data formatted as cols of a dataframe
  # tau: masking threshold
  # inc_lab: include label?
  
  x <- y <- seq(-20, 20, 0.2)
  
  ## format data as xyz dataframe 
  dat <- expand.grid(x = x, y = y)
  dat["z"] <- fields[,1]
  
  ## collect each plot in a list
  fplots <- list()
  
  ## build plot list
  for (i in 1:ncol(fields))
    local({
      i <- i
      # update data being plotted
      dat$z <- fields[,i]
      p <- ggplot(dat, aes(x, y)) +
        geom_raster(aes(fill = z > tau)) +
        scale_fill_manual(values = c("TRUE" = "#08306B", "FALSE" = "#F7FBFF")) +
        theme_void() +
        theme(plot.title = element_text(hjust = 0.5, size=12, face="bold"), # center title
              axis.text = element_blank(),
              axis.ticks = element_blank(),
              legend.position="none",
              aspect.ratio = 1/1) 
      
      if (!inc_lab){
        p <- p + labs(x="",y="",title="")
      } else {
        if (i == 1) {
          p <- p + labs(x="",y="",title="Analysis")
        } else {
          p <- p + labs(x="",y="",title=paste("Forecast ", i-1, sep = ""))
        }
      }
      # add each plot into plot list
      fplots[[i]] <<- p  
    })
  
  return(fplots)
  
}

fields <- demo_ens_sim(a1, a2)
plist <- plot_binary(fields)

png("full_set.png", units="in", height=6.2, width=6, res=200, pointsize=10)

grid.arrange(
  arrangeGrob(grobs=plist, ncol=4)
)

dev.off()
