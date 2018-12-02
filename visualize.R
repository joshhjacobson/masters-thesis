

## Script to build a grid of actual field visualizations


# Sim. Functions --------------------------------------------------------------

## simulate 10 observations for each s1
library(RandomFields)

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

## Function to construct single realization
build_ensemble <- function(rng, rhored, xi=0.8, smooth=c(1.5, 1.5, 1.5), var=c(1, 1), n=11) {
  
  # arr_dat (array): 4D array storing exceedence data
  # index: realization number and s2 id
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
  names(realization) <- c("obs", paste("ens", 1:n, sep = ""))
  
  return(realization)
}

## Function to build a list of ensemble data frames
get_data <- function(samp_size, range, xi=0.8, smooth=c(1.5, 1.5, 1.5), var=c(1, 1), n=3) {
  
  # samp_size: number of realizations to be simulated 
  # ...: parameters passed to build_ensemble()
  
  s_1 <- range[1]
  s_2 <- range[2]
  rng <- c(s_1, sqrt(s_1*s_2), s_2)  #geometric mean
  
  ## determine rho for desired xi at given range
  rhored <- rhored_search(xi, smooth, rng, var)
  
  ## collect realizations in a list
  fields_list <- list()
  
  for (i in 1:samp_size) {
    print(i)
    fields <- build_ensemble(rng=rng, rhored=rhored, xi=xi, n=n)
    fields_list[[i]] <- fields
  }
  return(fields_list)
}


# Simulate & Plot --------------------------------------------------------------

library(cowplot)
library(grid)
library(gridExtra)
source("~/GitHub/random-fields/functions/plot_fields.R")
source("~/GitHub/random-fields/functions/plot_binary.R")
# source("~/GitHub/random-fields/functions/grid_arrange_shared_legend.R")

set.seed(0) 
## ideally, would set seed in the same way as in sims, 
## but this won't work with current setup below
s_1 <- seq(1,4,0.5)
N <- 10


pdf("~/GitHub/random-fields/images/fields.pdf")
for (ii in 1:length(s_1)) {
  s_2 <- c(0.5*s_1[ii], 1.5*s_1[ii])
  for (k in 1:N) {
    print(paste(k, "range:", s_1[ii], sep=" "))
    g <- list()
    for (jj in 1:length(s_2)) {
      local({
        ii <- ii
        jj <- jj
        rng <- c(s_1[ii], sqrt(s_1[ii]*s_2[jj]), s_2[jj])  #geometric mean
        rhored <- rhored_search(xi=0.8, smooth=c(1.5, 1.5, 1.5), rng=rng, var=c(1, 1))
        fields <- build_ensemble(rng=rng, rhored=rhored, n=3)
        if(jj==2) {
          f_plots <- plot_fields(fields, inc_lab=FALSE)
        } else {
          f_plots <- plot_fields(fields)
        }
        b_plots <- plot_binary(fields, inc_lab=FALSE)
        # f <- grid_arrange_shared_legend(f_plots, position = "right")
        f <- plot_grid(plotlist = f_plots, ncol=4)
        b <- plot_grid(plotlist = b_plots, ncol=4)
        g[[jj]] <<- arrangeGrob(f,b, nrow=2, left=textGrob(
          label=paste("s2=",s_2[jj],sep=""), 
          gp=gpar(fontsize=12), 
          rot=90))
      })
    }
    grid.arrange(
      arrangeGrob(grobs=g, nrow=2, top=textGrob(
        label=paste("s1=",s_1[ii],sep=""), 
        gp=gpar(fontsize=12)))
    )
  }
}
dev.off()






