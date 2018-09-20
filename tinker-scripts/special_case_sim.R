
## Look at rank hist for tau=0, rho=0, n=200, samps=5K for two cases:
##    1. s_2 = 0.5
##    2. all ensemble mems random noise (iid std normals)


library(RandomFields)
set.seed(7332)

## simulate data in batches to handle memory issue
## NOTE: to be run remotely
for (ii in 1:10) {

  print(paste("batch number: ", ii, sep=""))

  ## Setup sims
  source("get_data.R")
  build_noise <- function(range) {
    s_1 <- range[1]
    s_2 <- range[2]
    x <- y <- seq(-20, 20, 0.2)

    ## parameters
    smooth <- c(1.5, 1.5, 1.5)
    rng <- c(s_1, sqrt(s_1*s_2), s_2)
    var <- c(1, 1)
    rho <- 0

    ## model
    model_biwm <- RMbiwm(nu = smooth, s = rng, cdiag = var, rhored = rho)
    fields <- RFsimulate(model_biwm, x, y)

    ## noise
    noise <- replicate(200, rnorm(length(fields$variable1)))

    realization <- data.frame(fields$variable1, noise)
    names(realization) <- c("obs", paste("f", 1:200, sep = ""))
    return(realization)
  }

  get_noise <- function(samp_size, range) {
    data <- list()

    for (i in 1:samp_size) {
      print(i)
      fields <- build_noise(range = range)
      data[[i]] <- fields
    }

    return(data)
  }

  ## Simulate data for both cases
  short_range <- get_data(500, c(4,0.5), rho=0, n=200)
  save(short_range, file = paste("short_range_samples_0", ii, ".RData", sep=""))

  # random_noise <- get_noise(500, c(4,0.5))
  # save(random_noise, file = "random_noise_samples.RData")

  ## clear workspace to avoid memory overload
  rm(list = ls())

}

## NOTE: could use shell loop method like that used in ranking, but set would probably 
## be reset too often. Here it's just set once.
  
