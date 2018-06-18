
## Use Bi-variate Whittle-Matern model to generate ensemble of
## 11 forecasts perturbed from mean 

## Study variation in spatial structure using different values 
## for xi in model

library(RandomFields)
quartz() # external graphics

#RFoptions(seed = 0)
#RFoptions(seed = NA)


# Parameters --------------------------------------------------------

smooth <- c(1.5, 1.5, 1.5)
scale <- c(0.5, 0.5, 0.5)
corr <- c(1, 1)
rho <- 0.8

xi <- 0.8 # used to maintain std variance in forecasts

x <- y <- seq(-20, 20, 0.2)



# Ensemble construction ---------------------------------------------------

model_whittle <- RMwhittle(nu = 1.5, notinvnu = TRUE, scale = 0.5, var = 1)
# plot(RFsimulate(model_whittle, x, y))

omega <- replicate(11, RFsimulate(model_whittle, x, y)$variable1)



# Model construction ------------------------------------------------------

model_biwm <- RMbiwm(nu = smooth, s = scale, cdiag = corr, rhored = rho) 

plot(model_biwm)
plot(RFsimulate(model_biwm, x, y))

# separeate random fields into observation and forecast
fields <- RFsimulate(model_biwm, x, y)

# obs
Y <- fields$variable1
# forecast
zbar <- fields$variable2


# build ensemble as matrix where each column is a realization
zbar <- replicate(11, zbar)
ensemble <- xi*zbar + sqrt(1-xi^2)*omega



# Visualization -----------------------------------------------------------

# plot(RFsimulate(model_biwm, x, y)$variable2 + RFsimulate(model_whittle, x, y)$variable1)

for (index in 1:ncol(ensemble)) {
  fields$variable2 <- ensemble[,index]
  plot(fields)
}







