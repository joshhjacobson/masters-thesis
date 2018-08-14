
## Script to transform fields to exponential margins

source("~/GitHub/random-fields/functions/exp_margin.R")

s_2 <- seq(1,6,0.5)
nam <- paste("fields_data_rho0_s4", s_2, sep = "")

for (ii in 1:length(nam)) {
  
  print(paste("range param: ", s_2[ii], sep = ""))
  
  load(paste("/Volumes/My\ Passport/Forecasting\ Data/fields_rho0/", 
             nam[ii], ".RData", sep=""))
  data <- fields_data
  
  ## preform tansform
  exp_dat <- lapply(data, exp_margin)
  
  save(exp_dat, file = 
         paste("/Volumes/My\ Passport/Forecasting\ Data/transformed/fields_rho0/exp_fields_rho0_s4", 
               s_2[ii], ".RData", sep=""))
  
}