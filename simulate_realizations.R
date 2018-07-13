
## Simulate 1K realizations for a variety of range values

## Local 
# source("~/GitHub/random-fields/functions/get_data.R")

## Remote
source("get_data.R")

s_1 <- 4
s_2 <- seq(1, 6, 0.5)
#s_2 <-s_2[s_2 != 4] # don't need to look at same scale

for (ii in s_2) {
  print(paste("range param: ", ii))
  fields_data <- get_data(1000, c(s_1,ii))
  nam <- paste("fields_data_s4", ii, sep = "")
  
  ## Local
  # save(fields_data,
  #      file = paste("~/GitHub/random-fields/data/fields_data_s",
  #                   s_1, ii, ".RData", sep = ""))
  
  ## Remote
  save(fields_data,
       file = paste("fields_data_s", s_1, ii, ".RData", sep = ""))
  
}
