
## check print out to test whether correlation is where it should be
## NOTE: printout gives correlation between fields var1 and var2
source("~/GitHub/random-fields/functions/get_data.R")

range_list <- seq(1,6,1)
for(ii in range_list){
  print(paste("range param: ", ii))
  range <- c(4,ii)
  get_data(10, range)
}