
## Function to process data frame columns (single field realization) 
## through exponential transformation

library(dplyr)
library(tibble)

exp_margin <- function(field) {
  
  # field: data from with obs and forecasts as columns
  tb <- as_tibble(field) %>%
    mutate_all(funs(qexp(pnorm(.))))
  
  return(data.frame(tb))
}