
## Simulate 1K realizations for a variety of range values

s_1 <- 4
s_2 <- seq(1, 6, 0.5)
s_2 <-s_2[s_2 != 4] # don't need to look at same scale

for (ii in s_2) {
  paste("range param: ", ii)
  fields_data <- get_data(1000, c(s_1,ii))
  nam <- paste("fields_data_s4", ii, sep = "")
  save(fields_data,
       file = paste("~/GitHub/random-fields/data/fields_data_s",
                    s_1, ii, ".RData", sep = ""))
  
}
