
## get true xi values for each range
compute_xi <- function(range) {
  # range (list of 2): scale parameters for observation and ensemble; c(s_1, s_2)
  s_1 <- range[1]
  s_2 <- range[2]
  
  ## parameters
  smooth <- c(1.5, 1.5, 1.5)            #nu: smoothnes / differentiability
  rng <- c(s_1, sqrt(s_1*s_2), s_2)     #range: s = 1/a  
  var <- c(1, 1)                        #variances
  rho <- 0.8                            #rho: percent cross correlation 
  
  ## model
  model_biwm <- RandomFields::RMbiwm(nu = smooth, s = rng, cdiag = var, rhored = rho)
  cov_mat <- RandomFields::RFcov(model_biwm, x=0)
  return(round(cov_mat[1,1,2], 3))
}

## Comupte xi values at all ranges with biwm model
get_xi <- function(s_2) {
  xi_list <- c()
  for (r in 1:length(s_2)) {
    xi_list[r] <- compute_xi(c(4,s_2[r]))
  }
  return(xi_list)
}
