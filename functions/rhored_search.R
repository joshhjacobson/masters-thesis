
## Function to numerically determine rho value that produces desired xi

## what value for rho yeilds RFcov-xi=0
## NOTE: set tolerance in uniroot?


rho_root <- function(rho, xi, smooth, rng, var) {
  model <- RMbiwm(nu = smooth, s = rng, cdiag = var, rhored = rho)
  return (RFcov(model, x=0)[1,1,2] - xi)
}

rhored_search <- function(xi, smooth, rng, var) {
  # xi (float): desired weight ratio between ensemble mean and perturbation
  # NOTE: other model parameters passed to RMbiwm() are assumed to be set and constant
  rhored <- uniroot(rho_root, c(xi, 1), xi=xi, smooth=smooth, rng=rng, var=var)$root
  return (rhored)
}


