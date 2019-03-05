
## Produce FTE histograms for upscaled GEFS reanalysis data

library(ncdf4)
library(lubridate)
library(dplyr)
library(reshape2)
library(ggplot2)

ncin <- nc_open('./data/gefs/original/refcstv2_precip_ccpav3_subset_066_to_072.nc')
# print(ncin)

apcp_fcst_ens <- ncvar_get(ncin, 'apcp_fcst_ens')
apcp_anal_upsc <- ncvar_get(ncin, 'apcp_anal_upsc')
lons_fcst <- ncvar_get(ncin, 'lons_fcst')
lats_fcst <- ncvar_get(ncin, 'lats_fcst')

lon_idx <- which(lons_fcst[,1] >= -91 &  lons_fcst[,1] <= -81)
lat_idx <- which(lats_fcst[1,] >= 30 &  lats_fcst[1,] <= 40)

a_trim <- apcp_anal_upsc[lon_idx, lat_idx, ]
e_trim <- apcp_fcst_ens[lon_idx, lat_idx, , ]

nc_close(ncin)

# library(fields)
# library(maps)
# map('state')
# image.plot(lons_eff,lats_eff,apcp_fcst_ens[lon_idx,lat_idx,1,16])
# image.plot(lons_fcst[lon_idx,1],lats_fcst[1,lat_idx],e_eff[,,1,16])
# map('state',add=TRUE)

## Gather the analysis and ensemble for given day into a data frame
gather_df <- function(D, A, E) {
  # D: day index
  # A: analysis array
  # E: ensemble array
  A <- A[,,D]
  dim(A) <- NULL
  E <- E[,,,D]
  dim(E) <- c(nrow(E)*ncol(E), 11) ## is this correct?
  E <- matrix(E, nrow(E)*ncol(E), 11, byrow=FALSE) ## double check by looking at random lat lon cord
  return(data.frame(A, E))
}

## Compute the mean threshold exceedence of fields_df at array of thresholds 
## and return analysis rank
exceedence_ranks <- function(fields_df, tau) {
  # fields_df: analysis and ensemble in cols of data frame
  # tau: threshold array
  ranks <- array(dim= length(tau))
  for (i in 1:length(tau)) {
    m <- array(colMeans(fields_df > tau[i]))
    ranks[i] <- rank(m, ties.method = "random")[1]
  }
  return( ranks )
}


## Loop over days and compute ranks at different thresholds 
tau <- c(5,10,20)
n_days <- length(a_trim[1,1,])
ranks_arr <- array(dim = c(n_days, length(tau)))
for (d in 1:n_days){
  fields_df <- gather_df(d, a_trim, e_trim)
  ranks_arr[d,] <- exceedence_ranks(fields_df, tau)
}


## Stratify days by month and build histograms for each month-threshold pair
ranks_df <- data.frame(ranks_arr)
names(ranks_df) <- paste('tau',tau,sep='')

pdf("~/GitHub/random-fields/images/gefs_upsc_hists.pdf",
    width = 10, height = 8)
ranks_df %>% 
  mutate(date = seq.Date(as.Date('2000-01-01'), by='day', length.out=nrow(ranks_df)),
         month = month(date)) %>%
  select(-date) %>%
  melt(id.vars='month', variable.name='tau', value.name='rank') %>%
  ggplot(aes(x=rank)) +
    geom_histogram(binwidth = 1, fill="steelblue", color="white", size=0.25) +
    scale_x_continuous(breaks=seq(0,12,4)) +
    facet_grid(tau ~ month) +
    labs(title='Method 1: Upscaled Analyes')

dev.off()

