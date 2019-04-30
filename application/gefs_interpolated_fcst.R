
## Interpolate coarse grid GEFS forecasts to resolution of analyses
## and produce FTE histograms

library(ncdf4)
library(raster)
library(lubridate)
library(dplyr)
library(reshape2)
library(ggplot2)


# Analysis ----------------------------------------------------------------

## NOTE: dates range from 2002010200 to 2015123000

## grab field and time data
ncin <- nc_open('./data/gefs/original/refcstv2_precip_ccpav3_subset_066_to_072.nc')
apcp_anal <- ncvar_get(ncin, 'apcp_anal')

## subset data to same region
lons_anal <- ncvar_get(ncin, 'lons_anal')
lats_anal <- ncvar_get(ncin, 'lats_anal')
lon_idx <- which(lons_anal[,1] >= -91 &  lons_anal[,1] <= -81)
lat_idx <- which(lats_anal[1,] >= 30 &  lats_anal[1,] <= 40)
apcp_anal <- apcp_anal[lon_idx, lat_idx, ]
rm(lons_anal, lats_anal, lon_idx, lat_idx)

lons_fcst <- ncvar_get(ncin, 'lons_fcst')
lats_fcst <- ncvar_get(ncin, 'lats_fcst')
lon_idx <- which(lons_fcst[,1] >= -91 &  lons_fcst[,1] <= -81)
lat_idx <- which(lats_fcst[1,] >= 30 &  lats_fcst[1,] <= 40)
rm(lons_fcst, lats_fcst)

nc_close(ncin)
rm(ncin)

## create an array to store each analysis-ensemble pair dim(values, time, obs/ens_n)
field_dat <- array(dim = c(prod(dim(apcp_anal)[1:2]), dim(apcp_anal)[3], 12))

## convert analysis fields to vectors and populate field_dat
field_dat[,,1] <- matrix(apcp_anal, prod(dim(apcp_anal)[1:2]), dim(apcp_anal)[3])


# Forecasts ---------------------------------------------------------------

## store coars grid ensembles dim(lon x lat x date x mem)
fcst_ens <- array(dim = c(length(lon_idx), length(lat_idx), dim(apcp_anal)[3], 11))

i <- 1
files <- list.files(path='./data/gefs/calibrated', pattern='*.nc', full.names=TRUE)
lapply(files, function(f) {
  ncin <- nc_open(f)
  fcsts <- ncvar_get(ncin, 'forecasts')
  
  j <- i+dim(fcsts)[3]-1
  fcst_ens[,, i:j, ] <<- fcsts[lon_idx, lat_idx, , ]
  
  i <<- i + dim(fcsts)[3]
  nc_close(ncin)
  rm(ncin)
})
rm(i, files, lon_idx, lat_idx)


# Interpolation -----------------------------------------------------------

m <- c(1, 4, 7, 10) # Jan, Apr, Jul, Oct
dates <- seq.Date(as.Date('2002-01-02'), as.Date('2015-12-30'), by='day')
date_idx <- (month(dates) %in% m)
field_dat <- field_dat[,date_idx,] 
apcp_anal <- apcp_anal[,,date_idx]
fcst_ens <- fcst_ens[,,date_idx,]

## iterate over ensemble members and time to downscale forecasts
for (mem in 1:11){
  print(paste('ENSEMBLE MEM:', mem))
  for(t in 1:dim(apcp_anal)[3]){
    # downscale forecast to same res. as analysis using bilinear interp.
    field_dat[,t,mem+1] <- resample(raster(fcst_ens[,,t,mem]), raster(apcp_anal[,,t]))@data@values
  }
  save(field_dat, file='gefs_interp_fcsts.RData')
}
rm(apcp_anal, fcst_ens)

# A <- raster(apcp_anal[,,16])
# plot(A)
# E <- raster(fcst_ens[,,1,16])
# plot(E)
# 
# ## test <- disaggregate(E, fact=c(dim(A)[1]/dim(E)[1], dim(A)[2]/dim(E)[2]), method='bilinear')
# test <- resample(E, A)
# plot(test)
# test@data@values


# Threshold exceedence ranking --------------------------------------------

## compute the mean threshold exceedence of fields_df at array of thresholds
## and return analysis ranks
exceed_ranks <- function(dat_arr, tau){
  # dat_arr: 2d array (field x member)
  # tau: vector of thresholds
  ranks <- array(dim= length(tau))
  for (i in 1:length(tau)) {
    m <- apply(dat_arr, 2, function(field) mean(field > tau[i]))
    ranks[i] <- rank(m, ties.method = "random")[1]
  }
  return(ranks)
}

## iterate over time, compute ranks at different thresholds
tau <- c(5,10,20)
ranks_df <- data.frame(t(apply(field_dat, 2, exceed_ranks, tau=tau))) # (day x tau)


# FTE Histograms ----------------------------------------------------------

## stratify days by month and build histograms for each month-threshold pair
names(ranks_df) <- paste('tau',tau,sep='')

pdf("~/GitHub/random-fields/images/gefs_calib_interp_hists.pdf",
    width = 10, height = 8)
ranks_df %>% 
  mutate(date = dates[date_idx],
         month = month(date)) %>%
  dplyr::select(-date) %>%
  melt(id.vars='month', variable.name='tau', value.name='rank') %>%
  ggplot(aes(x=rank)) +
  geom_histogram(binwidth = 1, fill="steelblue", color="white", size=0.25) +
  scale_x_continuous(breaks=seq(0,12,4)) +
  facet_grid(tau ~ month) +
  labs(title='Method 3: GEFS calibrated, interpolated forecasts')

dev.off()


