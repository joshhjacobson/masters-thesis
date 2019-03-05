
## Produce FTE histograms for calibrated GEFS reanalysis data
## Question: how are the forecasts downscaled if they are coarse-scale? Are they downscaled?

library(ncdf4)
library(lubridate)
library(dplyr)
library(reshape2)
library(ggplot2)


# Analyses ----------------------------------------------------------------

## NOTE: dates range from 2002010200 to 2015123000

## grab analyses and time data
ncin <- nc_open('./data/gefs/original/refcstv2_precip_ccpav3_subset_066_to_072.nc')
anal_upsc <- ncvar_get(ncin, 'apcp_anal_upsc')
init_anal <- ncvar_get(ncin, 'yyyymmddhh_init')

## get indices of data in upscaled region
lons_fcst <- ncvar_get(ncin, 'lons_fcst')
lats_fcst <- ncvar_get(ncin, 'lats_fcst')
lon_idx <- which(lons_fcst[,1] >= -91 &  lons_fcst[,1] <= -81)
lat_idx <- which(lats_fcst[1,] >= 30 &  lats_fcst[1,] <= 40)


## create an array to store all each analysis-ensemble pair dim(lon, lat, time, obs/ens_n)
field_dat <- array(dim = c(length(lon_idx), length(lat_idx), length(init_anal), 12))
## trim to same range as ensembles
field_dat[,,,1] <- anal_upsc[lon_idx, lat_idx, ]

nc_close(ncin)
rm(ncin, anal_upsc, init_anal, lats_fcst, lons_fcst)


# Ensembles (coarse-scale) ------------------------------------------------

i <- 1
files <- list.files(path='./data/gefs/calibrated', pattern='*.nc', full.names=TRUE)
lapply(files, function(f) {
  ncin <- nc_open(f)
  fcsts <- ncvar_get(ncin, 'forecasts')
  
  j <- i+dim(fcsts)[3]-1
  field_dat[,, i:j, 2:12] <<- fcsts[lon_idx, lat_idx, , ]

  i <<- i + dim(fcsts)[3]
  nc_close(ncin)
  rm(ncin)
})
rm(i, files, lon_idx, lat_idx)


# Threshold exceedence ranking --------------------------------------------

## compute the mean threshold exceedence of fields_df at array of thresholds 
## and return analysis ranks
exceed_ranks <- function(dat_arr, tau){
  # dat_arr: 3d array (lon x lat x member)
  # tau: vector of thresholds
  ranks <- array(dim= length(tau))
  for (i in 1:length(tau)) {
    m <- apply(dat_arr, 3, function(field) mean(as.vector(field) > tau[i]))
    ranks[i] <- rank(m, ties.method = "random")[1]
  }
  return(ranks)
}

## iterate over time, compute ranks at different thresholds 
tau <- c(5,10,20)
ranks_df <- data.frame(t(apply(field_dat, 3, exceed_ranks, tau=tau))) # (day x tau)


# FTE Histograms ----------------------------------------------------------

## stratify days by month and build histograms for each month-threshold pair
names(ranks_df) <- paste('tau',tau,sep='')

pdf("~/GitHub/random-fields/images/gefs_calibrated_hists.pdf",
    width = 10, height = 8)
ranks_df %>% 
  mutate(date = seq.Date(as.Date('2002-01-02'), as.Date('2015-12-30'), by='day'),
         month = month(date)) %>%
  select(-date) %>%
  melt(id.vars='month', variable.name='tau', value.name='rank') %>%
  ggplot(aes(x=rank)) +
  geom_histogram(binwidth = 1, fill="steelblue", color="white", size=0.25) +
  scale_x_continuous(breaks=seq(0,12,4)) +
  facet_grid(tau ~ month) +
  labs(title='Method 3: GEFS Calibrated Forecasts')

dev.off()




