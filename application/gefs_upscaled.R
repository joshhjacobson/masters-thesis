
## Produce FTE histograms for upscaled GEFS reanalysis data

library(ncdf4)
library(lubridate)
library(dplyr)
library(reshape2)
library(ggplot2)


# Fcst + Analysis ---------------------------------------------------------

## NOTE: dates range from 2002010200 to 2015123000

## grab field and time data
ncin <- nc_open('./data/gefs/original/refcstv2_precip_ccpav3_subset_066_to_072.nc')
fcst_ens <- ncvar_get(ncin, 'apcp_fcst_ens')
anal_upsc <- ncvar_get(ncin, 'apcp_anal_upsc')
init_anal <- ncvar_get(ncin, 'yyyymmddhh_init')

## get indices of data in upscaled region
lons_fcst <- ncvar_get(ncin, 'lons_fcst')
lats_fcst <- ncvar_get(ncin, 'lats_fcst')
lon_idx <- which(lons_fcst[,1] >= -91 &  lons_fcst[,1] <= -81)
lat_idx <- which(lats_fcst[1,] >= 30 &  lats_fcst[1,] <= 40)

## create an array to store each analysis-ensemble pair dim(lon, lat, time, obs/ens_n)
field_dat <- array(dim = c(length(lon_idx), length(lat_idx), length(init_anal), 12))

## populate and trim to same regions
field_dat[,,,1] <- anal_upsc[lon_idx, lat_idx, ]
field_dat[,,,2:12] <- fcst_ens[lon_idx, lat_idx, , ]

nc_close(ncin)
rm(ncin, anal_upsc, fcst_ens, init_anal, lats_fcst, lons_fcst, lat_idx, lon_idx)

library(fields)
library(maps)
map('state')
image.plot(lons_fcst, lats_fcst, fcst_ens[lon_idx,lat_idx,1,16])
image.plot(lons_fcst[lon_idx,1], lats_fcst[1,lat_idx], fcst_ens[,,1,16])
map('state',add=TRUE)
image.plot(field_dat[,,16,2], col=brewer.pal(9, "BuPu"), nx=200, ny=200)


# Threshold exceedence ranking --------------------------------------------

m <- c(1, 4, 7, 10) # Jan, Apr, Jul, Oct
dates <- seq.Date(as.Date('2002-01-02'), as.Date('2015-12-30'), by='day')
date_idx <- (month(dates) %in% m)
field_dat <- field_dat[,, date_idx,] 

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

pdf("~/GitHub/random-fields/images/gefs_upsc_hists.pdf",
    width = 10, height = 8)
ranks_df %>% 
  mutate(date = dates[date_idx],
         month = month(date)) %>%
  select(-date) %>%
  melt(id.vars='month', variable.name='tau', value.name='rank') %>%
  ggplot(aes(x=rank)) +
  geom_histogram(binwidth = 1, fill="steelblue", color="white", size=0.25) +
  scale_x_continuous(breaks=seq(0,12,4)) +
  facet_grid(tau ~ month) +
  labs(title='Method 1: GEFS Upscaled Analyses')

dev.off()

