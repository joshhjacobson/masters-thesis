
## Produce FTE histograms for downscaled GSDM data

library(ncdf4)
library(lubridate)
library(dplyr)
library(reshape2)
library(ggplot2)


# Analyses ----------------------------------------------------------------

## NOTE: dates range from 2002010200 to 2015123000

## grab analyses and time data
ncin <- nc_open('./data/gefs/original/refcstv2_precip_ccpav3_subset_066_to_072.nc')
obs <- ncvar_get(ncin, 'apcp_anal')

## create an array to store all each analysis-ensemble pair dim(lon, lat, time, obs/ens_n)
field_dat <- array(dim = c(dim(obs), 12))
field_dat[,,,1] <- obs

nc_close(ncin)
rm(ncin, obs)


# Ensembles (downscaled) --------------------------------------------------

## NOTES: Each array has space for 31 days, but 
#   - January 2002 starts from 01/02 (start timeframe one day earlier, and trim)
#   - April always has 30 days

m <- c(1, 4, 7, 10) # Jan, Apr, Jul, Oct
dates <- seq.Date(as.Date('2002-01-02'), as.Date('2015-12-30'), by='day')

files <- list.files(path='./data/gsdm', pattern='*.nc', full.names=TRUE)
lapply(seq_along(files), function(i) {
  ncin <- nc_open(files[i])
  fcsts <- ncvar_get(ncin, 'downscaled')
  
  # april
  if(m[i]==4){
    fcsts <- fcsts[,,,1:30,]
  }
  
  d <- dim(fcsts)
  dim(fcsts) <- c(d[1], d[2], d[3], d[4]*d[5])
  
  # january
  if(m[i]==1){
    # nil <- seq(31, dim(fcsts)[4], by=31)
    fcsts <- fcsts[,,, -31]
  }
  
  date_idx <- which(month(dates)==m[i])
  for(j in 1:11) {
    jj <- j+1
    # print(dim(field_dat[,, date_idx, jj]))
    # print(dim(fcsts[,,j,]))
    field_dat[,, date_idx, jj] <<- fcsts[,,j,]
  }
  
  nc_close(ncin)
  rm(ncin)
})
rm(files)

date_idx <- (month(dates) %in% m)
field_dat <- field_dat[,, date_idx,]


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

pdf("~/GitHub/random-fields/images/gsdm_downscaled_hists.pdf",
    width = 10, height = 8)
ranks_df %>%
  mutate(month = month(dates[date_idx])) %>%
  melt(id.vars='month', variable.name='tau', value.name='rank') %>%
  ggplot(aes(x=rank)) +
  geom_histogram(binwidth = 1, fill="steelblue", color="white", size=0.25) +
  scale_x_continuous(breaks=seq(0,12,4)) +
  facet_grid(tau ~ month) +
  labs(title='Method 4: GSDM Downscaled Forecasts')

dev.off()


