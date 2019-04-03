
## Create standard PIT histograms from calibrated forcasts with upscaled analyses

library(lubridate)
library(dplyr)
library(reshape2)
library(ggplot2)

## import fields_dat dim(lat x lon x time x mem)
load('./data/gefs/gefs_calibrated_fields.RData')

m <- c(1, 4, 7, 10) # Jan, Apr, Jul, Oct
dates <- seq.Date(as.Date('2002-01-02'), as.Date('2015-12-30'), by='day')
dates <- dates[(month(dates) %in% m)]


# M1: Include complete random cases ---------------------------------------

## iterate over time, compute pointwise ranks across members
ranks_arr <- apply(field_dat, c(3,1,2), 
                   function(point) rank(point, ties.method = "random")[1])


## tidy data
ranks_df <- data.frame(t(matrix(ranks_arr, dim(ranks_arr)[1], prod(dim(ranks_arr)[2:3]))))
colnames(ranks_df) <- dates
ranks_df <- melt(ranks_df, variable.name='date', value.name='rank')


## stratify days by month and build histograms
pdf("~/GitHub/random-fields/images/pit_calibrated_wt_hists.pdf",
    width = 10, height = 4)
ranks_df %>% 
  mutate(month = month(date)) %>%
  ggplot(aes(x=rank)) +
  geom_histogram(binwidth = 1, fill="steelblue", color="white", size=0.25) +
  scale_x_continuous(breaks=seq(0,12,4)) +
  facet_wrap(~month, ncol=4) +
  labs(title='Standard PIT histogram: calibrated forecasts, full-ties kept')
dev.off()
rm(ranks_arr, ranks_df)


# M2: Remove complete random cases ----------------------------------------

## iterate over time, compute pointwise ranks across members
ranks_arr <- apply(field_dat, c(3,1,2), 
                   function(point){
                     if(length(unique(point)) == 1) { 
                       return(NA)
                     }
                     return(rank(point, ties.method = "random")[1])
                   })


## tidy data
ranks_df <- data.frame(t(matrix(ranks_arr, dim(ranks_arr)[1], prod(dim(ranks_arr)[2:3]))))
colnames(ranks_df) <- dates

rand_arr <- array(dim = c(length(dates), 2))
rand_arr[,1] <- apply(ranks_df, 2, function(col) sum(is.na(col)))
rand_arr[,2] <- apply(ranks_df, 2, function(col) length(col))
colnames(rand_arr) <- c('count', 'total')

rand_count_df <- data.frame(date=dates, rand_arr) %>%
  mutate(m = month(date)) %>%
  select(-date) %>%
  group_by(m) %>%
  summarise(r_percent = sum(count)/sum(total) * 100)

ranks_df <- melt(ranks_df, variable.name='date', value.name='rank')

## create labels for facets
rp <- c(
  '1' = paste('Jan, ', round(rand_count_df[1,2], 2), '%', sep=''),
  '4' = paste('Apr, ', round(rand_count_df[2,2], 2), '%', sep=''),
  '7' = paste('Jul, ', round(rand_count_df[3,2], 2), '%', sep=''),
  '10' = paste('Oct, ', round(rand_count_df[4,2], 2), '%', sep='')
)

## stratify days by month and build histograms
pdf("~/GitHub/random-fields/images/pit_calibrated_wot_hists.pdf",
    width = 10, height = 4)
ranks_df %>% 
  mutate(month = month(date)) %>%
  ggplot(aes(x=rank)) +
  geom_histogram(binwidth = 1, fill="steelblue", color="white", size=0.25) +
  scale_x_continuous(breaks=seq(0,12,4)) +
  facet_wrap(~month, ncol=4, labeller=as_labeller(rp)) +
  labs(title='Standard PIT histogram: calibrated forecasts, full-ties removed')
dev.off()

