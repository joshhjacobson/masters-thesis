

## transform scheuerer stat tables into flattened tibble 
library(tibble)
library(reshape2)
library(dplyr)

s_1 <- seq(1,4,0.5)

## read all tables into list
dat <- list()
nam <- paste("rank_stats_s", s_1, sep = "")
for (ii in 1:length(nam)){
  print(nam[ii])
  ## load rank_stats
  dat[[ii]] <- read.table(paste("~/GitHub/random-fields/data/", nam[ii], ".RData", sep = ""))
}

scheuerer_dat <- data.frame()
for (ii in 1:length(dat)) {
  
  ## read and format
  df <- dat[[ii]]
  s_2 <- seq(0.5*s_1[ii], 1.5*s_1[ii], 0.1*s_1[ii])
  names(df) <- s_2
  
  ## flatten and mutate
  df <- melt(df)
  df$rowid <- seq(0, 4, 0.5)
  df <- mutate(df, ratio = as.numeric(levels(variable))[variable] / s_1[ii])
  df$s1 <- s_1[ii]
  
  ## append and remove
  scheuerer_dat <- rbind(scheuerer_dat, df)
  rm(df)
}

## write out scheuerer_dat
write.table(as_tibble(scheuerer_dat), file="~/GitHub/random-fields/data/scheuerer_dat.RData")



