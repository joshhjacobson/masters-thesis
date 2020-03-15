
library(tidyverse)
library(latex2exp)
library(grid)
library(gridExtra)

# Get ranks data
rank_tab <- read.table('../data/rank_tab.RData')


# five histograms per ratio (0.5, 1.1?): fix ratio, plot hist for threshold in (0,1,2,3,4), include percent dropped (or at least number)
tau_labs <- c(
  '0' = TeX(paste("$\\tau = 0$")),
  '1' = TeX(paste("$\\tau = 1$")),
  '2' = TeX(paste("$\\tau = 2$")),
  '3' = TeX(paste("$\\tau = 3$")),
  '4' = TeX(paste("$\\tau = 4$"))
)

# ratio_labs <- c(
#   '0.5' = "ratio = 0.5",
#   '1.1' = "ratio = 1.1"
# )


df <- rank_tab %>%
  mutate(rank = (rank-0.5)/12,
         ratio=s2/s1) %>%
  dplyr::filter(s1==2, ratio==0.5, tau %in% c(0,1,2,3,4)) %>%
  mutate_at(vars(tau), funs(factor))

df_tau4 <- df %>% filter(tau==4) 
sum(is.na(df_tau4$rank)) # 1996 nans

levels(df$tau) <- tau_labs
# levels(df$ratio) <- ratio


png("hists_over_tau_r05.png", units="in", height=1.8, width=8, res=300, pointsize=10)
df %>%
  ggplot(aes(x=rank)) +
    geom_hline(yintercept=1, linetype=3, size=0.3, color="grey") +
    geom_histogram(aes(y=..density..), bins=12, fill="black", color="white") +
    facet_wrap(~tau, nrow=1, labeller=label_parsed) +
    labs(y="", x="") +
    theme_bw() +
    theme(legend.title = element_blank(),
          strip.background = element_blank(),
          text = element_text(color="black"),
          strip.text= element_text(size=12, face="bold"),
          axis.text = element_text(size=9, color="black"),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.major = element_blank(),
          aspect.ratio = 1/1,
          plot.margin = unit(c(0,0.1,0,0), "cm"))

dev.off()


df <- rank_tab %>%
  mutate(rank = (rank-0.5)/12,
         ratio=s2/s1) %>%
  dplyr::filter(s1==2, ratio==1.1, tau %in% c(0,1,2,3,4)) %>%
  mutate_at(vars(tau), funs(factor))

df_tau3 <- df %>% filter(tau==3) 
df_tau4 <- df %>% filter(tau==4) 
sum(is.na(df_tau3$rank)) # 187 nans
sum(is.na(df_tau4$rank)) # 3990 nans

levels(df$tau) <- tau_labs

# 1996 nans
png("hists_over_tau_r11.png", units="in", height=1.8, width=8, res=300, pointsize=10)
df %>%
  ggplot(aes(x=rank)) +
  geom_hline(yintercept=1, linetype=3, size=0.3, color="grey") +
  geom_histogram(aes(y=..density..), bins=12, fill="black", color="white") +
  facet_wrap(~tau, nrow=1, labeller=label_parsed) +
  labs(y="", x="") +
  theme_bw() +
  theme(legend.title = element_blank(),
        strip.background = element_blank(),
        text = element_text(color="black"),
        strip.text= element_text(size=12, face="bold"),
        axis.text = element_text(size=9, color="black"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major = element_blank(),
        aspect.ratio = 1/1,
        plot.margin = unit(c(0,0.1,0,0), "cm"))

dev.off()
  


# five histograms: a0=1 case and ratios from aM=0.5,0.9,1,1.1,1.5
am_vec <- c(0.5, 0.9, 1.0, 1.1, 1.5)

ratio_labs <- c(
  '0.5' = "ratio = 0.5",
  '0.9' = "ratio = 0.9",
  '1' = "ratio = 1.0",
  '1.1' = "ratio = 1.1",
  '1.5' = "ratio = 1.5"
)

png("hists_over_ratio.png", units="in", height=1.8, width=8, res=300, pointsize=10)

rank_tab %>% 
  dplyr::filter(s1==1 & s2 %in% am_vec & tau==1) %>%
  mutate(rank = (rank-0.5)/12) %>%
  ggplot(aes(x=rank)) +
    geom_hline(yintercept=1, linetype=3, size=0.3, color="grey") +
    geom_histogram(aes(y=..density..), bins=12, fill="black", color="white") +
    facet_wrap(~s2, nrow=1, labeller=as_labeller(ratio_labs)) +
    labs(y="", x="") +
    theme_bw() +
    theme(legend.title = element_blank(),
          strip.background = element_blank(),
          text = element_text(color="black"),
          strip.text= element_text(size=12, face="bold"),
          axis.text = element_text(size=9, color="black"),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.major = element_blank(),
          aspect.ratio = 1/1,
          plot.margin = unit(c(0,0.1,0,0), "cm"))

dev.off()


