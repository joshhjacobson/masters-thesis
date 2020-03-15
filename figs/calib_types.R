
## plot flat / cup / cap shaped histograms

library("tidyverse")

n_samp <- 10000

## simulate data and check that it's reasonable
dat_flat <- rbeta(n_samp, 1, 1)
dat_cup <- rbeta(n_samp, 0.6, 0.6)
dat_cap <- rbeta(n_samp, 1.4, 1.4)


## create dataframe and melt 
dat_flat <- data.frame(dat_flat)
colnames(dat_flat) <- c("Calibrated")


dat <- cbind(dat_cup, dat_cap)
dat <- data.frame(dat)
colnames(dat) <- c("Underdispersed", "Overdispersed")
dat <- gather(dat)

## uniform hist
png("calib_flat.png", units="in", height=4, width=4, res=300, pointsize=10)

ggplot(dat_flat, aes(Calibrated)) +
  geom_histogram(aes(y=..density..), bins=14, fill="black", color="white") +
  scale_x_continuous(limits=c(0,1)) +
  scale_y_continuous(breaks = 1) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size=14),
        axis.title = element_blank(),
        axis.text = element_text(size = 14),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linetype="dashed", size=0.5),
        panel.grid.major.x = element_blank(),
        aspect.ratio = 1/1) +
  labs(x="", title="Calibrated")

dev.off()


## faceted histograms
png("calib_types.png", units="in", height=4, width=6, res=300, pointsize=10)

ggplot(dat, aes(value)) +
  geom_histogram(aes(y=..density..), bins=14, fill="black", color="white") +
  scale_x_continuous(limits=c(0,1)) +
  scale_y_continuous(breaks = 1) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size=12),
        axis.title = element_blank(),
        axis.text = element_text(size = 14),
        strip.background = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        strip.text.x = element_text(size = 14),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linetype="dashed", size=0.5),
        panel.grid.major.x = element_blank(),
        aspect.ratio = 1/1) +
  facet_wrap(.~key, nrow=1) +
  labs(x="")

dev.off()


## underdispersed with beta dist and params
dat_cup <- data.frame(rbeta(n_samp, 0.68, 0.73))
names(dat_cup) <- c("Underdispersed")

png("fte_demo.png", units="in", height=4, width=4, res=300, pointsize=10)

ggplot(dat_cup, aes(Underdispersed)) +
  geom_histogram(aes(y=..density..), bins=14, fill="black", color="white") +
  scale_x_continuous(limits=c(0,1)) +
  scale_y_continuous(breaks = 1, limits = c(0, 1.3)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size=14),
        axis.title = element_blank(),
        axis.text = element_text(size = 14),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "None",
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linetype="dashed", size=0.5),
        panel.grid.major.x = element_blank(),
        aspect.ratio = 1/1) +
  labs(x="", title="Underdispersed")

dev.off()


png("fte_demo_beta.png", units="in", height=4, width=4, res=300, pointsize=10)

ggplot(dat_cup, aes(Underdispersed)) +
  geom_histogram(aes(y=..density..), bins=14, fill="black", color="white") +
  stat_function(fun = dbeta, args = c(0.68, 0.73), color = "skyblue3") +
  scale_x_continuous(limits=c(0,1)) +
  scale_y_continuous(breaks = 1, limits = c(0, 1.3)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size=14),
        axis.title = element_blank(),
        axis.text = element_text(size = 14),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "None",
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linetype="dashed", size=0.5),
        panel.grid.major.x = element_blank(),
        aspect.ratio = 1/1) +
  labs(x="", title="Underdispersed (0.68, 0.73)")

dev.off()
