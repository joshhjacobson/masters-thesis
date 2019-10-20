
## Demonstrate dependence on domain size by analyzing range of ratios for a1 = 1,3 and tau fixed at 1

library(tidyverse)
library(reshape2)

## Beta param plot for a1 = 2
cont_fit_tab <- read.table('../data/cont_fit_tab.RData')

df <- cont_fit_tab %>% 
  filter((tau==1 & s1==1) |  (tau==1 & s1==3)) %>% 
  select(-tau) %>% 
  melt(id.vars=c("s1", "ratio"))

range_labs <- c(
  `1` = "a1 = 1",
  `3` = "a1 = 3"
)

png("beta_params_range.png", units="in", height=3.3, width=6.4, res=200, pointsize=9)

ggplot(data=df, aes(x=log(ratio), y=value, color=variable)) +
  facet_grid(~s1, labeller = as_labeller(range_labs)) +
  geom_line() + 
  scale_colour_manual(values=c(a="red", b="slateblue3")) +
  labs(x="log(ratio)", y="parameter") +
  theme_bw() +
  theme(legend.title = element_blank(),
        strip.background = element_blank(),
        text = element_text(color="black"),
        strip.text= element_text(size=12),
        axis.text = element_text(size=9),
        legend.text = element_text(size=10),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linetype="dashed", size=0.2),
        aspect.ratio = 1/1,
        plot.margin = unit(c(0,0,0,0), "cm"))

dev.off()

