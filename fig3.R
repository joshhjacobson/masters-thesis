

library(dplyr)


## Beta param plot for a1 = 2
cont_fit_tab <- read.table('data/cont_fit_tab.RData')

a1 = 2

df <- cont_fit_tab %>% filter(tau==0, s1==a1)
param_a <- df %>% dplyr::select(s1, ratio, a) %>% mutate(value = a, param='a')
param_b <- df %>% dplyr::select(s1, ratio, b) %>% mutate(value = b, param='b')


png("beta_params.png", units="in", height=5, width=4.8, res=200, pointsize=10)
par(mar=c(4,4,1,2)+0.1)

plot(log(param_a$ratio), param_a$value, type="l", lwd=2, col="red",
     xlab="log(a2/a1)", ylab="shape parameter")
grid()
lines(log(param_a$ratio), param_a$value, lwd=2, col="red")
lines(log(param_b$ratio), param_b$value, lwd=2, col="slateblue3")
legend("bottomright", c("shape 1", "shape 2"), col=c("red", "slateblue3"), 
       lwd=2, bty="n", cex=1, inset=c(0.1, 0.05))

dev.off()

