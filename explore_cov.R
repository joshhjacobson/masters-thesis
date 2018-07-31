
library(RandomFields)

s_1 <- 4
s_2 <- 5

smooth <- c(1.5, 1.5, 1.5)            #nu: smoothnes
rng <- c(s_1, sqrt(s_1*s_2), s_2)     #range: s = 1/a  
var <- c(1, 1)                        #variances
rho <- 1  

# test_model <- RMbiwm(nudiag=c(1, 2), nured=1, rhored=1, cdiag=c(1, 5),
#                 s=c(1, 1, 2))
test_model <- RMbiwm(nu = smooth, s = rng, cdiag = var, rhored = rho)
plot(test_model)

x <- seq(0, 20, 0.1)
z <- RFsimulate(test_model, x=x, y=x, n=1)

emp.vario <- RFempiricalcovariance(data=z)
plot(emp.vario, model=test_model)

c <- RFcov(test_model, x=x)
c[1,,]


covinfo <- RFgetModelInfo(level=3)
print((covinfo))
