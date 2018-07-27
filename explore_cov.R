model <- RMbiwm(nudiag=c(1, 2), nured=1, rhored=1, cdiag=c(1, 5),
                s=c(1, 1, 2))
x <- seq(0, 20, 0.1)
z <- RFsimulate(model, x=x, y=x, n=1)
emp.vario <- RFempiricalcovariance(data=z)
plot(emp.vario, model=model)

c <- RFcov(model, x=x)
