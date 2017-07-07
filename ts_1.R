setwd('/Users/djl358/Documents/new_search/script/')

x <- read.csv("quakes.csv", header =1)
x <- ts(x[,2])

acf(x, xlim=c(1,19))
xlag1=lag(x,-1)
y=cbind(x,xlag1) 
ar1fit=lm(y[,1]~y[,2])#Does regression, stores results object named ar1fit
summary(ar1fit) # This lists the regression results
plot(ar1fit$fit,ar1fit$residuals) #plot of residuals versus fits
acf(ar1fit$residuals, xlim=c(1,18)) # ACF of the residuals for lags 1 to 18

acfma1=ARMAacf(ma=c(0.7), lag.max=10) # 10 lags of ACF for MA(1) with theta1 = 0.7
lags=0:10 #creates a variable named lags that ranges from 0 to 10.
plot(lags,acfma1,xlim=c(1,10), ylab="r",type="h", main = "ACF for MA(1) with theta1 = 0.7")
abline (h=0) #adds a horizontal axis to the plot

xc=arima.sim(n=150, list(ma=c(0.7))) #Simulates n = 150 values from MA(1)
x=xc+10 # adds 10 to make mean = 10. Simulation defaults to mean = 0.
plot(x,type="b", main="Simulated MA(1) data")
acf(x, xlim=c(1,10), main="ACF for simulated sample data")

acfma2=ARMAacf(ma=c(0.5,0.3), lag.max=10)
acfma2
lags=0:10
plot(lags,acfma2,xlim=c(1,10), ylab="r",type="h", main = "ACF for MA(2) with theta1 =
     0.5,theta2=0.3")
abline (h=0)
xc=arima.sim(n=150, list(ma=c(0.5, 0.3)))
x=xc+10
plot (x, type="b", main = "Simulated MA(2) Series")
acf(x, xlim=c(1,10), main="ACF for simulated MA(2) Data")

ma1pacf = ARMAacf(ma = c(.7),lag.max = 36, pacf=TRUE)
plot(ma1pacf,type="h", main = "Theoretical PACF of MA(1) with theta = 0.7")


