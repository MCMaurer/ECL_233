# plotting exponential growth for different valus of r and n0

# dn/dt = rn
# solution n(t)=n0e^rt

# function for exponential growth

# n0 is initial pop size
# r is pop growth rate
# t is final time or time series

# gives us exponential growth trajectory


expGrowth = function(n0, r, t){
  n = n0 * exp(r*t) #exponential growth
  return(n)
}

y <- expGrowth(10, 0.02, 0:10)

plot(0:10,y)

# pseudocoding is writing out a plan
# goal: plot exponential growth for different values of r and n0, one t series
# make function for exponential growth
# pick values for r and n0 and time series
# apply function
# plot

# parameters
t <- seq(0,10,by=0.1)
r.base <- 1.2 #baseline growth rate
n0.base <- 2
rn0.factor <- 2 #factor to multiply each by
r.factored <- r.base*rn0.factor #second r value
n0.factored <- n0.base*rn0.factor #second n value

first <- expGrowth(n0.base,r.base,t)
big.n <- expGrowth(n0.factored,r.base,t)
big.r <- expGrowth(n0.base,r.factored,t)

plot(t,big.r,type="l", col="blue", xlab="time", ylab="size", log="y") # log puts y axis on log scale
lines(t, first, col="black")
lines(t, big.n, col="red")
legend("topleft", legend=c("Base", "r doubled", "n0 doubled"), col=c("black", "blue", "red"), lty=rep(1,3))


plot(t,first)
plot(t,big.n)
plot(t,big.r)


