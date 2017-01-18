# creating a plot of Lyapunov exponents to accompany your bifurcation
# diagram 

# define the Ricker map and its derivative (K=1 here)
f=function(r,n)n*exp(r*(1-n))
fp=function(r,n)exp(r*(1-n))*(1-r*n)

# define the values of r to compute the Lyapunov exponent for
k=100 # number of r values
r=seq(1.5,3.6,length=k) # vector of r values

# define the length of the simulation
Tf=1000

# a vector to store the values in and the initial densities
lyap=numeric(k)
n=1.1

# loop computing the lyap
for(t in 1:Tf){
  n=f(r,n) # updating the n values for all the r values
  lyap=lyap+log(abs(fp(r,n))) # summing up all the logs of absolute values of derivatives
}

# dividing by the length of the run to get the average stretch

lyap=lyap/Tf

# plotting away

plot(r,lyap,type="l",col="red")
abline(h=0,lty=2)

