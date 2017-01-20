# Assignment 1
# (N_t+1=N_t*exp(r*(1-N_t/K)))

RickRoll <- function(nt, r, k){
  nt1 <- nt*exp(r*(1-nt/k))
  return(nt1)
}

k <- 100
n0 <- 50
tf <- 100

vr <- c(1.5,2.3,2.6,3.6)
Nvr <- length(vr)
r <- matrix(NA,nrow=tf,ncol=Nvr)
str(r)
r[1,] <- rep(n0,Nvr)
for(t in 1:(tf-1)){
  r[t+1,] <- RickRoll(r[t,],r=vr,k=k)
}

matplot(1:tf,r,type = "l",col = c("black","blue","red","green"),lty = rep(1.4))
legend("topleft", title = "r =", col = c("black","blue","red","green"),legend=c("1.5","2.3","2.6","3.6"),lty = rep(1,4))

## round 2

rm(n)
rm(r)
rm(t)

k <- 100
n0 <- 50
tf <- 500

vr <- seq(from=1.5,to=3.6,by=0.001)
Nvr <- length(vr)
r <- matrix(NA,nrow=tf,ncol=Nvr)
str(r)
r[1,] <- rep(n0,Nvr)
for(t in 1:(tf-1)){
  r[t+1,] <- RickRoll(r[t,],r=vr,k=k)
}

#matplot(vr,r[(tf-40):tf,],type = "p",col = rep("black",Nvr),pch = rep(20,Nvr))

####

rm(x)
rm(lyap)

f=function(r,x)x*exp(r*(1-x))
fp=function(r,x)exp(r*(1-x))*(1-r*x)
Tf=1000

# a vector to store the values in and the initial densities
lyap=numeric(Nvr)
x=n0

for(t in 1:Tf){
  x=f(vr,x) # updating the n values for all the r values
  lyap=lyap+log(abs(fp(vr,x))) # summing up all the logs of absolute values of derivatives
}
length(lyap)
lyap=lyap/Tf

#####

str(r)
r2 <- r[(tf-79):tf,]
str(r2)

par(mar=c(5,3,4,3)) # change margins to make room for two y axes
plot(vr,r2[1,], pch=".", xlab="r",ylab=NA) # plot initial bifurcation diagram points
mtext(side=2,line=2,"Pop Size") # add axis label
for(j in 2:40) points(vr,r2[j,],pch=".") # plot the rest of the bifurcation diagram points
par(new=T) # add a new plot on top of the old one
plot(vr,lyap,type="l",col="red",axes=F,xlab=NA,ylab=NA) # plot lyapunov exponents against vr
axis(side=4) # add a second y axis on the right side
mtext(side=4,line=2,"Lyapunov Exponent") # add a second y axis label
abline(h=0,lty=2,col="red") # add a dashed line showing a lyapunov exponent of 0
abline(v=c(2.0,3.1575),lty=2,col="black") # add a dashed line to see how bifurcation matches up with lyapunov

#### 









lines(vr,lyap,type="l", col="red")

rm(n)
Tf=1000

# a vector to store the values in and the initial densities
lyap=numeric(Nvr)
n=n0

for(t in 1:Tf){
  n=f(r,n) # updating the n values for all the r values
  lyap=lyap+log(abs(fp(vr,n))) # summing up all the logs of absolute values of derivatives
}
lyap
lyap=lyap/Tf

plot(vr,lyap,type="l",col="red")
abline(h=0,lty=2)
