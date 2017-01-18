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

rm(n)
Tf=1000

# a vector to store the values in and the initial densities
lyap=numeric(Nvr)
n=n0

for(t in 1:Tf){
  n=f(r,n) # updating the n values for all the r values
  lyap=lyap+log(abs(fp(vr,n))) # summing up all the logs of absolute values of derivatives
}
#lyap
lyap=lyap/Tf

#####





str(r)
r2 <- r[(tf-79):tf,]
str(r2)
plot(vr,r2[1,], pch=".", xlab="r", ylab="pop size")
for(i in 2:80) points(vr,r2[i,],pch=".")
par(new=T)
plot(vr,lyap,type="l",col="red",axes=F,xlab=NA,ylab=NA)
axis(side=4)
mtext(side=4,line=3,"Lyapunov Exponent")












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
