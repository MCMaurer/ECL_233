PredPrey2 <- function(t,n,parms){
  with(as.list(c(parms,n)),{
    dN <- r*N*(1-N/K) - b*N*P
    dP <- c*N*P-m*P
    res <- list(c(dN,dP))
    return(res)
  })
}

# parameters

b = 0.01 # predator attack rate
c = 0.1*b # predator conversion of preation into reproduction
m = 0.2 # predator mortality
a = 1 # rate of prey capture/unit prey and time
d = 0.0015 # handling time
r = 0.5 # prey growth rate
K = 1000 # prey carrying capacity

parms <- c(r=r,b=b,c=c,m=m,a=a,d=d,K=K) # named vector of parameters
tf <- 100 # end time
times <- 1:tf
n0 <- c(N=300,P=50) # this just makes n0 a named list, allows you to keep labels which is good

# run it

out1 <- as.data.frame(lsoda(n0,times,PredPrey1,parms))

plot(out1$N,out1$P,type="l")
points(out1$N[1],out1$P[1],pch=21,bg="green")
points(out1$N[length(out1$N)],out1$P[length(out1$P)],pch=21,bg="red")


saveGIF({for( w in seq(from=.001, to=.05, by=.001)){
  
  times <- 1:1000
  parms <- c(r=r,b=w,c=c,m=m,a=a,d=d,K=K)
  out5 <- as.data.frame(lsoda(n0,times,PredPrey2,parms))
  plot(times, out5$N, type = "l", xlab = "N density", ylab = "P density",
       main=paste("d=",w), lwd = 2, xlim=c(0,100), ylim=c(0,400), lty=1)
  lines(times,out5$P,col="red")
  }},
  movie.name = "inclass.gif", interval = 0.3, clean=TRUE)


