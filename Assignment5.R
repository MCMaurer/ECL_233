PredPrey1 <- function(t,n,parms){
  with(as.list(c(parms,n)),{
    dN <- r*N - b*N*P
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

#### now the second model ####

PredPrey2 <- function(t,n,parms){
  with(as.list(c(parms,n)),{
    dN <- r*N*(1-N/K) - b*N*P
    dP <- c*N*P-m*P
    res <- list(c(dN,dP))
    return(res)
  })
}

out2 <- as.data.frame(lsoda(n0,times,PredPrey2,parms))

plot(out2$N,out2$P,type="l")
points(out2$N[1],out2$P[1],pch=21,bg="green")
points(out2$N[length(out2$N)],out2$P[length(out2$P)],pch=21,bg="red")

#### third model ####

PredPrey3 <- function(t,n,parms){
  with(as.list(c(parms,n)),{
    dN <- r*N*(1-N/K) - b*(N/(1+d*N))*P
    dP <- c*(N/(1+d*N))*P-m*P
    res <- list(c(dN,dP))
    return(res)
  })
}

out3 <- as.data.frame(lsoda(n0,times,PredPrey3,parms))

plot(out3$N,out3$P,type="l")
points(out3$N[1],out3$P[1],pch=21,bg="green")
points(out3$N[length(out3$N)],out3$P[length(out3$P)],pch=21,bg="red")

#### fourth model ####

PredPrey4 <- function(t,n,parms){
  with(as.list(c(parms,n)),{
    dN <- r*N*(1-N/K) - b*(N^2/(1+d*N^2))*P
    dP <- c*(N^2/(1+d*N^2))*P-m*P
    res <- list(c(dN,dP))
    return(res)
  })
}

out4 <- as.data.frame(lsoda(n0,times,PredPrey4,parms))

plot(out4$N,out4$P,type="l")
points(out4$N[1],out4$P[1],pch=21,bg="green")
points(out4$N[length(out4$N)],out4$P[length(out4$P)],pch=21,bg="red")

saveGIF({for( w in seq(from=.0001, to=.005, by=.0001)){
  
  times <- 1:1000
  parms <- c(r=r,b=b,c=c,m=m,a=a,d=w,K=K)
  out5 <- as.data.frame(lsoda(n0,times,PredPrey4,parms))
  plot(out5$N, out5$P, type = "l", xlab = "N density", ylab = "P density",
          main=paste("d=",w), lwd = 2, xlim=c(0,1000), ylim=c(0,100), lty=1)
  points(out5$N[1],out5$P[1],pch=21,bg="green")
  points(out5$N[length(out5$N)],out5$P[length(out5$P)],pch=21,bg="red")}},
  movie.name = "assignment5.gif", interval = 0.3, clean=TRUE)



#### let's try the other part of the assignment ####




