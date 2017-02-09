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


# gif changing handling time for model 4
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


competition <- function(t,n,parms){
  with(as.list(c(parms,n,a)),{
  dn1 <- r1*n1*(1-n1/K1-a1*n2/K1)
  dn2 <- r2*n2*(1-n2/K2-a2*n1/K2)
  res <- list(c(dn1,dn2))
  return(res)
  })
}

rkt <- as.matrix(read.table("GitHub/ECL_233/parameciumrk.txt"))
colnames(rkt)=NULL
parms <- c(r=c(rkt[1,]),K=c(rkt[2,]),a=c(6,1)) # named vector of parameters
tf <- 20 # end time
times <- 1:tf
n0 <- c(n=c(2,2)) 

out5 <- as.data.frame(lsoda(n0,times,competition,parms))
out5
plot(out5$time,out5$n1,type="l")
lines(out5$time,out5$n2,col="red")


nTrue <- as.matrix(read.table("GitHub/ECL_233/paramecium2.txt"))
colnames(nTrue)=NULL
parmsrk <- c(r=c(rkt[1,]),K=c(rkt[2,]))
n0 <- c(n=c(nTrue[1,1],nTrue[1,2])) # pull first entries for initial pop size
times <- 1:length(nTrue[,1])

compMin <- function(parms){ # don't need initial conditions bc we're putting data in
  out <- as.data.frame(lsoda(n0,times,competition,parms)) # run model with current guess
  mse1 <- mean((out$n1-nTrue[,1])^2) # calculate mean square difference
  mse2 <- mean((out$n2-nTrue[,2])^2)
  sum.mse <- mse1 + mse2
  return(sum.mse) # return value we want minimized
} 

a <- c(1,1)
parms.tot <- c(parmsrk,a=a)


## this is trying the standard option, what was shown in the assignment description, but it is optimizing across all the parameters, even though we know r and K
optimOut <- optim(par=c(parmsrk,a=a),compMin)
optimOut$par
out6 <- as.data.frame(lsoda(n0,times,competition,optimOut$par))

plot(out6$time,out6$n1,type="l")
lines(out6$time,out6$n2,col="red")
points(out6$time,nTrue[,1])
points(out6$time,nTrue[,2],col="red")

## this is constraining the initial parameters to a tiny range around the ones we put in from the start, which should keep them where we want them, rather than letting them vary
optimOut2 <- optim(par=parms.tot,compMin,method="L-BFGS-B",lower=
                     c(parms.tot[[1]]-0.01,
                       parms.tot[[2]]-0.01,
                       parms.tot[[3]]-0.01,
                       parms.tot[[4]]-0.01,-Inf,-Inf), upper=c(parms.tot[[1]]+0.01,
                                                          parms.tot[[2]]+0.01,
                                                          parms.tot[[3]]+0.01,
                                                          parms.tot[[4]]+0.01,Inf,Inf))

optimOut2$par

out7 <- as.data.frame(lsoda(n0,times,competition,optimOut2$par))
out7

plot(out7$time,out7$n1,type="l")
lines(out7$time,out7$n2,col="red")
points(out7$time,nTrue[,1])
points(out7$time,nTrue[,2],col="red")


## the results of these two are different, and they seem pretty significant. I'm not sure which is right





#### known values built in ####
# 
# competition.known <- function(t,n,parms){
#   with(as.list(c(parms,n,a)),{
#     dn1 <- 0.7816*n1*(1-n1/559.6860-a1*n2/559.6860)
#     dn2 <- 0.6283*n2*(1-n2/202.4931-a2*n1/202.4931)
#     res <- list(c(dn1,dn2))
#     return(res)
#   })
# }
# 
# compMin.known <- function(parms){ # don't need initial conditions bc we're putting data in
#   out <- as.data.frame(lsoda(n0,times,competition.known,parms)) # run model with current guess
#   mse1 <- mean((out$n1-nTrue[,1])^2) # calculate mean square difference
#   mse2 <- mean((out$n2-nTrue[,2])^2)
#   m.mse <- (mse1 + mse2)/2
#   return(m.mse) # return value we want minimized
# } 
# 
# parmsA <- c(a=a)
# 
# optimOut3 <- optim(parmsA,compMin.known)
# optimOut3$par
# 
# 
# parmsB <- c(parmsrk,optimOut3$par)
# out <- as.data.frame(lsoda(n0,times,competition,parmsB))
# out
# 
# plot(out$time,out$n1,type="l")
# lines(out$time,out$n2,col="red")
# points(out$time,nTrue[,1])
# points(out$time,nTrue[,2],col="red")
