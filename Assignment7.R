# HW ASSIGNMENT

#============================
# multispecies lottery competition modeol
#============================
# Chesson and Warner's lottery model envisions a community of individuals
# competing for a finite (but very large) number sites
# Let N[t,i] denote the fraction of sites occupied by species
# i at time t. 
# Let d be the fraction of individuals that die each year
# Let kids[t,i] be the number of kids produced by species i
# right before the census in year t 
# Then the model is given by 
# N[t,i]=(1-d)*N[t-1,i]+d*(kids[t,i]*N[t-1,i]/sum(kids[t,]*N[t-1,]))

# To implement the model, we need to make some decision about the 
# the distribution of offspring produced by each species. 
# We will assume that the log number of offspring
# are normally distributed with vector mu of means 
# i.e. mu[i] is the mean log number of offspring
# and a covariance matrix Sigma 
# i.e. Sigma[i,j] is the covariance between offspring of species i and species j
# To create multivariate normals, we need the MASS package
# (use the install.package command if you don't have it already)

library("MASS")

# the following function simulates the Lottery model
# for a length Tf using the aforementioned parameters
# and an initial condition that you specify 
# The output is a matrix whose columns represent species
# and rows represent time

lottery=function(k=10,N0=rep(0.1,10),Sigma=diag(10),mu=seq(1,1.25,length=10),Tf=100,d=0.1){
  N=matrix(0,Tf,k) # matrix which will be filled in
  N[1,]=N0 # initial values
  for(t in 2:Tf){ # starting the loop
    kids=exp(mvrnorm(n=1,mu=mu,Sigma=Sigma)) # random draw creating the lognormal number of kids
    N[t,]=(1-d)*N[t-1,]+d*kids*N[t-1,]/sum(kids*N[t-1,]) # determining the next community state
  }
  return(N) # returning the simulation
}

# running a sample simulation for 10,000 time steps and plotting

out=lottery(Sigma=diag(10),Tf=10000)
matplot(out,type="l")


# To understand when all species coexist, it is useful to look 
# at the stochastic growth rate of each species when rare. 
# This growth rate for, say species 1, is given by running the 
# simulation without species 1 and computing the average of 
# the following quantity 
# log(1-d+d*kids[1]/sum(kids*N[t-1,]))
# during the simulation. For the assignment, compute 
# stochastic growth rates when rare for each species in the 
# community when (i) Sigma=diag(10) and 
# (ii) Sigma=diag(10)*0.5+0.5. 
# Create a pair of barplots in a single figure showing 
# these stochastic growth rates for all the species. 
# HINT: To make the simulations run a bit faster, 
# it isn't necessary to store N at every point in time, 
# just keep track of the current vector N of population frequencies

out1 <- lottery(N0 = c(0,rep((1/9),9)))
matplot(out1,type = "l")
kids=exp(mvrnorm(n=1,mu=seq(1,1.25,length=10),Sigma=diag(10))) 
log(1-d+d*kids[1]/sum(kids*out1[t-1,]))


lottery.stoch=function(k=10,N0=rep(0.1,10),Sigma=diag(10),mu=seq(1,1.25,length=10),Tf=100,d=0.1){
  N=numeric(length=length(k)) #
  N=N0 # initial values
  for(t in 2:Tf){ # starting the loop
    kids=exp(mvrnorm(n=1,mu=mu,Sigma=Sigma)) # random draw creating the lognormal number of kids
    N=(1-d)*N+d*kids*N/sum(kids*N) # determining the next community state
  }
  return(N) # returning the simulation
}


# ok this function works, it just keeps returns stuff at final time step

out2=lottery.stoch(Sigma=diag(10),Tf=10000)
out2


lottery.stoch.2=function(k=10,N0=rep(0.1,10),Sigma=diag(10),mu=seq(1,1.25,length=10),Tf=100,d=0.1){
  N=numeric(length=length(k)) #
  N=N0 # initial values
  stoch <- log(1-d+d*kids/sum(kids*N))
  for(t in 2:Tf){ # starting the loop
    kids=exp(mvrnorm(n=1,mu=mu,Sigma=Sigma)) # random draw creating the lognormal number of kids
    N=(1-d)*N+d*kids*N/sum(kids*N) # determining the next community state
    stoch <- (stoch + log(1-d+d*kids/sum(kids*N)))/t
  }
  return(stoch) # returning the simulation
}

out3=lottery.stoch.2(Sigma=diag(10),Tf=10000)
out3




# run for a long time, 10000 time steps. 10 rows, 10000 columns. need to store vectors of kids at each time step into matrix

lottery.2=function(k=10,N0=rep(0.1,10),Sigma=diag(10),mu=seq(1,1.25,length=10),Tf=100,d=0.1){
  N=matrix(0,Tf,k) # matrix which will be filled in
  N[1,]=N0 # initial values
  K=matrix(NA,Tf-1,k)
  for(t in 2:Tf){ # starting the loop
    K[t-1,]=exp(mvrnorm(n=1,mu=mu,Sigma=Sigma)) # random draw creating the lognormal number of kids
    N[t,]=(1-d)*N[t-1,]+d*K[t-1,]*N[t-1,]/sum(K[t-1,]*N[t-1,]) # determining the next community state
  }
  return(list(N=N,K=K)) # returning the simulation
}

out4=lottery.2(N0 = c(0,rep((1/9),9)),Sigma=diag(10),Tf=10000)
out4$K

Tf <- 10000
K <- out4$K
N <- out4$N
N <- N[-Tf,]
d <- 0.1




mean(log(1-d+d*K[,1]/rowSums(K*N))) # this is the final value we need for if we ran it with no species 1



calc.stoch <- function(k,Sigma,Tf,d){
  stoch <- numeric(length=k)
  for(z in 1:k){
    n0 <- rep((1/(k-1)),k)
    n0[z] <- 0
    out <- lottery.2(N0 = n0,Sigma=Sigma,Tf=Tf,d=d)
    K <- out$K
    N <- out$N
    N <- N[-Tf,]
    stoch[z] <- mean(log(1-d+d*K[,z]/rowSums(K*N)))
  }
  return(stoch)
}

dat1 <- calc.stoch(k=10,Sigma=diag(10),Tf=10000,d=0.1)
barplot(dat1)


k <- 10
stoch <- numeric(length=k)

for(z in 1:k){
  n0 <- rep((1/9),10)
  n0[z] <- 0
  out <- lottery.2(N0 = n0,Sigma=diag(10),Tf=10000)
  K <- out$K
  N <- out$N
  N <- N[-Tf,]
  stoch[z] <- mean(log(1-d+d*K[,z]/rowSums(K*N)))
}
stoch
barplot(stoch)


stoch2 <- numeric(length=k)
for(z in 1:k){
  n0 <- rep((1/9),10)
  n0[z] <- 0
  out <- lottery.2(N0 = n0,Sigma=diag(10)*0.5+0.5,Tf=10000)
  K <- out$K
  N <- out$N
  N <- N[-Tf,]
  stoch2[z] <- mean(log(1-d+d*K[,z]/rowSums(K*N)))
}
barplot(stoch2)

dat <- rbind(stoch,stoch2)

barplot(dat,beside = T)
