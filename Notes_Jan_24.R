# Leslie matrices today

# N(t+1) = LN(t) where L is the Leslie matrix

# stable growth factor: leading eigenvalue of L: lambda
# stable age dist: corresponding eigenvector: v

# Lv = lambda*v    ie the matrix times eigenvector gives you the same as eigenvalue times eigenvector

# can do age classes, stage classes, size classes

L <- rbind(c(1/2,1,3/4),c(2/3,0,0),c(0,1/3,0)) # rbind 3 vectors: fecundity, then two survivorships
L

n0 <- c(2,0,0) # starting population size/distribution

L%*%n0 # this does standard matrix multiplication L*v

tf <- 10 # 10 time steps

Nt <- matrix(NA,nrow=nrow(L),ncol=tf) # a matrix for value of each class at each time step

Nt[,1] <- n0

for(t in 1:(tf-1)) Nt[,t+1] <- L%*%Nt[,t] # for loop iterating across time steps
Nt
#Nt[4,] <- colSums(Nt) # could use this to create a total row

eigen(L) # gives us eigenvalues and eigenvectors, leading eigenvalue FIRST
ev <- eigen(L)
ev$values[1] # gives us leading eigenvalue
Re(ev$values[1]) # JUST the real portion of leading eigenvalue

leval <- Re(eigen(L)$values[1]) # first eigenvalue
levec <- Re(eigen(L)$vectors[,1]) # first eigenvector

# could make both of these into functions


#### Teasel data now ####

# upload teasel data
# run for 50 time steps starting with 2 individuals in stage 1
# plot actual growth factors over time: compare total populations from time step to time step
# this is total N(t+1)/N(t)
# then plot expected growth factor over time: leading eigenvalue. Compare to real growth
# we do this to see how much deviation you get from not being in stable age distribution

#### Upload teasel data ####

TS <- as.matrix(read.table("GitHub/ECL_233/teasel_stage.txt"))
TS

#### run through time ####
nstg <- ncol(TS) # number of stages in the matrix
n0 <- c(2,rep(0,nstg-1)) # initial population vector
tf <- 50 # final run time
Nt <- matrix(NA,nrow=nstg,ncol=tf)
Nt[,1] <- n0

for(t in 1:(tf-1)){
  Nt[,t+1] <- TS%*%Nt[,t]
}
Nt

#### plot actual growth factor ####

Ntot_t <- colSums(Nt) # total pop sizes through time
gt <- Ntot_t[2:tf]/Ntot_t[1:(tf-1)] # growth factor
gt
gt <-Ntot_t[-1]/Ntot_t[-tf] # take out first entry on top, last entry on bottom

lambda <- Re(eigen(TS)$values[1]) # leading eigenvalue
lambda

#### plot time ####

plot(1:(tf-1),gt,type="l",col="black",xlab="time",ylab="growth factor")
lines(1:(tf-1),rep(lambda,tf-1),col="red")


#### Sebastian's stuff ####

# test the approximation from Peron-Frobenius at time tf-1

# first compute the approximation
v <- Re(eigen(TS)$vectors[,1])
v <- v/sum(v) # now we have the vector as proportions of individuals in each stage AFTER enough time
v
w <- Re(eigen(t(TS))$vectors[,1]) # transpose T and take the eigenvector, which is the left eigenvector
w <- w/sum(w*v) # forcing sum(wv) to be 1 after we do this
sum(w*v) # this should be 1
w # starting with 1 individual in stage 7 gives you a total growth factor of 95*lambda^t

Napprox <- sum(w*n0)*v*lambda^(tf-1)

# plot approximation against the actual simulation

barplot(cbind(Napprox,Nt[,tf]),beside=TRUE)

# Napprox looks the same as the simulation final 



# sensitivity analysis time
# create matrix of sensitivities ie (dlambda/dL[i,j]) = S

S <-w%o%v # outer product of w*v to make matrix of sensitivities

barplot(S,beside=T) # plotting all sensitivities
# each group of bars is a column
# the highest bar is the TS entry that is first column, last row. It's the contribution of the first
# stage to the LAST stage. It's not realistic for this entry to be anything but 0.

# now elasticities
E <- S*TS/lambda
sum(E) # sum of E should be 1
barplot(E,beside=T)
# greatest elasticity is increasing the last row, first entry

# HW

# first plot is just changing 431 entry from 0 to 75, no density dependence needed
