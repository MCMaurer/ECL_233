# could use approximation to calculate across lots of matrices with different entries




## everything works, just have to go through and check the length of things, whether they're 0 to 75
## or 1 to 75, there are some discrepancies


TS <- as.matrix(read.table("GitHub/ECL_233/teasel_stage.txt"))
TS

A <- function(N){
  A.temp=TS;
  A.temp[1,7]=f/(1+a*sum(N));
  return(A.temp)
}

f=50
a=0
A(Nt)

# one value of f

nstg <- ncol(TS) # number of stages in the matrix
n0 <- c(2,rep(0,nstg-1)) # initial population vector
tf <- 500 # final run time
Nt <- matrix(NA,nrow=nstg,ncol=tf)
Nt[,1] <- n0

f=50
a=0
for(t in 1:(tf-1)){
  Nt[,t+1] <- A(Nt[,t])%*%Nt[,t]
}

Nt[,10]

## This works, but how to iterate over values of f?


#### Ok here's the stuff for plot 1 ####
eigenvalues <- rep(NA,length(1:75))

for(b in 1:75){
  eigenvalues[b] <- Re(eigen(A2(n0,b))$values[1])
}
length(eigenvalues)
plot(1:75,eigenvalues,type="p", xlab="f")
abline(h=1,lty=2,col="red")

#### now time for plot 2 ####


# define the function

A2 <- function(N,f){
  A.temp=TS;
  A.temp[1,7]=f/(1+a*sum(N));
  return(A.temp)
}

a=0.01
tf <- 500
nstg <- ncol(TS) # number of stages in the matrix
n0 <- c(2,rep(0,nstg-1)) # initial population vector
f <- seq(from=0,to=75,by=1)
Ntf <- array(NA,c(length(f),nstg,tf)) # create an array of length(f) matrices
for(f in 1:length(f)) Ntf[f,,1] <- n0
Ntf[1,,]

A2(Ntf[1,,1],50)




for(t in 1:(tf-1)){
  for(b in 1:75){
    Ntf[b,,t+1] <- A2(Ntf[b,,t],f=b)%*%Ntf[b,,t]
  }
}

## ok Ntf is filled with good values now. Now I just need to sum all the t=500 values for each matrix

sum(Ntf[1,,500])
sum(Ntf[75,,500])
length(Ntf[,1,1])
Ntf[75,,]

totals <- rep(NA,length(Ntf[,1,1]))
for(i in 1:length(Ntf[,1,1])){
  totals[i] <- sum(Ntf[i,,500])
}

plot(1:length(totals),totals,type="p", xlab="f",ylab="stable pop size")

