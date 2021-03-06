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
fseq <- (0:75)
eigenvalues <- rep(NA,length(fseq))

for(b in fseq){
  eigenvalues[b+1] <- Re(eigen(A2(n0,b))$values[1])
}
length(eigenvalues)
plot(fseq,eigenvalues,type="p", xlab="f")
abline(h=1,lty=2,col="red")


## this might be wrong... 

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
fseq <- seq(from=0,to=75,by=1)
Ntf <- array(NA,c(length(fseq),nstg,tf)) # create an array of length(f) matrices
for(x in 1:length(fseq)) Ntf[x,,1] <- n0
Ntf[1,,]





# for(t in 1:(tf-1)){
#   for(b in 1:75){
#     Ntf[b,,t+1] <- A2(Ntf[b,,t],f=b)%*%Ntf[b,,t]
#   }
# }

## i think this works
for(t in 1:(tf-1)){
  for(b in fseq){
    Ntf[b+1,,t+1] <- A2(Ntf[b+1,,t],f=b)%*%Ntf[b+1,,t]
  }
}

#### now trying something new ####

# gonna try to simply store the 500th value as a matrix ranging with f
tf <- 500
fseq <- 0:75
a <- 0.01
nstg <- ncol(TS) # number of stages in the matrix
Nt <- matrix(NA, length(fseq),nstg)
n0 <- c(2,rep(0,nstg-1)) # initial population vector
for(i in length(fseq)){
  Nt[i,] <- n0 
}



for(t in 1:(tf-1)){
  Nt[f+1,] <- A2(Nt[f+1,],f=fseq)%*%Nt[f+1,]
}




##
# 
# for(t in 1:(tf-1)){
#   Ntf[b+1,,t+1] <- A2(Ntf[b+1,,t],f=f)%*%Ntf[b+1,,t]
# }


# 
# for(t in 1:(tf-1)){
#   for(b in 0:75){
#     Ntf[b+1,,t+1] <- A2(Ntf[b+1,,t],f=b)%*%Ntf[b,,t]
#   }
# }

## ok Ntf is filled with good values now. Now I just need to sum all the t=500 values for each matrix

sum(Ntf[1,,500])
sum(Ntf[75,,500])
length(Ntf[,1,1])
Ntf[76,,]

totals <- rep(NA,length(Ntf[,1,1]))
for(i in 1:length(Ntf[,1,1])){
  totals[i] <- sum(Ntf[i,,500])
}

plot(1:length(totals),totals,type="p", xlab="f",ylab="stable pop size")







#### this stuff is copy pasted from a markdown doc ####


For fun, let's try doing the approximation we did in class for the linearization...
```{r}
eigenvectors <- matrix(NA,length(f),nstg)
for(b in f){
eigenvectors[b+1,] <- Re(eigen((A2(0,b)))$vectors[,1])
}
for(b2 in f){
eigenvectors[b2+1,] <- eigenvectors[b2+1,]/sum(eigenvectors[b2+1,])
}

left.eigenvectors <- matrix(NA,length(f),nstg)
for(b3 in f){
left.eigenvectors[b3+1,] <- Re(eigen((t(A2(0,b3))))$vectors[,1])
}
for(b4 in f){
left.eigenvectors[b4+1,] <- eigenvectors[b4+1,]/sum(eigenvectors[b4+1,])
}

NapproxMat <- matrix(NA, length(f),nstg)

for(b5 in f){
NapproxMat[b5+1,] <- sum(left.eigenvectors[b5+1]*n0)*eigenvectors[b5+1]*eigenvalues[b5+1]^(tf-1)
}

approx_totals <- rowSums(NapproxMat)

plot(1:length(approx_totals),log(approx_totals),type="p", xlab="f",ylab="stable pop size") # plot totals out
```
