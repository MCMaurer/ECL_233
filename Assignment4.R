disp <- function(y,x){exp(-abs(y-m-x)/a)/(2*a)}

a <- 2
m <- 1
R <- 4

disp(1,1)

K <- function(y,x)R*disp(y,x) # this is everything in the IPM except integral and n(t,x)
K(1,1)

#### here's the plan for Q1 ####

# step one is create the projection matrix from the IPM
  # create the K, discretize, all that jazz.
  # create a starting population vector of densities of 1 for x between -2 and 2
  # multiply this starting vector by the projection matrix to get your distro for t=2
  # repeat for all the time steps

L <- 10
n <- 200
deltax <- (L--L)/n
xs <- seq(-L+deltax/2,L-deltax/2,length=n)

starting <- rep(NA,length(xs))

starting[which(xs <= 2)] <- 1
starting[which(xs > 2)] <- 0
starting[which(xs < -2)] <- 0
starting

# now we have all the necessary pieces

A <- outer(xs,xs,K)*deltax

Re(eigen(A)$values[1])

results <- matrix(NA,nrow=5,ncol = length(xs))
results[1,] <- starting
# starting is time 1

# now generate the rest
for(i in 2:5){
  results[i,] <- A%*%results[i-1,]
}

# plot it
plot(xs,results[5,],type="l")
for(l in 1:4){
  lines(xs,results[l,])
}



#### now for the second question ####

# just recreate A a bunch of times with different L values
# calculate the dominant eigenvalue for each of the new matrices with different L values
# plot eigenvalues against L values

## let's try just doing this for one value of L, something small so it'll hopefully be below 1

# deltax <- (0.5--0.5)/n # generate deltax using the L sequence values and a set length n
# xs <- seq(-0.5+deltax/2,0.5-deltax/2,length=n) # create the xs vector
# C <- outer(xs,xs,K)*deltax
# 
# Re(eigen(C)$values[1])


# ## now a sequence
# 
# 
# B <- array(NA,c(length(L.seq),n,n)) # create an array to hold all the matrices generated
# 
# for(q in 1:length(L.seq)){ # for each value in the L sequence
#   deltax <- (L.seq[q]--L.seq[q])/n # generate deltax using the L sequence values and a set length n
#   xs <- seq(-L.seq[q]+deltax/2,L.seq[q]-deltax/2,length=n) # create the xs vector
#   B[q,,] <- outer(xs,xs,K)*deltax # populate the array with the various matrices
# }
# 

L.seq <- seq(from=0,to=5,length=100) # create sequence of L values
dom.eig3 <- rep(NA,length(L.seq))

for(q in 1:length(L.seq)){ # for each value in the L sequence
  deltax <- (L.seq[q]--L.seq[q])/n # generate deltax using the L sequence values and a set length n
  xs <- seq(-L.seq[q]+deltax/2,L.seq[q]-deltax/2,length=n) # create the xs vector
  dom.eig3[q] <- Re(eigen((outer(xs,xs,K)*deltax))$values[1]) # populate the array with the various matrices
}

plot(L.seq,dom.eig3)
abline(h=1,lty=2)




##
# 
# dom.eig3 <- rep(NA,length(L.seq)) # create vector for the eigenvalues
# 
# for(z in 1:length(L.seq)){ # calculate eigenvalues for every L value
#   dom.eig[z] <- Re(eigen(B[z,,])$values[1])
# }
# 
# plot(L.seq,dom.eig)
# abline(h=1,lty=2)
