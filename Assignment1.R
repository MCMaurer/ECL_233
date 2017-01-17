# don't need to iterate beyond two values, n0 and n1
# probably turn in in markdown


# RickRoll is function to iterate Ricker model one time step
# nt is n(t)
# r is growth rate
# k is carrying capacity
# returns n1, which is n(t+1)

RickRoll <- function(nt, r, k){
  n1 <- nt*exp(r*(1-nt/k))
  return(n1)
}

RickRoll2 <- function(nt, r, k){
  n1 <- nt*exp(r*(1-nt/k))
  n2 <- n1*exp(r*(1-n1/k))
  n3 <- n2*exp(r*(1-n2/k))
  n4 <- n3*exp(r*(1-n3/k))
  n5 <- n4*exp(r*(1-n4/k))
  return(c(n1,n2,n3,n4,n5))
}

# BevMo is function to iterate Beverton-Holt model one time step
# nt is n(t)
# r is growth rate
# k is carrying capacity
# returns n1, which is n(t+1)


BevMo <- function(nt, r, k){
  n1 <- (exp(r)*nt)/(1+exp(r)*nt/k)
  return(n1)
}

BevMo2 <- function(n0, r, k){
  n1 <- (exp(r)*n0/(1+((exp(r)-1)*n0/k)))
  return(n1)
}

# saving results for N(t+1) for each model given the same inputs
rick <- RickRoll(1:120,1.2,100)
bev <- BevMo(1:120,1.2,100)

# plotting the results from each model against N(t), adding some color and a legend
plot(1:120, rick, col="red", xlab="N(t)", ylab="N(t+1)")
points(1:120, bev, col="blue")
legend("topleft", legend=c("Ricker", "Beverton-Holt"), col=c("red","blue"), pch = c(1,1))

