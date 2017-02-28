# (B) Create a plot of the probability of extinction by
# generation 10 as function of q for both control strategies.
# Use pgfs to do this part of the problem.
# HINTS: i) If g1(s), g2(s),...,gk(s) are pgfs for k different
# offspring distributions and each individual draws from the ith offspring
# distribution with probability pi, then
# g(s)=p1*g1(s)+....+pk*gk(s) is pgf of this mixed distribution
# ii) The pgf for a binomial with k trials and probability
# q of success is ((1-q)+q*s)^k


# part 1

reps <- 10000
Tf <- 10
q <- 0.4
Npa <- matrix(NA,Tf,reps)
Npa[1,] <- 1

# this flips a coin for each infection event based on q. This is partial absolute control, since we are deciding whether or not an infected individual falls into our subset of controlled individuals. If you have more than 0 infected individuals, and if your individual does not fall into your subset of controlled individuals, then 
for(i in 1:reps){
  for(t in 1:(Tf-1)){
    if(Npa[t,i]>0){
      x <- rbinom(1,size=Npa[t,i],1-q) # flip the coin to determine how many of the current individuals are not in your protected subgroup, define this as x
      Npa[t+1,i]=sum(sample(SARS,size=x,replace=TRUE)) # now sample from those that are NOT in your protected subgroup
      }else{Npa[t+1,i]<-0}
    
  }
}


# this is homogenous partial control. This option takes each infectious individual and then flips a coin weighted according to q. The number of flips is based on the random draw from the SARS data. So if you draw a 33 from the SARS data, you flip the 0.6 weighted coin 33 times, so each individual infection event is based on our q reduction. Then that is summed up and goes into the matrix.
Nhp <- matrix(NA,Tf,reps)
Nhp[1,] <- 1

for(i in 1:reps){
  for(t in 1:(Tf-1)){
    if(Nhp[t,i]>0){
      possible.infections <- sum(sample(SARS,size=Nhp[t,i],replace=TRUE))
      Nhp[t+1,i]=rbinom(1,size=possible.infections,prob = 1-q)
    }else{Nhp[t+1,i]<-0}
  }
}


which(Npa[Tf,]!=0)
which(Nhp[Tf,]!=0)


hist(Npa[Tf,which(Npa[Tf,]!=0)])
hist(Nhp[Tf,which(Nhp[Tf,]!=0)])



#### part 2 ####

# now we can just loop the two functions through different q values and compute the prob of extinction for each q value and each strategy. Do this first, then try the pgf stuff.
q.seq <- seq(from=0,to=1,by=0.01)
Nhp.prob <- numeric(length(q.seq))

for(j in 1:length(q.seq)){
for(i in 1:reps){
  for(t in 1:(Tf-1)){
    if(Nhp[t,i]>0){
      possible.infections <- sum(sample(SARS,size=Nhp[t,i],replace=TRUE))
      Nhp[t+1,i]=rbinom(1,size=possible.infections,prob = 1-q.seq[j])
    }else{Nhp[t+1,i]<-0}
  }
}
Nhp.prob[j] <- length(which(Nhp[Tf,]==0))/reps
}

Nhp.prob



Npa.prob <- numeric(length(q.seq))

for(j in 1:length(q.seq)){
for(i in 1:reps){
  for(t in 1:(Tf-1)){
    if(Npa[t,i]>0){
      x <- rbinom(1,size=Npa[t,i],1-q.seq[j]) # flip the coin to determine how many of the current individuals are not in your protected subgroup, define this as x
      Npa[t+1,i]=sum(sample(SARS,size=x,replace=TRUE)) # now sample from those that are NOT in your protected subgroup
    }else{Npa[t+1,i]<-0}
  }
}
  Npa.prob[j] <- length(which(Npa[Tf,]==0))/reps
}

Npa.prob

dev.off()
plot(q.seq,Npa.prob,type="l",ylab="prob of extinction at t=10",xlab="q value")
lines(q.seq,Nhp.prob,col="red")
legend("bottomright", col=c("black","red"),
       legend=c("partial absolute","homogenous partial"),lty = rep(1,2),cex=0.75)








p=numeric(34)
for(i in 1:34)p[i]=length(which(SARS==(i-1)))/34

# HINTS: i) If g1(s), g2(s),...,gk(s) are pgfs for k different
# offspring distributions and each individual draws from the ith offspring
# distribution with probability pi, then
# g(s)=p1*g1(s)+....+pk*gk(s) is pgf of this mixed distribution
# ii) The pgf for a binomial with k trials and probability
# q of success is ((1-q)+q*s)^k



g=function(s){
  sum(p*s^(0:33))
}