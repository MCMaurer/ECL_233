# Intro to probability with R

# Goals: random number commands, maximum likelihood, branching process

rm(list=ls())

# binomial distribution. Basically going to flip a coin 24,000 times

rbinom(n=1,size=24000,prob=0.5)
dbinom(1200,size=24000,prob=0.5)
sum(dbinom(12102:24000,size = 24000,prob = 0.5)) # probability of getting 12102 or higher

1-pbinom(12101,size = 24000,prob = 0.5) # pbinom calculates cumulative prob of value and LESS than it

# what does this distribution look like?

no.trials <- 20
barplot(dbinom(0:20,20,0.5)) # this looks normal because of Central Limit Theorem
barplot(dbinom(0:24000,24000,0.5))

#### more Central Limit Theorem ####

my.data <- c(8,88,123,250,1390)

# sample from my.data 100 times and find the mean, then do this 10,000 times
# CLT says the sample means will be normally distributed, regardless of the underlying distribution

no.trials <- 100
k <- 10000

# sample no.trials*k with replacement from data
x <- sample(my.data,size = no.trials*k,replace = T)

# rewrite as a matrix with 100 rows and 10000 columns, each column is one experiment
xx <- matrix(data=x,nrow=no.trials,ncol=k)

# get sums of all the Xi values (each column)
sum.XX <- colSums(xx)

# what does the histogram look like?
hist(sum.XX,freq = FALSE) # this gives density associated with the bar, not the counts

# get the right normal to plot over this
meanX <- mean(my.data)*no.trials
varX <- mean((my.data-mean(my.data))^2)*no.trials # take data values, subtract means, square them, and take the mean of that. This is the definition of variance
sdX <- sqrt(varX)

# plot on top
xs <- seq(min(sum.XX),max(sum.XX),length=100)
lines(xs,dnorm(xs,mean=meanX,sd=sdX))



#### Poisson distribution ####

# approximate pois with binom
lambda <- 5
n <- 120
plot(dpois(0:(3*lambda),lambda = lambda),type="b") # we're just iterating from 0 to 3*lambda so the plot goes far enough out
lines(dbinom(0:(3*lambda),size = n,prob=lambda/n),type="b",col="red") # we're going n number of time steps, and for each of them, we flip a coin with the probability of lambda/n


#### infection stuff ####

# read in SARS data, with goal to fit distributions to it

load("GitHub/ECL_233/SARS.Rdata")
SARS

require(fitdistrplus)

model1 <- fitdist(SARS,distr="pois",method="mle")
summary(model1)
mean(SARS) # this is the SAME as the value the model gives. The estimate for lambda is just the mean of the dataset

# we are gonna use a compound distribution, the gamma-poisson, which gives you a neg. binomial

model2 <- fitdist(SARS,distr="nbinom",method="mle")

cdfcomp(list(model1,model2),legendtext=c("poisson","neg binom")) # cdf is cumulative density function. y axis is probability that you infect x or fewer individuals. neg binom is way better than poisson
model1$aic
model2$aic # model 2 

# simulate a branching process (disease outbreak). It's like the exponential growth model for individual based models. each individual creates a certain number of infected individuals. we have a fixed discrete distribution for infection. idk some other stuff sebastian said

# sample from SARS data to determine # of folks infected by each individual in a given generation of the outbreak

reps <- 5000 # of outbreaks
Tf <- 15 # number of generations for each simulation
N <- matrix(NA,Tf,reps)
N[1,] <- 1

# run a double loop to go through reps and time

for(i in 1:reps){
  for(j in 1:(Tf-1)){ # we calculate # of individuals you infect during your infectious period, so generations don't overlap
    if(N[j,i]>0){
    N[j+1,i] <- sum(sample(SARS,size=N[j,i],replace=T)) 
    }else{N[j+1,i]<-0} # if N[j,i] is 0, N[j+1,i] is 0
  }
}

matplot(N,type="l",pch=".",xlim=c(8,15))

# what if we wanna know probability of no outbreak? use a probability generating function?
