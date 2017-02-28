#------------------------------
# INTRO TO PROBABILITY WITH R
#==============================

#------------------------------
# Goals: Cover basic random number commands, maximum likelihood,
#	branching process simulation and exact calculations using probability
# generating functions (pgf)
#=============================

# clear variables etc
rm (list = ls ())

#----------------------------
# Binomial distribution
#============================


#A. The French naturalist Count Buffon (1707-1788) tossed a coin 4040 times.
#   Result: 2048 heads, or proportion 2048/4040 = 0.5069 for heads.
#B. Around 1900, the English statistician Karl Pearson heroically
#		tossed a coin 24,000 times.
#		Result: 12,012 heads, a proportion of 0.5005.
#C. While imprisoned by the Germans during World War II,
#		the South African mathematician John Kerrich tossed a coin 10,000 times.
# 	Result:5067 heads, a proportion of 0.5067.


# rbinom allows us to flip a virtual coin 24000 times with ease
rbinom(1,size=24000,p=0.5)
# how likely was Pearson's outcome
dbinom(12012,size=24000,p=0.5)
# How likely was getting within 12 of 12000?
sum(dbinom(11988:12012,size=24000,p=0.5))
# or
pbinom(12012,size=24000,p=0.5)-pbinom(11987,size=24000,p=0.5)

#------------------------------
# Central Limit Theorem
#==============================

# choose your favorite 5 numbers between 0 and 20 (not all the same please!!!)
my.data=c(1,3,5,7,11)

# To illustrate the CLGT, we will ample X of these numbers with replacement,
# compute and save average, and
# repeat Y times;

X=100  # number of samples per "trial"
Y=10000 # number of trials

#Create a matrix whose columns correspond to a trial

	samples=sample(my.data,X*Y,replace=TRUE) # sample with replacement
	samples=matrix(samples,X,Y) # reformat as a matrix
	sample.means=colMeans(samples) # computes mean for each column

# plot histogram
hist(sample.means,col="red",20,freq=FALSE)

# Looks bell shaped. Why? Recall the central limit theorem
# states that if X1,X2,X3,... are iid random variables with
# mean M and variance V then
# (X1+....+Xn)/n is approximately normally distributed
# with mean M and variance V/n.
# Don't believe it? Well lets check it out.

M=mean(my.data) #compute mean, varaince, and sd of the sample distribution
V=mean((my.data-M)^2)/sqrt(Y)
SD=sqrt(V)

#create a range of x values, evaluate normal density, and plot on top of histogram
xs=seq(M-3*SD,M+3*SD,length=100)
ys=dnorm(xs,mean=M,sd=SD)

hist(sample.means,col="red",20,freq=FALSE)
lines(xs,ys,lwd=3)

#------------------------------
#  Poisson distribution
#  and maximum likelihood
#==============================

# consider an infected individual that
# is said to infect others at a rate lambda per week.
# what does this mean? One possible interpretation
# is that at every small interval of time, say of length
# dt, the probability of this individual infecting someone
# is lambda*dt. If dt=1/N week, then the total number
# of individuals infected is binomial with probability of
# success lambda/N and N trials. Taking the limit
# as n goes to infinity yields a poisson random variable
# with mean lambda.

#compare poisson with lambda=5 and binomial with k trials and p=lambda/k

lambda=5
k=40
plot(dpois(0:20,lambda=lambda),type="b")
lines(dbinom(0:20,size = k,prob=lambda/k),type="b",col=2)




# now lets consider the following contact tracing data from a 
# 2003 outbreak of SARS in Beijing and plot its histogram

load("SARS.Rdata")
hist(SARS)

# Next, weuse the fitdistrplus package to fit the poisson and negative
# binomial to the data. Note: Theory implies that the MLE
# for the poisson distribution is just the mean value. To
# see why, let k1,k2,k3,..,kn be the observed counts
# from what we want to model as independent draws from the
# poisson ditribion. The probability of seeing these counts
# from independent poisson random variables with mean lambda
# is
# exp(-lambda)*lambda^k1/k1!*....*exp(-lambda)*lambda^kn/kn!
# Equivalently, by simplifying and taking logs want
# to maximize the log-likelihood
# -lambda*n+(k1+...+kn)*log(lambda)-k1!-...-kn!
# To find the maximum, we can differentiate with
# respect to zero and set things equal to zero
# which yields
# -n+(k1+...+kn)/lambda=0
# equivalently lambda=(k1+...+kn)/n
require(fitdistrplus)
model1=fitdist(SARS,method="mle",distr="pois") # fit poisson to data
model2=fitdist(SARS,method="mle",distr="nbinom") # fit negative binomial to data

# compare CDFs of the two models and the empirical distribution
cdfcomp(list(model1,model2),lwd=3,xlab="cases",legendtext=c("poisson","neg binom"))

# computing AICs for the two models.
model1$aic
model2$aic

#------------------------------
# Simulating a branching process
# and using prob. generating functions
#==============================
# now we will simulate an outbreak starting
# with one infected individual. At each
# generation of the outbreak, each infected individuals
# infects a random number of individuals drawn from
# SARS data. This model is an example of a branching
# process

reps=5000 # number of simulations to run
Tf=15 # length of each simulation
N=matrix(0,Tf,reps) # matrix for storing the simulations
N[1,]=1

# run a double loop of time vectorizing the reps

for(i in 1:reps){
for(t in 1:(Tf-1)){
  if(N[t,i]>0)N[t+1,i]=sum(sample(SARS,size=N[t,i],replace=TRUE))
}
}

# plot the solutions against one another

matplot(N+1,type="l",log="y")
abline(h=1)

# estimate for probability of extinction by T=10

length(which(N[Tf,]==0))/reps

# We can compute this last quantity exactly using probability generating functions
# Given the probability of infecting k individuals is p[k],
# the probability generating function is
# g(s)=sum(p(k)*s^k)
# where s is a dummy variable. This pgfs contain alot of information.
# for example, mean number of infections is given by
# g'(1)=sum(k*p(k))
# and the probability of going extinct in one time step is
# g(0)=p(0) given initially only one individual is infected.
# Most importantly, the probability of extinction by time Tf is
# given by g^Tf(0)=g(g(g(...g(0)))) i.e. Tf-1 applications of g to 0

# to get the p(k) we can run the following loop
p=numeric(34)
for(i in 1:34)p[i]=length(which(SARS==(i-1)))/34

# to try this out we introduce the probability generating function
g=function(s)sum(p*s^(0:33))
# next we iterate the value 0, Tf-1 times to get us to time Tf
s=0;
for(t in 1:(Tf-1))s=g(s)
# the exact value for extinction by time Tf or sooner is
print(s)
