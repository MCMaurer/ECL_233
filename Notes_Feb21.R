# environmental stochasticity

#### get data ####

rm(list=ls())
load("GitHub/ECL_233/checkerspot.Rdata")
# plot the data
N <- N.data$N
N.yr <- N.data$years
plot(N.yr,N,log="y",type="b")

rain <- precip.data$precip
rain.yr <- precip.data$years
plot(rain.yr,rain,type="b")


#### predictive model stuff ####

# develop a model that uses precip data and density data to predict population density in the next year

y <- log(N) # log of densities
n <- length(N.yr) # number of pop densities
temp <- N.yr[-1]-N.yr[-n] # years between counts of density
index <- which(temp==1) # the data indices with single years between counts
r <- y[index+1]-y[index] # take next log-density, subtract current log-density. This is realized per-capita growth rate
yrs <- N.yr[index]

# now we want to get precip data from the years in which we have density measurements
rain.index <- rain.yr %in% yrs # damn this %in% function is DOPE
w <- rain[rain.index]^(-2) # just trust sebastian on the -2 power thing
N2 <- N[index] # get densities for our relevant years
plot(w,N2)


# build our little model
model <- lm(r~N2+w)

# what model now?
# N[t+1] = N[t]*exp(a+b*N[t]+c*w[t])=f(N[t],w[t])
# take log of both sides
# logN[t+1]-logN[t] = a+b*N[t]+c*w[t]
# precip value plus density value gives you next density value

f <- function(N,w){N*exp(model$coefficients[1]+model$coefficients[2]*N+model$coefficients[3]*w)}

# so what?
# based on the notes, we are going to calculate E[log(R(t))], we call it ER

w.data <- rain^(-2) # put all our weather data into the right format

R <- function(w){exp(model$coefficients[1]+model$coefficients[3]*w)} # function for R

# look at pre-1971 data and compute this
index.pre <- which(rain.yr < 1971)
ER.pre <- mean(log(R(w.data[index.pre]))) # calculate mean of log of R of all the relevant rainfall data. This is the expected value of log(R(t))

index.post <- which(rain.yr >= 1971)
ER.post <- mean(log(R(w.data[index.post])))

# simulate the full model

simulator <- function(N.init=1000,Tf=100,reps=1,W=w.data){
  Nt <- matrix(N.init,Tf,reps)
  for(t in 1:(Tf-1)){
    temp <- sample(W,size=reps,replace=T)
    Nt[t+1,] <- f(Nt[t,],temp)
  }
  return(Nt)
}

# run the model
Nt <- simulator(Tf=200,reps=20,W=w.data[index.pre])

matplot(Nt+1,type="l",log="y")

Nt <- simulator(Tf=1000,reps=20,W=w.data[index.post])

matplot(Nt+1,type="l",log="y")

# stationary distributions (statistical distribution) if you go long enough. We want to know the long-term behavior. You can run the model one long time, only one rep, IF you are only interested in the long-term stationary distribution.

# this is useful for the homework

# look at model with 10 competing species. They compete for space. Basic model is lottery model. You have 10 species competing for space, space is FULLY occupied. Every year individuals reprodce and the # of offspring they have depends on species they are and how environment is affecting them that year. Each species has a specific response to the environment. Each individual of a species produces the same # of offspring but this changes depending on environment. They make a prospectivce seed bank, and some parents die, and the seeds have some chance of grabbing one of these new spots. Without any fluctuation, the species that makes the most offspring will always win. Environmental fluctuation can cause coexistence. Storage is that the fraction of parents that die each year is less than 1. They are perennial plants. 

# write a simulator like for the 1 species model, but for k species. each species draws # of kids it produces from lognormal, log(fecundity), which have different means. What is realized per-capita growth rate of each species when it's missing (this is like invasion rate). calculate tendencies of each species to increase when rare when they're removed from the community. two scenarios: little storage vs. lots of storage. positive invasion rates when rare = they can invade. 

# first barplot: all but 1 species can exist when rare, second one: only 6 can coexist. This is high storage vs. low storage
