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


