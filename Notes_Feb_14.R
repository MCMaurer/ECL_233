# Intro to probability with R

# Goals: random number commands, maximum likelihood, branching process

rm(list=ls())

# binomial distribution. Basically going to flip a coin 24,000 times

rbinom(n=1,size=24000,prob=0.5)
dbinom(1200,size=24000,prob=0.5)
sum(dbinom(12102:24000,size = 24000,prob = 0.5)) # probability of getting 12102 or higher

1-pbinom(12101,size = 24000,prob = 0.5) # pbinom calculates cumulative prob of value and LESS than it


