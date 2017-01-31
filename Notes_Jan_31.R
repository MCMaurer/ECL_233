# step 1: read data into R, plot it
# step 2: use glm package to fit models to data to make s, G, F functions, and plot
# step 3: stitch together K function (kernel)
# step 4: Discretize
# step 5: Do some eigenstuff with it

# read in data

D <- read.csv("GitHub/ECL_233/IPM-data.csv")
head(D)

#### Plot data ####

plot(D$size, D$surv, pch=21,bg=rgb(1,0,0,0.2))
winners <- which(D$surv==1) # list of those that survived
plot(D$size[winners], D$sizeNext[winners], pch=21,bg=rgb(1,0,0,0.2)) # only plot the "winners"

#### fit models and plot ####
# linear model assumes noise around linear regression is NORMALLY distributed
# glm doesn't assume the noise is normal
# survival is either 0s or 1s, NOT normal. It's Bernoulli
# we want prob of success as a function of size
# other types of noise we can look at: with counting data, Poisson makes sense as a random variable.
# in a GLM there's also a link function that transforms data such that under the transformation the 
# relationship might be roughly linear

# fit first model for survivorship

model.surv <- glm(surv~size,data=D,family=binomial) # model survival as function of size
# default link here is logit transform. Fit a logistic between prob of survival and size
