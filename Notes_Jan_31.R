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



#### fit first model for survivorship ####

model.surv <- glm(surv~size,data=D,family=binomial) # model survival as function of size
# default link here is logit transform. Fit a logistic between prob of survival and size
# logit transform turns logistic curve into linear

summary(model.surv)
# we get intercept and slope (which is "size" here) and significance of fit

# plot the model against the data
# use predict command to create our s(x) function

s <- function(x){predict(model.surv,data.frame('size'=x),
                         type="response")} # have to use data frame bc that's what we
# used initially in the model. All we're putting into it is x. type="response" is just to make sure we 
# look at original response variable (0s and 1s)

s(4) # individual of size 4 has ~98% chance of survival

# now plot with our data
a <- min(D$size,D$sizeNext,na.rm=T) # take minimum of all observed sizes, removing NAs
b <- max(D$size,D$sizeNext,na.rm=T)
n <- 100 # how many values in our sequence
xs <- seq(a,b,length=n)

plot(D$size, D$surv, pch=21,bg=rgb(1,0,0,0.2))
lines(xs,s(xs))

#### growth model fit ####

# let's look at data, the plot of size against sizeNext. Looks pretty standard and linear, should we
# just do a normal linear regression? (better model would look at variance depending on size,
# but that's more complicated)

model.growth <- glm(sizeNext~size,data=D) # default family is normal
summary(model.growth)

# function for mean sizeNext
G.mean <- function(x){predict(model.growth,data.frame('size'=x),type="response")}
G.mean(0.5)

# plot it up
plot(D$size[winners], D$sizeNext[winners], pch=21,bg=rgb(1,0,0,0.2))
lines(xs,G.mean(xs))

# check to see if the residuals are normal
# plot a histogram of residuals
hist(model.growth$residuals) # ehhh not bad, let's go for it

# we're assuming the variation doesn't change with size, which is probably a bad assumption

# create big G function. we want some variance from the G.mean, which is mean y coming from x. We will
# base this on the residuals of the model.growth

G <- function(y,x){dnorm(y,mean=G.mean(x),
                         sd=sd(model.growth$residuals))}
# dnorm, what is the density of a normal random variable

G(1,1) # sweet this works!

#### fecundity function ####

# in order to have seeds, it has to flower first. If it flowered, they produce a number of seeds (which
# is likely size dependent). The seeds are not the things you measure the next census. You count the 
# individuals that successfully germinated. What is prob they establish, then measure them after they
# have grown to some point, since we're not measuring them IMMEDIATELY after germination. A bunch of
# steps to go through to get F function.

# read in the fecundity function
data <- D # have to redefine a few lines that are in the other file, data and parents
parents <- which(data$fec.flower==1)
source("GitHub/ECL_233/IPM-Fecundity.R") 
# a few plots come out showing size against fec.flower and size[parents] against fec.seed[parents]

F.all(1,1) # this works!

#### create our kernel K ####

K <- function(y,x)s(x)*G(y,x)+F.all(y,x)
K(1,3) # infinitesmal contribution of individuals from size 1 to size 3. Can think of this as a 
# matrix entry

#### discretize ####

# define delta x

deltax <- (b-a)/n
xs <- seq(a+deltax/2,b-deltax/2,length=n) # we wanna look at midpoints so we use a+deltax/2

# create our matrix model
A <- outer(xs,xs,K)*deltax # iterate to create every matrix entry

# what does the kernel look like
filled.contour(xs,xs,A,plot.title = title(xlab="who you contribute to",
                                          ylab="your current size")) # diagonal patch is growth, the vertical one is births
# y axis is your current size, x axis is who you contribute to

# let's do some matrix shit

estuff <- eigen(A)
lambda <- Re(estuff$values[1])
v <- Re(estuff$vectors[,1])
v <- v/sum(v)
plot(xs,v) # distribution of sizes would look like this if population continued growing
# bimodal- lots of adults producing lots of kids, in between period is pretty transient
lambda
