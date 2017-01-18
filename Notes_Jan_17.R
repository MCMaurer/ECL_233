#### for loops, baby! ####
x <- 1
if(x>=0) y=x
y

x <- -1
if(x>=0 & y>=0) z<- x+y
z

# | is or for operators

if(x>=0 | y>=0) z<- x+y
z

# we can do ifelse
if(x>=0) y<-x else y<- -x
y

v <- -5:5
v
v > 0

# in R's programming, false is equivalent to 0, true is equivalent to 1

(v>0)*4

#### while loops folks, ####

# while performs an action while the parentheses stuff is true, stop as soon as it's false
# doing stuff with BevHo now. It's the integrated form of the logistic equation, btw
# N(t+1) = aN(t)/(1+bN(t))

bevho <- function(nt, alpha=1.2, beta=0.001){ # putting in some default values here
  nt1 <- alpha*nt/(1+beta*nt)
  return(nt1)
}

bevho(2)
bevho(2,alpha=2)

# useful to try code on something you KNOW the answer for

bevho(2,alpha=2,beta=0)

# let's look at equilibria to test our model
a=1.5
b=0.005
Nbar=(a-1)/b
bevho(Nbar, alpha=a, beta=b)

# back to while loops

# let's run a while loop until we hit equilibrium, where the output is the same as the input, 
# or close within some margin of error
minDiff <- 0.01 # minimum difference that signals reached equilibrium (reached margin of error), 
                # starting with a big difference

n0 <- 100 # initial pop size

n <- rep(NA,2)
n[1] <- n0 # set first time point
n[2] <- bevho(n[1]) # second time point
t <- 2 # at time 2 to start
tmax <- 1000 # maximum time to run for. there's always danger of infinite while loops, 
              # so you should include a tmax

# now the loop
while(abs(n[t]-n[t-1])>minDiff & t<tmax){ # while n(t) and n(t-1) are still far apart, keep running
  n[t+1] <- bevho(n[t]) # update population size
  t <- t+1
  #print(n) # show pop size
  #print(t) # show time
}

plot(1:t, n, xlab="time", type="l")

#### for loops,, again brother ####

# for loops perform an action for each value of x from xmin to xmax

rm(n) # clear n to start over

# it's more computationally efficient to generate an ENTIRE vector at once (like with a for loop) than
# to add to it one piece at a time (like we did in the while loop)

tf <- 100 # final time
n <- rep(NA,tf) # sets aside a vector for the time series
n[1] <- n0 # initialize pop size
for(t in 1:(tf-1)){ # only going to 4 because we are using n[t+1] below, which gives us the value for n(t=5)
  n[t+1] <- bevho(n[t]) # step through beverton holt
  #print(t)
  #print(n)
}

plot(1:tf, n, xlab="time", type="l")

# for loops are powerful but take time

y <- rep(NA,10)
for(x in 1:10) y[x] <- exp(x)
y
#this is equivalent to
exp(1:10)

# vectorization is faster, let's do our for loop but with a range of initial pop sizes

# little note on matrices
M <- matrix(1:10,nrow=2,byrow=2)
M

# clear values of n and t
rm(n)
rm(t)

tf=50
vn0 <- seq(10,300,by=10) # vector of initial pop sizes
Nn0 <- length(vn0) # number of initial conditions
n <- matrix(NA,nrow=tf,ncol=Nn0) # create empty matrix for time series for all initial conditions
n[1,] <- vn0 # initialize pop size, filling first row
for(t in 1:(tf-1)){ # loop all initial conditions through time
  n[t+1,] <- bevho(n[t,]) # step through beverton holt
}
matplot(1:tf,n,type="l",xlab="time", ylab="population size")

nrow(1:tf)
nrow(n)

# bifurcation plots

# plot points for values for each value of r, but only the LAST ___ number of time steps

#### Sebastian's up ####



