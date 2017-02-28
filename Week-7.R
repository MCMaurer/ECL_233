# Extinction of checkerspot butterflies) 
# In the 1990s, two populations of checkerspot 
# butterflies went extinct in Northern California. 
# These extinctions were observed to coincide with a change 
# in precipitation variability in the 1970s. 
# This week we use models to examine whether 
# or not this variablity may have caused these extinction events. 

# clear variables etc
rm (list = ls ())

# read in the data

load("checkerspot.Rdata")

# creating some shorter names for the data

rain=precip.data$precip
rain.yr=precip.data$years
N=N.data$N
N.yr=N.data$years

# plotting the precip data
plot(rain.yr,rain,type="b")
# coloring in the post 1970 years
index=which(rain.yr>1970)
points(rain.yr[index],rain[index],
     type="b",pch=21,bg="red",cex=1.25)
abline(v=1971,lty=2)

# plotting the population data
plot(N.yr,N,type="b",log="y")


# To evaluate whether this shift in precipitation variability 
# may have caused the extinction of the checkerspots, 
# Mclaughlin-etal-02 developed a stochastic difference 
# equation of the following type 
# n[t+1]=n[t]*exp(a-b*n[t]+c*rain[t]^{-2})
# To fit the model to the data, 
# we rewrite it as (i.e. divide by n[t] and take logs)
# log n[t+1]/n[t]=a-b*n[t]+c*rain[t]^{-2}
# Assuming only observation error
# we will try to predict r[t]=log n[t+1]/n[t]
# using n[t] and rain[t]

# To this end, we identify years where there
# are successive population counts

n=length(N.yr)
temp=N.yr[-1]-N.yr[-n]
index=which(temp==1)

# Next we compute r[t] for these years
r=log(N[index+1])-log(N[index]) 
# identify the years of interest
ts=N.yr[index]
# next we need to get the transformed rain data for these years
rain.index=rain.yr %in% ts #
rain.transformed=rain[rain.index]^(-2)
# now we can perform the linear regression
model=lm(r~N[index]+rain.transformed)
# putting together the model 
f=function(N,W)N*exp(model$coeff[1]+model$coeff[2]*N+model$coeff[3]*W^(-2))


# plot model fit against data
one.step=f(N[index],rain[rain.index])
points(N.yr[index+1],one.step,bg="red",pch=23)


# Next we use linearizations to make predictions about 
# pre and post 1971. The linearization of the model at 
# low densities is 
# n[t+1]=n[t]*exp(a-c*rain[t]^{-2})
# Hence, the long-term growth rate is given by 
# mean(a-c*rain[t]^{-2}) where the mean is taken 
# over the rain years being sampled. 

r.function=function(W)(model$coeff[1]+model$coeff[3]*W^(-2))

# computing the pre and post 1971 long-term growth rates for the 
# linearization 

index=which(rain.yr<1971)
r.pre=mean(r.function(rain[index]))
index=which(rain.yr>1970)
r.post=mean(r.function(rain[index]))

# hence the linearization suggests that extinction should occur
# asymptotically with the post 1971 rainfall but not
# with the pre 1971 rainfall. To see that these predictions
# are consistent witht the full nonlinear model,we develop 
# a simulator for the full model. 

simulator=function(weather,Tf,reps,N.init=1000){
  N=matrix(N.init,Tf,reps)
  for(t in 1:(Tf-1)){
    temp=f(N[t,],sample(weather,size = reps,replace = TRUE))
    N[t+1,]=temp
  }
  return(N)
}



#  Finally, we run the model with random draws from the pre-1971 data and the post-1970 data as shown in Figure~\ref{fig:checkerspot4}.

index=which(rain.yr<1971)
set.seed(2)
N=simulator(rain[index],200,10)

matplot(N+1,type="l",log="y",lty=1,lwd=2,
        ylim=c(1,max(N)),ylab="density",
        xlab="year",main="pre-1971 rainfall")


index=which(rain.yr>1970)
set.seed(2)
N=simulator(rain[index],200,10)
matplot(N+1,type="l",log="y",lty=1,lwd=2,
        ylab="density",xlab="year"
        ,main="post-1971 rainfall")



# When using the pre-1971 rainfall data, 
# it can be proved (see,e.g., Schreiber 2017 review)
# that the model converges to a unique positive 
# stationary distribution. To understand this
# stationary distirbution, it sufficies to 
# run the model ONCE for a LONG time. 

index=which(rain.yr<1971)
set.seed(2)
N=simulator(rain[index],10000,1)
hist(N,50) # ploting that approximation of the stationary distribution

################################################
# ADDITIONAL STUFF BELOW WHICH YOU CAN IGNORE!!!
################################################
# 
# 
# # adding a quasi-extinction threshold of into the model
# 
# simulator2=function(weather,Tf,reps,N.init=1000,threshold=0){
#   N=matrix(N.init,Tf,reps)
#   for(t in 1:(Tf-1)){
#     temp=f(N[t,],sample(weather,size = reps,replace = TRUE))
#     temp2=which(temp<threshold)
#     if(length(temp2)>0)temp[temp2]=0
#     N[t+1,]=temp
#   }
#   return(N)
# }
# 
# # extimating extinction
# 
# index=which(rain.yr>1970)
# set.seed(2)
# N=simulator2(rain[index],500,1000,threshold=1)
# 
# matplot(N+1,type="l",log="y",lty=1,lwd=2,
#         ylim=c(1,max(N)),ylab="density",
#         xlab="year",main="pre-1971 rainfall")
# 
# # estimating distribution of quasi-extinction times (roughly exponential)
# 
# max(N[500,])
# hist(colSums(sign(N)))
# mean(colSums(sign(N)))
# 
# 
# 
# # another approach to doing this using quasi-stationary distibution
# # requires a threshold>0
# 
# simulator3=function(weather,Tf,N.init=1000,threshold=1){
#   N=numeric(Tf)
#   N[1]=N.init
#   cross=matrix(0,Tf,1)
#   for(t in 1:(Tf-1)){
#     temp=f(N[t],sample(weather,size = 1))
#     if(temp<threshold){
#       temp=sample(N[1:t],size=1);
#       cross[t]=1}
#     N[t+1]=temp
#   }
#   return(list(N=N,cross=cross))
# }
# 
# 
# index=which(rain.yr>1970)
# set.seed(2)
# N=simulator3(rain[index],1000,threshold=1)
# 
# hist(N)
# 
# 
