disp <- function(y,x){exp(-abs(y-m-x)/a)/(2*a)}

L <- 10
a <- 2
m <- 1

disp(1,1)

K <- function(t,y){R*k(y,x)}


n <- 100



deltax <- (L--L)/n
xs <- seq(-L+deltax/2,L-deltax/2,length=n)

A <- outer(xs,xs,K)*deltax

filled.contour(xs,xs,A)


#### here's the plan ####

# step one is create the projection matrix from the IPM
  # create the K, discretize, all that jazz.
  # create a starting population vector of densities of 1 for x between -2 and 2
  # multiply this starting vector by the projection matrix to get your distro for t=2
  # repeat for all the time steps


# now for the second question
# just recreate A a bunch of times with different L values
# calculate the dominant eigenvalue for each of the new matrices with different L values
# plot eigenvalues against L values
