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