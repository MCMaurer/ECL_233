# Choice (b) predator-prey
# Create phase plane diagrams (predator density vs. prey density, where lines are trajectories over time) for the predator-prey mode:
# dN/dt = r*F(N)-b*G(N)*P # prey 
# dP/dt = c*G(N)*P-m*P # predator 
# for four scenarios:
# (i) type I functional response (G(N)=N) and no density dependence in prey (F(N)=N)
# (ii) type I functional response (G(N)=N) and density dependence in prey (F(N)=N*(1-N/K))
# (iii) type II functional response (G(N)=N/(1+d*N)) and density dependence in prey (F(N)=N*(1-N/K))
# (iv) type III functional response (G(N)=N^2/(1+d*N^2)) and density dependence in prey (F(N)=N*(1-N/K))
# in addition to plotting trajectories, mark the starting and ending points. Bonus if you also put a point at the internal equilibrium (equilibrium with nonzero values for both prey and predator) for each model.
# some parameter recommendations, you can alter these as you see fit:
# b = 0.01 # predator attack rate
# c = 0.1*b # predator conversion of preation into reproduction
# m = 0.2 # predator mortality
# a = 1 # rate of prey capture/unit prey and time
# d = 0.0015 # handling time
# r = 0.5 # prey growth rate
# K = 1000 # prey carrying capacity
# N0=300, P0=50 for initial population sizes, although you might want to try a few different initial population sizes per plot



PredPrey <- function(t,n,parms){
  with(as.list(parms)){
    dn <- r*N - b*N*P
  }
}