# Computer Practical 6 - Multi species interactions 1

# clear environment
rm(list=ls())

# call on required packages
library(tidyverse)
library(ggplot2)
library(deSolve)


# STEP ONE: IMPLEMENTATION OF THE BASE LOTKA-VOLTERRA MODEL

# using ode() to solve a differential equation, we had to implement an Rfunction
# representing the differential equaltion representing logistic growth...

# dP/dt = r (1 - P/K)P
#logistic_growth <- function(times,state,parameters){ ##logistic grown function, that takes a set of parameter values, initial conditions and a time sequence
#  with(as.list(c(state, parameters)),{ ##"with" is a function that allows us to use the variable names directly - it looks for r, K and P in state and parameters
    
#    dP <- r*(1-P/K)*P ##this is our logistic equation governing the rate of change of P
    
#    return(list(dP)) ## return the rate of change - it needs to be a list
#  }) # end with(as.list ...
#}

# we can modify this include two different differential equations representing
# a predator-prey Lotka-Volterra model (both in the same function)

# dx/dt = ax - bxy
# dy/dt = cxy - dy
lotka_volterra_exponential <- function(times,state,parameters){ #logistic growth function, that takes a set of parameter values, initial conditions and a time sequence
    with(as.list(c(state, parameters)), { #"with" is a function that allows us to use the variable names directly - it looks for r, K and P in state and parameters
      
      dX <- a*X - b*X*Y # this is our equation for the prey dynamics
      dY <- c*X*Y - d*Y # this is the equation for predator dynamics
      
      # return the rate of change
      list(c(dX, dY))
    }) # end with(as.list ...
}


# initial population values
state <- c(X=10, Y=10)
# equation parameters
parameters <- c(a=0.1, b=0.02, c=0.02, d=0.4)
#sequence of time steps
times <- seq(0,500,by=0.01)

# call the function
out <- ode(y=state, times = times, func = lotka_volterra_exponential, parms = parameters)
out_df <- data.frame(out)

# we can then plot the output
predator_prey_plot <- ggplot(data = out_df)+
  geom_line(mapping=aes(x=time,y=X),color="lightblue") +
  geom_line(mapping=aes(x=time,y=Y),color="blue") +
  geom_hline(yintercept=0,color="darkgrey") +
  geom_vline(xintercept=0,color="darkgrey") +
  labs(x = "Time", y = "P") +
  ggtitle("Predator-Prey Cycles - 10 prey, 10 predators") +
  theme_bw()
predator_prey_plot
# this gives the changes in the two populations through time

# it is also useful to plot changes in populations through time in the phase space
# i.e. it is useful to plot one predator population as a function of the prey population
phase_space_plot <- ggplot(data = out_df)+
  geom_path(mapping=aes(x=X,y=Y),color="blue") +
  xlim(0,70) +
  ylim(0,40) +
  geom_hline(yintercept=0,color="darkgrey") +
  geom_vline(xintercept=0,color="darkgrey") +
  labs(x = "Prey", y = "Predator") +
  ggtitle("Phase Space Plot - 10 prey, 10 predators") +
  theme_bw()
phase_space_plot

# now change the values of the parameters and the initial conditions, what
# happens to the outputs when you increase or decrease them
# initial population values
state <- c(X=50, Y=5) # more prey, less predators
# equation parameters
parameters <- c(a=0.1, b=0.02, c=0.02, d=0.4) # same parameters for now
# re-call the function
out2 <- ode(y=state, times = times, func = lotka_volterra_exponential, parms = parameters)
out_df2 <- data.frame(out2)
predator_prey_plot2 <- ggplot(data = out_df2)+
  geom_line(mapping=aes(x=time,y=X),color="lightblue") +
  geom_line(mapping=aes(x=time,y=Y),color="blue") +
  geom_hline(yintercept=0,color="darkgrey") +
  geom_vline(xintercept=0,color="darkgrey") +
  labs(x = "Time", y = "P") +
  ggtitle("Predator-Prey Cycles - 50 prey, 5 predators") +
  theme_bw()
predator_prey_plot2
phase_space_plot2 <- ggplot(data = out_df2)+
  geom_path(mapping=aes(x=X,y=Y),color="blue") +
  xlim(0,70) +
  ylim(0,40) +
  geom_hline(yintercept=0,color="darkgrey") +
  geom_vline(xintercept=0,color="darkgrey") +
  labs(x = "Prey", y = "Predator") +
  ggtitle("Phase Space Plot - 50 prey, 5 predators") +
  theme_bw()
phase_space_plot2
library(patchwork)
predator_prey_plot/predator_prey_plot2 # similar pattern, but higher k for both predator and prey
phase_space_plot/phase_space_plot2 # increases the number of possible states of the system

state <- c(X=10, Y=10) # same starting numbers
# equation parameters
parameters <- c(a=0.5, b=0.02, c=0.02, d=0.4) # increase hunting efficiency
# re-call the function
out3 <- ode(y=state, times = times, func = lotka_volterra_exponential, parms = parameters)
out_df3 <- data.frame(out3)
predator_prey_plot3 <- ggplot(data = out_df3)+
  geom_line(mapping=aes(x=time,y=X),color="lightblue") +
  geom_line(mapping=aes(x=time,y=Y),color="blue") +
  geom_hline(yintercept=0,color="darkgrey") +
  geom_vline(xintercept=0,color="darkgrey") +
  labs(x = "Time", y = "P") +
  ggtitle("Predator-Prey Cycles - 10 prey, 10 predators") +
  theme_bw()
phase_space_plot3 <- ggplot(data = out_df3)+
  geom_path(mapping=aes(x=X,y=Y),color="blue") +
  xlim(0,70) +
  ylim(0,60) +
  geom_hline(yintercept=0,color="darkgrey") +
  geom_vline(xintercept=0,color="darkgrey") +
  labs(x = "Prey", y = "Predator") +
  ggtitle("Phase Space Plot - 10 prey, 10 predators, increased predator efficiency") +
  theme_bw()
predator_prey_plot/predator_prey_plot3 # causes tighter osciliations 
phase_space_plot/phase_space_plot3 # increases the number of possible states of the system

state <- c(X=5, Y=50) # inverse of dfirst edit - more predators than prey
# equation parameters
parameters <- c(a=0.1, b=0.02, c=0.02, d=0.4) # increase hunting efficiency
# re-call the function
out4 <- ode(y=state, times = times, func = lotka_volterra_exponential, parms = parameters)
out_df4 <- data.frame(out4)
predator_prey_plot4 <- ggplot(data = out_df4)+
  geom_line(mapping=aes(x=time,y=X),color="lightblue") +
  geom_line(mapping=aes(x=time,y=Y),color="blue") +
  geom_hline(yintercept=0,color="darkgrey") +
  geom_vline(xintercept=0,color="darkgrey") +
  labs(x = "Time", y = "P") +
  ggtitle("Predator-Prey Cycles - 5 prey, 50 predators") +
  theme_bw()
phase_space_plot4 <- ggplot(data = out_df4)+
  geom_path(mapping=aes(x=X,y=Y),color="blue") +
  xlim(0,100) +
  ylim(0,70) +
  geom_hline(yintercept=0,color="darkgrey") +
  geom_vline(xintercept=0,color="darkgrey") +
  labs(x = "Prey", y = "Predator") +
  ggtitle("Phase Space Plot - 5 prey, 50 predators") +
  theme_bw()
predator_prey_plot/predator_prey_plot4 # population crashes? 
phase_space_plot/phase_space_plot4 # increases the number of possible states of the system
# bottom cuts off - because crashes are a potential?


# STEP TWO: PREY GROWTH RATE - EXPONENTIAL VS LOGISTIC

# lets change the equation to include logistic growth instead
# dx/dt = ax(1- x/K)

lotka_volterra_logistic <- function(times,state,parameters){ #logistic growth function, that takes a set of parameter values, initial conditions and a time sequence
  with(as.list(c(state, parameters)), { #"with" is a function that allows us to use the variable names directly - it looks for r, K and P in state and parameters
    
    dX <- a*X*(1-X/K) - b*X*Y # this is our equation for the prey dynamics, with logistic growth instead
    dY <- c*X*Y - d*Y # this is the equation for predator dynamics
    
    # return the rate of change
    list(c(dX, dY))
  }) # end with(as.list ...
}

# initial population values
state <- c(X=10, Y=10)
# equation parameters
parameters <- c(a=0.1, b=0.02, c=0.02, d=0.4, K=30)
#sequence of time steps
times <- seq(0,500,by=0.01)

# call the function
out <- ode(y=state, times = times, func = lotka_volterra_logistic, parms = parameters)
out_df <- data.frame(out)


# we can then plot the output
predator_prey_logistic <- ggplot(data = out_df)+
  geom_line(mapping=aes(x=time,y=X),color="lightblue") +
  geom_line(mapping=aes(x=time,y=Y),color="blue") +
  geom_hline(yintercept=0,color="darkgrey") +
  geom_vline(xintercept=0,color="darkgrey") +
  labs(x = "Time", y = "P") +
  ggtitle("Predator-Prey Cycles - logistic growth") +
  theme_bw()

phase_space_logistic <- ggplot(data = out_df)+
  geom_path(mapping=aes(x=X,y=Y),color="blue") +
  xlim(0,70) +
  ylim(0,40) +
  geom_hline(yintercept=0,color="darkgrey") +
  geom_vline(xintercept=0,color="darkgrey") +
  labs(x = "Prey", y = "Predator") +
  ggtitle("Phase Space Plot - logistic growth") +
  theme_bw()

predator_prey_plot/predator_prey_logistic # far less oscillations, just oscillate until settled around k
phase_space_plot/phase_space_logistic # swirly fucker?? Ask about meaning? Chaotic effects?


# STEP THREE: INCORPORTATING FUNCTIONAL RESPONSE

# prey consumption by predators is not always linear
# at some point there will be so many prey that predators will not be able to
# hunt more than they already are - due to processing time etc.

# the rate at which predators consume prey is called a functional response
# three types - I, II, and III
# type I (linear) is captured by the terms bxy and dxy in our existing model

# dx / dt = ax - bxy / 1+A*x
# dy / dt = cxy / 1+A*x - dy
# A controls the slope of the functional response 
# high A = poor hunting efficiency by the predator

# type II functional response 
x <- seq(0,50,0.1) 
A <- 0.1
y <- x / (1+A*x)

# plot functional response with different values of A

functional_response_0.1 <- ggplot() +
  geom_line(mapping=aes(x=x,y=y),color="blue") +
  geom_hline(yintercept=0,color="darkgrey") +
  geom_vline(xintercept=0,color="darkgrey") +
  labs(x = "Prey population", y = "Prey consumed") +
  ggtitle("Functional Response II - A=0.1") +
  theme_bw()

A <- 0.2
y <- x / (1+A*x)
functional_response_0.2 <- ggplot() +
  geom_line(mapping=aes(x=x,y=y),color="blue") +
  geom_hline(yintercept=0,color="darkgrey") +
  geom_vline(xintercept=0,color="darkgrey") +
  labs(x = "Prey population", y = "Prey consumed") +
  ggtitle("Functional Response II - A=0.2") +
  theme_bw()

A <- 0.4
y <- x / (1+A*x)
functional_response_0.4 <- ggplot() +
  geom_line(mapping=aes(x=x,y=y),color="blue") +
  geom_hline(yintercept=0,color="darkgrey") +
  geom_vline(xintercept=0,color="darkgrey") +
  labs(x = "Prey population", y = "Prey consumed") +
  ggtitle("Functional Response II - A=0.4") +
  theme_bw()

A <- 0.6
y <- x / (1+A*x)
functional_response_0.6 <- ggplot() +
  geom_line(mapping=aes(x=x,y=y),color="blue") +
  geom_hline(yintercept=0,color="darkgrey") +
  geom_vline(xintercept=0,color="darkgrey") +
  labs(x = "Prey population", y = "Prey consumed") +
  ggtitle("Functional Response II - A=0.6") +
  theme_bw()

A <- 0.8
y <- x / (1+A*x)
functional_response_0.8 <- ggplot() +
  geom_line(mapping=aes(x=x,y=y),color="blue") +
  geom_hline(yintercept=0,color="darkgrey") +
  geom_vline(xintercept=0,color="darkgrey") +
  labs(x = "Prey population", y = "Prey consumed") +
  ggtitle("Functional Response II - A=0.8") +
  theme_bw()

A <- 1
y <- x / (1+A*x)
functional_response_1.0 <- ggplot() +
  geom_line(mapping=aes(x=x,y=y),color="blue") +
  geom_hline(yintercept=0,color="darkgrey") +
  geom_vline(xintercept=0,color="darkgrey") +
  labs(x = "Prey population", y = "Prey consumed") +
  ggtitle("Functional Response II - A=1.0") +
  theme_bw()

functional_response_0.1+functional_response_0.2/functional_response_0.4+functional_response_0.6/functional_response_0.8+functional_response_1.0
# curve becomes sharper as A increases

# implement into the lotka volterra model
# dx / dt = ax - bxy / 1+A*x
# dy / dt = cxy / 1+A*x - dy
lotka_volterra_type2 <- function(times,state,parameters){ #logistic growth function, that takes a set of parameter values, initial conditions and a time sequence
  with(as.list(c(state, parameters)), { #"with" is a function that allows us to use the variable names directly - it looks for r, K and P in state and parameters
    
    dX <- a*X - b*X*Y/(1+A*X) # this is our equation for the prey dynamics, with logistic growth instead
    dY <- c*X*Y/(1+A*X) - d*Y # this is the equation for predator dynamics
    
    # return the rate of change
    list(c(dX, dY))
  }) # end with(as.list ...
}

# initial population values
state <- c(X=10, Y=10)
# equation parameters
parameters <- c(a=0.1, b=0.02, c=0.02, d=0.4, K=30, A=0.02)
#sequence of time steps
times <- seq(0,500,by=0.01)

# call the function
out <- ode(y=state, times = times, func = lotka_volterra_type2, parms = parameters)
out_df <- data.frame(out)

# we can then plot the output
predator_prey_type2 <- ggplot(data = out_df)+
  geom_line(mapping=aes(x=time,y=X),color="lightblue") +
  geom_line(mapping=aes(x=time,y=Y),color="blue") +
  geom_hline(yintercept=0,color="darkgrey") +
  geom_vline(xintercept=0,color="darkgrey") +
  labs(x = "Time", y = "P") +
  ggtitle("Predator-Prey Cycles - type 2 functional response") +
  theme_bw()
phase_space_type2 <- ggplot(data = out_df)+
  geom_path(mapping=aes(x=X,y=Y),color="blue") +
  xlim(0,150) +
  ylim(0,100) +
  geom_hline(yintercept=0,color="darkgrey") +
  geom_vline(xintercept=0,color="darkgrey") +
  labs(x = "Prey", y = "Predator") +
  ggtitle("Phase Space Plot - type 2 functional response") +
  theme_bw()
# check plots
predator_prey_type2/phase_space_type2

# even for small values of A, with efficient predators, the population will
# collapse, because a small decrease in predation will limit the birth rate

# combine both the logistic growth and the functional response in your model
x <- seq(0,30,0.1)
A <- 0.1
y <- x/(1+A*x)
ggplot()+
  geom_line(mapping=aes(x=x,y=y),color="blue") +
  geom_hline(yintercept=0,color="darkgrey") +
  geom_vline(xintercept=0,color="darkgrey") +
  labs(x = "Prey population", y = "Prey consumed") +
  theme_bw()


lotka_volterra2 <- function(t,state,parameters){
  with(as.list(c(state, parameters)),{
    # rate of change
    dX <- a*X*(1-X/K) - b*X*Y/(1+A*X)
    dY <- c*X*Y/(1+A*X) - d*Y
    
    # return the rate of change
    list(c(dX, dY))
  }) # end with(as.list ...
}

parameters <- c(a=0.1, b=0.02, c=0.02, d=0.4, K=30, A=0.01)
state <- c(X=10, Y=10)
times <- seq(0,500,by=0.01)
out <- ode(y=state, times = times, func = lotka_volterra2, parms = parameters)
out_df <- data.frame(out)


ggplot(data = out_df)+
  geom_line(mapping=aes(x=time,y=X),color="lightblue") +
  geom_line(mapping=aes(x=time,y=Y),color="blue") +
  geom_hline(yintercept=0,color="darkgrey") +
  geom_vline(xintercept=0,color="darkgrey") +
  labs(x = "Time", y = "P") +
  theme_bw()


ggplot(data = out_df)+
  geom_path(mapping=aes(x=X,y=Y),color="blue") +
  xlim(0,70) +
  ylim(0,40) +
  geom_hline(yintercept=0,color="darkgrey") +
  geom_vline(xintercept=0,color="darkgrey") +
  labs(x = "Prey", y = "Predator") +
  theme_bw()


# STEP FOUR: THREE SPECIES COMPETITION LOTKA-VOLTERRA MODEL - LIMITING SIMILARITY

# change to a competition model
parameters <- c(a12=1, a21=0.9, r=0.3, K = 100)
state <- c(X1=50, X2=10)

# dx1/dt = r1x1 (1 - x1/K1 - a12x2/K1)
# dx2/dt = r2x2 (1 - x2/K2 - a21x1/K2)
lotka_volterra_competition <- function(times,state,parameters){ #logistic growth function, that takes a set of parameter values, initial conditions and a time sequence
  with(as.list(c(state, parameters)), { #"with" is a function that allows us to use the variable names directly - it looks for r, K and P in state and parameters
    
    dX1 <- r*X1*(1-(X1+a12*X2)/K) # this is our equation for the prey dynamics, now considering competition
    dX2 <- r*X2*(1-(X2+a21*X1)/K) # this is the equation for predator dynamics, now considering competition
    
    # return the rate of change
    list(c(dX1, dX2))
  }) # end with(as.list ...
}

times <- seq(0,1000,by=0.01)

out <- ode(y=state, times = times, func = lotka_volterra_competition, parms = parameters)
out_df <- data.frame(out)

ggplot(data = out_df)+
  geom_line(mapping=aes(x=time,y=X1),color="lightblue") +
  geom_line(mapping=aes(x=time,y=X2),color="blue") +
  geom_hline(yintercept=0,color="darkgrey") +
  geom_vline(xintercept=0,color="darkgrey") +
  labs(x = "Time", y = "P") +
  theme_bw()

ggplot(data = out_df)+
  geom_path(mapping=aes(x=X1,y=X2),color="blue") +
  geom_hline(yintercept=0,color="darkgrey") +
  geom_vline(xintercept=0,color="darkgrey") +
  labs(x = "Species 1", y = "Species 2") +
  theme_bw()


# now we can consider three species
# the aij parameters will be determined by the relative niches of the species
# see lecture for alpha equations
rm(list=ls())

# dx1/dt = r1x1 (1 - (x1 + a12 x2 + a13 x3)/K1)
# dx2/dt = r2x2 (1 - (x2 + a21 x1 + a32 x3)/K2)
# dx3/dt = r3x3 (1 - (x3 + a31 x1 + a32 x2)/K1)
alpha_function <- function(mu1,sig1,mu2,sig2,K1,K2,start,end){ # this is the function to compute the alpha coefficients from the mean and standard deviations of the Gaussian niches of the species and the start and end values of the environment
  niche1 <- K1*dnorm(seq(start,end,length.out=100),mean=mu1,sd=sig1) # dnorm() generates the values of the Gaussian. Check ?dnorm
  niche2 <- K2*dnorm(seq(start,end,length.out=100),mean=mu2,sd=sig2)
  a <- sum(niche1*niche2)/sum(niche1*niche1) # because we have discrete values, we use a sum to approximate the integral
  return(a)
}

# Let's try different parameter values
D <- 10 # distance between the niche optima
mu1 <- 5 # niche optima of species 1
mu2 <- mu1+D # niche optima of species 2
mu3 <- mu1+2*D # niche optima of species 3
sig1 <- sig2 <- sig3 <- 10 # all species niches have the same standard deviation for simplicity
start <- 0
end <- 30
K1 <- 200 # carrying capacity species 1 and 3
K2 <- 250 # carrying capacity species 2
a12 <- alpha_function(mu1,sig1,mu2,sig2,K1,K2,start,end)
a13 <- alpha_function(mu1,sig1,mu3,sig3,K1,K1,start,end)
a21 <- alpha_function(mu2,sig2,mu1,sig1,K2,K1,start,end)
a23 <- alpha_function(mu2,sig2,mu3,sig3,K2,K1,start,end)
a31 <- alpha_function(mu3,sig3,mu1,sig1,K1,K1,start,end)
a32 <- alpha_function(mu3,sig3,mu2,sig2,K1,K2,start,end)

# visualise the niches
resource <- seq(start,end,length.out=100)
niche1 <- dnorm(resource,mean=mu1,sd=sig1)*K1
niche2 <- dnorm(resource,mean=mu2,sd=sig2)*K2
niche3 <- dnorm(resource,mean=mu3,sd=sig3)*K1
ggplot()+
  geom_line(mapping=aes(x=resource,y=niche1),color="blue")+
  geom_line(mapping=aes(x=resource,y=niche2),color="lightblue")+
  geom_line(mapping=aes(x=resource,y=niche3),color="darkblue") +
  theme_bw()

# setup and solve the system of differential equations
parameters <- c(a12=a12, a13=a13, a21=a21, a23=a23, a31=a31, a32=a32, r=0.3, K1 = K1, K2 = K2)
state <- c(X1=10, X2=10, X3=10)

lotka_volterra_three_species <- function(t,state,parameters){
  with(as.list(c(state, parameters)),{
    # rate of change
    dX1 <- r*X1*(1-(X1+a12*X2+a13*X3)/K1)
    dX2 <- r*X2*(1-(X2+a21*X1+a23*X3)/K2)
    dX3 <- r*X3*(1-(X3+a31*X1+a32*X2)/K1)
    
    # return the rate of change
    list(c(dX1, dX2, dX3))
  }) # end with(as.list ...
}

times <- seq(0,200,by=0.01)
out <- ode(y=state, times = times, func = lotka_volterra_three_species, parms = parameters)
out_df <- data.frame(out)

# plot the populations
ggplot(data = out_df)+
  geom_line(mapping=aes(x=time,y=X1),color="blue") +
  geom_line(mapping=aes(x=time,y=X2),color="lightblue") +
  geom_line(mapping=aes(x=time,y=X3),color="darkblue") +
  geom_hline(yintercept=0,color="darkgrey") +
  geom_vline(xintercept=0,color="darkgrey") +
  labs(x = "Time", y = "P") +
  theme_bw()
# species 2 outcompetes both species 1 and 3
