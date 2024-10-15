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


