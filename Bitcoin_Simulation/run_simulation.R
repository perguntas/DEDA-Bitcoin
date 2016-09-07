#--------------------------------------------------
# Quantlet: Simulating a Bitcoin Network 
#--------------------------------------------------
# Description: Simulates a network of employees
# and firms over a time period of ten weeks with
# flexible parameters regarding the frequency
# with which transaction types take place.
#--------------------------------------------------
# Keywords: Bitcoin, blockchain
#--------------------------------------------------
# Author: Lara Vomfell, 2016 / 29 / 08
#--------------------------------------------------


path = ""
setwd(path)

library(random)
library(gtools)
library(reshape2)

rm(list=ls())

source("helper_functions.R")
source("simulation_function.R")

# Parameter Setting
settings = list(consumption = 0.8, business = 0.7, shadow = 0.85, share = 0.25)
params   = expand.grid(size = c(20, 50, 150), scenario = c(1:3))

results = list()
for (i in 1:nrow(params)){
  results[[i]] = sim(t = 10, pop.size.init = params$size[i], 
                      settings = settings, threshold = 0.8, params$scenario[i])
}

