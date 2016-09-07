[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="880" alt="Visit QuantNet">](http://quantlet.de/index.php?p=info)

## [<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **Bitcoin_Simulation** [<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/d3/ia)

```yaml

Name of Quantlet : Bitcoin_Simulation


Description : 'Simulates a network of employees and firms over a time period of ten weeks with flexible parameters regarding the frequency with which transaction types take place. It saves a file containing the transaction data and the corresponding truth table used to simulate the data.'

Keywords : 'Bitcoin, Blockchain, Crypto Privacy'

Author : Lara Vomfell

```


### R Code:
```r
# requires an internet connection
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


```
