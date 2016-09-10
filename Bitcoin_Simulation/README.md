
[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="880" alt="Visit QuantNet">](http://quantlet.de/index.php?p=info)

## [<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **Bitcoin_Simulation** [<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/d3/ia)

```yaml

Name of Quantlet : Bitcoin_Simulation

Published in : Q-Kolleg Seminar Paper

Description : 'Simulates a network of employees and firms over a time period of ten weeks with
flexible parameters regarding the frequency with which transaction types take place. Saves a file
containing the transaction data and the corresponding truth table used to simulate the data.'

Keywords : Bitcoin, Crypto, Simulation, Blockchain, Network

Author : Lara Vomfell

Submitted : Wed, Sep 8 2016 by Jiejie Zhang

```


### R Code:
```r
# install and load packages
libraries = c("random", "gtools", "reshape2")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
    install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

rm(list = ls())

# ====================== 2. Helper Functions # ======================

create.dates = function(time) {
    month = seq(as.Date("2016/1/1"), by = "month", length.out = ceiling(time/4))
    weeks = list()
    for (j in 1:length(month)) {
        weeks[[j]] = seq(month[j], by = "week", length.out = 4)
    }
    dates = do.call("c", weeks)
    return(dates)
}

# Transaction IDs # different format than the account numbers
idmaker = function(x) {
    max.val = x * 100
    count   = nchar(as.character(max.val))
    size    = paste("%0", count, "d", sep = "")
    lets    = toupper(sample(letters, x, replace = T))
    nums    = sprintf(size, sample(1:max.val)[1:x])
    ids     = paste(lets, nums, sep = "")
    return(ids)
}

# generate random probabilities vectors of 3
gen.prob = function(n) {
    m = matrix(round(runif(3 * n, 0, 1), 2), ncol = 3)
    m = round(sweep(m, 1, rowSums(m), FUN = "/"), 2)
    m
}

# sample helper function for vectors of length 1
resamp = function(x, ...) {
    if (length(x) == 1) 
        x else sample(x, ...)
}

sim = function(t, pop.size.init, settings, threshold, scenario) {
    
    tr.prob.k = settings$consumption  # per period prob. of consumption transaction
    tr.prob.j = settings$business  # per period prob. of business transaction
    tr.prob.m = runif(pop.size.init, 0.4, 0.9)  # per transaction prob. of multi-input
    tr.prob.s = settings$shadow  # per transaction prob. of change
    share     = settings$share
    
    # per period prob. of interaccount transaction
    tr.prob.h = ifelse(rep(scenario, pop.size.init) == 1, runif(pop.size.init, 0.35, threshold), 
        ifelse(rep(scenario, pop.size.init) == 2, c(runif(pop.size.init/2, 0.35, threshold), 
            runif(pop.size.init/2, threshold, 1)), ifelse(rep(scenario, pop.size.init) == 
            3, runif(pop.size.init, threshold, 1), stop("Error in Scenario"))))
    
    # per period prob. of creating new address
    tr.prob.f = ifelse(rep(scenario, pop.size.init) == 1, runif(pop.size.init, 0.35, threshold), 
        ifelse(rep(scenario, pop.size.init) == 2, c(runif(pop.size.init/2, 0.35, threshold), 
            runif(pop.size.init/2, threshold, 1)), ifelse(rep(scenario, pop.size.init) == 
            3, runif(pop.size.init, threshold, 1), stop("Error in Scenario"))))
    
    
    # ========================== 1. Attribute Dataframe # ==========================
    
    maxAcc = 4 * t
    ID = 1:pop.size.init
    
    # assign firm/employee type
    type = as.factor(sample(c(1, 2), size = pop.size.init, prob = c(share, 1 - share), replace = T))
    # create dataframe of individuals
    my.ind = data.frame(ID, type)
    
    # create some vectors for indexing
    firms = my.ind$ID[my.ind$type == 1]
    empl  = my.ind$ID[my.ind$type == 2]
    
    # set employer-employee relations and assign wages
    my.ind$work[my.ind$type == 2] = sample(firms, size = length(empl), replace = T)
    my.ind$wages[my.ind$type == 2] = sample(c(2400, 2000, 5600, 4000, 3200), size = length(empl), 
        replace = T)
    
    # set mean consumption and deviation
    my.ind$mean.cons = round(my.ind$wage * 0.15)
    
    my.ind$sd.con[my.ind$type == 2] = sample(c(1, 1.1, 1.2, 1.4), size = length(empl), replace = T)
    
    
    
    # initialize three accounts per person and assign balance according to type
    
    # assign random strings as account names
    accounts = randomStrings(n = pop.size.init * 3, len = 10, digits = T, upperalpha = F, 
        loweralpha = T, unique = T)
    
    # expand account table to allow for more accounts later
    length(accounts) = prod(dim(matrix(accounts, nrow = pop.size.init, ncol = maxAcc + 3)))
    
    mult.acc = matrix(accounts, nrow = pop.size.init, ncol = maxAcc + 3)
    
    # name account columns
    colnames(mult.acc) = c("main.acc", paste0(1:(ncol(mult.acc) - 1), ".acc"))
    
    # set account balances and adjust for firms
    balances = c(rep(2000, pop.size.init), rep(800, pop.size.init), rep(500, pop.size.init))
    
    balances[my.ind$type == 1] = c(rep(50000, length(firms)), rep(45000, length(firms)), 
        rep(30000, length(firms)))
    
    # expand balance table
    length(balances) = prod(dim(matrix(balances, nrow = pop.size.init, ncol = maxAcc + 3)))
    
    mult.bal = matrix(balances, nrow = pop.size.init, ncol = maxAcc + 3)
    
    # name balances
    colnames(mult.bal) = c("main.balance", paste0(1:(ncol(mult.bal) - 1), ".balance"))
    
    # combine to truth table
    mult    = data.frame(mult.acc, mult.bal, stringsAsFactors = F)
    my.ind  = cbind(my.ind, mult[, mixedsort(colnames(mult))])
    acc.set = list(grep("acc", colnames(my.ind))[1:3])[rep(1, pop.size.init)]
    acc.vec = sapply(acc.set, length)
    
    # create reference dates
    date.vec = create.dates(time = t)
    
    # ================ 2. Simulation # ================
    
    # list of edge.matrices over time our later anonymized transaction list for the cluster
    transaction.list = list()
    
    for (i in 1:t) {
        # loop for each time increment
        print(paste("Week", i))
        
        # create matrices for each type of transaction with columns sender, (multi-input),
        # receiver, (shadow addresse), amount
        acc.list = matrix(nrow = pop.size.init, ncol = 5)
        wage.list = matrix(nrow = length(empl), ncol = 5)
        business.list = matrix(nrow = length(firms), ncol = 5)
        cons.list = matrix(nrow = length(empl), ncol = 5)
        int.list = matrix(nrow = pop.size.init, ncol = 5)
        
        # create new account
        for (f in 1:pop.size.init) {
            if (runif(1) <= tr.prob.f[f]) {
                # new account number
                my.ind[f, (7 + acc.vec[f] * 2)] = randomStrings(n = 1, len = 10, digits = T, 
                  upperalpha = F, loweralpha = T, unique = T)
                my.ind[f, (7 + acc.vec[f] * 2) + 1] = 0
                
                # transfer some initial balance
                acc.list[f, 1] = paste(my.ind[f, sample(acc.set[[f]], size = 1)])
                acc.list[f, 2] = 2
                acc.list[f, 3] = paste(my.ind[f, (7 + acc.vec[f] * 2)])
                acc.list[f, 4] = 2
                acc.list[f, 5] = paste(round(rlnorm(1, meanlog = log(200), sdlog = log(1.1)), 
                  1))
                
                # indices
                send.in = which(Vectorize(function(x) x %in% acc.list[f, 1])(my.ind), arr.ind = TRUE)
                
                # check balance
                acc.list[f, 5] = ifelse(as.numeric(acc.list[f, 5]) < as.numeric(my.ind[send.in + 
                  c(0, 1)]), acc.list[f, 5], 0)
                
                # change balances: add/substract transaction
                my.ind[send.in + c(0, 1)] = as.numeric(my.ind[send.in + c(0, 1)]) - as.numeric(acc.list[f, 
                  5])
                
                my.ind[f, (7 + acc.vec[f] * 2) + 1] = (as.numeric(my.ind[f, (7 + acc.vec[f] * 
                  2) + 1]) + as.numeric(acc.list[f, 5]))
                
                acc.vec[f] = acc.vec[f] + 1
            }
        }
        
        # prepare actions
        cols    = data.frame(which(!is.na(my.ind), arr.ind = T))
        cols    = acast(cols, row ~ col, value.var = "col")
        cols    = split(cols, seq(nrow(cols)))
        
        acc.set = lapply(cols, function(x) intersect(x, grep("acc", colnames(my.ind))))
        
        
        # wages
        if (i%%4 == 1) {
            # wages are paid only every four periods (once a month)
            for (g in 1:length(empl)) {
                # in wage transactions, both firms and employees use their main accounts
                wage.list[g, 1] = paste(my.ind$main.acc[my.ind$work[empl[g]]])
                wage.list[g, 2] = 2
                wage.list[g, 3] = paste(my.ind$main.acc[empl[g]])
                wage.list[g, 4] = 2
                wage.list[g, 5] = paste(my.ind$wages[empl[g]])
                
                # balance check
                wage.list[g, 5] = ifelse(as.numeric(wage.list[g, 5]) < my.ind$main.balance[my.ind$work[empl[g]]], 
                  wage.list[g, 5], 0)
                
                # change balances: add wage, substract wage
                my.ind$main.balance[empl[g]] = my.ind$main.balance[empl[g]] + as.numeric(wage.list[g, 
                  5])
                
                my.ind$main.balance[my.ind$work[empl[g]]] = (my.ind$main.balance[my.ind$work[empl[g]]] - 
                  as.numeric(wage.list[g, 5]))
                
            }
        }
        
        # Business transactions
        for (j in 1:length(firms)) {
            if (runif(1) <= tr.prob.j) {
                # in business transactions, firms transfer money from any account to any other firm's
                # account
                
                if (runif(1) <= tr.prob.m[firms[j]]) {
                  # multi-input transaction
                  sender = sample(acc.set[[firms[j]]], size = 1)
                  second = sample(acc.set[[firms[j]]][acc.set[[firms[j]]] != sender], size = 1)
                  receiver = sample(firms[-j], size = 1)
                  
                  business.list[j, 1] = paste(my.ind[firms[j], sender])
                  business.list[j, 2] = paste(my.ind[firms[j], second])
                  business.list[j, 3] = paste(my.ind[receiver, sample(acc.set[[receiver]], 
                    size = 1)])
                  business.list[j, 4] = 2
                  business.list[j, 5] = paste(round(rlnorm(1, meanlog = log(350), sdlog = log(1.5)), 
                    1))
                  
                  # indices to access the correct account number plus corresponding balance
                  send.in = cbind(firms[j], sender)
                  mult.in = cbind(firms[j], second)
                  rece.in = which(Vectorize(function(x) x %in% business.list[j, 3])(my.ind), 
                    arr.ind = TRUE)
                  
                  # balance check and split amount
                  split = runif(1, 0, min(as.numeric(business.list[j, 5]), as.numeric(my.ind[send.in + 
                    c(0, 1)])))
                  
                  business.list[j, 5] = ifelse(as.numeric(my.ind[mult.in + c(0, 1)]) > as.numeric(business.list[j, 
                    5]) - split, business.list[j, 5], 0)
                  
                  # change balances: add/substract transaction
                  my.ind[send.in + c(0, 1)] = as.numeric(my.ind[send.in + c(0, 1)]) - split
                  my.ind[mult.in + c(0, 1)] = (as.numeric(my.ind[mult.in + c(0, 1)]) - (as.numeric(business.list[j, 
                    5]) - split))
                  
                  # multi-output transactions
                  if (runif(1) <= tr.prob.s & business.list[j, 5] != 0) {
                    change = runif(1, 0, 10)
                    
                    # create shadow address
                    my.ind[firms[j], (7 + acc.vec[firms[j]] * 2)] = randomStrings(n = 1, 
                      len = 10, digits = T, upperalpha = F, loweralpha = T, unique = T)
                    
                    # adjust transaction list
                    business.list[j, 4] = paste(my.ind[firms[j], (7 + acc.vec[firms[j]] * 
                      2)])
                    
                    # transfer money + change
                    my.ind[firms[j], (7 + acc.vec[firms[j]] * 2) + 1] = change
                    
                    my.ind[rece.in + c(0, 1)] = (as.numeric(my.ind[rece.in + c(0, 1)]) + 
                      as.numeric(business.list[j, 5]) - change)
                    
                    # adjust account vector
                    acc.vec[firms[j]] = acc.vec[firms[j]] + 1
                    
                  } else {
                    # else transfer whole amount
                    my.ind[rece.in + c(0, 1)] = (as.numeric(my.ind[rece.in + c(0, 1)]) + 
                      as.numeric(business.list[j, 5]))
                  }
                } else {
                  receiver = sample(firms[-j], size = 1)
                  
                  business.list[j, 1] = paste(my.ind[firms[j], sample(acc.set[[firms[j]]], 
                    size = 1)])
                  business.list[j, 2] = 2
                  business.list[j, 3] = paste(my.ind[receiver, sample(acc.set[[receiver]], 
                    size = 1)])
                  business.list[j, 4] = 2
                  business.list[j, 5] = paste(round(rlnorm(1, meanlog = log(350), sdlog = log(1.5)), 
                    1))
                  
                  # indices
                  send.in = which(Vectorize(function(x) x %in% business.list[j, 1])(my.ind), 
                    arr.ind = TRUE)
                  rece.in = which(Vectorize(function(x) x %in% business.list[j, 3])(my.ind), 
                    arr.ind = TRUE)
                  
                  # balance check
                  business.list[j, 5] = ifelse(as.numeric(business.list[j, 5]) < as.numeric(my.ind[send.in + 
                    c(0, 1)]), business.list[j, 5], 0)
                  
                  # change balances: add/subtract transaction
                  my.ind[send.in + c(0, 1)] = (as.numeric(my.ind[send.in + c(0, 1)]) - as.numeric(business.list[j, 
                    5]))
                  
                  # multi-output transactions
                  if (runif(1) <= tr.prob.s & business.list[j, 5] != 0) {
                    change = runif(1, 0, 10)
                    # create shadow address
                    my.ind[send.in[1], (7 + acc.vec[send.in[1]] * 2)] = randomStrings(n = 1, 
                      len = 10, digits = T, upperalpha = F, loweralpha = T, unique = T)
                    
                    # adjust transaction list
                    business.list[j, 4] = paste(my.ind[send.in[1], (7 + acc.vec[send.in[1]] * 
                      2)])
                    
                    # transfer money + change
                    my.ind[send.in[1], (7 + acc.vec[send.in[1]] * 2) + 1] = change
                    
                    my.ind[rece.in + c(0, 1)] = (as.numeric(my.ind[rece.in + c(0, 1)]) + 
                      as.numeric(business.list[j, 5]) - change)
                    
                    # adjust account vector
                    acc.vec[send.in[1]] = acc.vec[send.in[1]] + 1
                    
                  } else {
                    # else transfer whole amount
                    my.ind[rece.in + c(0, 1)] = (as.numeric(my.ind[rece.in + c(0, 1)]) + 
                      as.numeric(business.list[j, 5]))
                  }
                }
            }
        }
        
        # Consumption
        for (k in 1:length(empl)) {
            if (runif(1) <= tr.prob.k) {
                # in consumption, employees use any account to make purchases and transfer to any firms
                # account
                
                if (runif(1) <= tr.prob.m[empl[k]]) {
                  # multi-input transaction
                  sender = sample(acc.set[[empl[k]]], size = 1)
                  second = sample(acc.set[[empl[k]]][acc.set[[empl[k]]] != sender], size = 1)
                  receiver = sample(firms, size = 1)
                  
                  cons.list[k, 1] = paste(my.ind[empl[k], sender])
                  cons.list[k, 2] = paste(my.ind[empl[k], second])
                  cons.list[k, 3] = paste(my.ind[receiver, sample(acc.set[[receiver]], size = 1)])
                  cons.list[k, 4] = 2
                  cons.list[k, 5] = paste(round(rlnorm(1, meanlog = log(my.ind$mean.cons[empl[k]]), 
                    sdlog = log(my.ind$sd.con[empl[k]])), 1))
                  
                  # indices to access the correct account number plus corresponding balance
                  send.in = cbind(empl[k], sender)
                  mult.in = cbind(empl[k], second)
                  rece.in = which(Vectorize(function(x) x %in% cons.list[k, 3])(my.ind), 
                    arr.ind = TRUE)
                  
                  # balance check and split amount
                  split = runif(1, 0, min(as.numeric(cons.list[k, 5]), as.numeric(my.ind[send.in + 
                    c(0, 1)])))
                  
                  cons.list[k, 5] = ifelse(as.numeric(my.ind[mult.in + c(0, 1)]) > as.numeric(cons.list[k, 
                    5]) - split, cons.list[k, 5], 0)
                  
                  # change balances: add/substract transaction
                  my.ind[send.in + c(0, 1)] = as.numeric(my.ind[send.in + c(0, 1)]) - split
                  my.ind[mult.in + c(0, 1)] = (as.numeric(my.ind[mult.in + c(0, 1)]) - (as.numeric(cons.list[k, 
                    5]) - split))
                  
                  if (runif(1) <= tr.prob.s & cons.list[k, 5] != 0) {
                    change = runif(1, 0, 10)
                    # create shadow address
                    my.ind[empl[k], (7 + acc.vec[empl[k]] * 2)] = randomStrings(n = 1, len = 10, 
                      digits = T, upperalpha = F, loweralpha = T, unique = T)
                    
                    # adjust transaction list
                    cons.list[k, 4] = paste(my.ind[empl[k], (7 + acc.vec[empl[k]] * 2)])
                    
                    # transfer money + change
                    my.ind[empl[k], (7 + acc.vec[empl[k]] * 2) + 1] = change
                    
                    my.ind[rece.in + c(0, 1)] = (as.numeric(my.ind[rece.in + c(0, 1)]) + 
                      as.numeric(cons.list[k, 5]) - change)
                    
                    # adjust account vector
                    acc.vec[empl[k]] = acc.vec[empl[k]] + 1
                    
                  } else {
                    # else transfer whole amount
                    my.ind[rece.in + c(0, 1)] = (as.numeric(my.ind[rece.in + c(0, 1)]) + 
                      as.numeric(cons.list[k, 5]))
                  }
                  
                } else {
                  receiver = sample(firms, size = 1)
                  
                  cons.list[k, 1] = paste(my.ind[empl[k], sample(acc.set[[empl[k]]], size = 1)])
                  cons.list[k, 2] = 2
                  cons.list[k, 3] = paste(my.ind[receiver, sample(acc.set[[receiver]], size = 1)])
                  cons.list[k, 4] = 2
                  cons.list[k, 5] = paste(round(rlnorm(1, meanlog = log(my.ind$mean.cons[empl[k]]), 
                    sdlog         = log(my.ind$sd.con[empl[k]])), 1))
                  
                  # indices to access accounts
                  send.in = which(Vectorize(function(x) x %in% cons.list[k, 1])(my.ind), 
                    arr.ind = TRUE)
                  rece.in = which(Vectorize(function(x) x %in% cons.list[k, 3])(my.ind), 
                    arr.ind = TRUE)
                  
                  # check balance
                  cons.list[k, 5] = ifelse(as.numeric(cons.list[k, 5]) < as.numeric(my.ind[send.in + 
                    c(0, 1)]), cons.list[k, 5], 0)
                  
                  # change balances: add/subtract transaction
                  my.ind[send.in + c(0, 1)] = (as.numeric(my.ind[send.in + c(0, 1)]) - as.numeric(cons.list[k, 
                    5]))
                  
                  if (runif(1) <= tr.prob.s & cons.list[k, 5] != 0) {
                    change = runif(1, 0, 10)
                    # create shadow address
                    my.ind[send.in[1], (7 + acc.vec[send.in[1]] * 2)] = randomStrings(n = 1, 
                      len = 10, digits = T, upperalpha = F, loweralpha = T, unique = T)
                    
                    # adjust transaction list
                    cons.list[k, 4] = paste(my.ind[send.in[1], (7 + acc.vec[send.in[1]] * 
                      2)])
                    
                    # transfer money + change
                    my.ind[send.in[1], (7 + acc.vec[send.in[1]] * 2) + 1] = change
                    
                    my.ind[rece.in + c(0, 1)] = (as.numeric(my.ind[rece.in + c(0, 1)]) + 
                      as.numeric(cons.list[k, 5]) - change)
                    
                    # adjust account vector
                    acc.vec[send.in[1]] = acc.vec[send.in[1]] + 1
                    
                  } else {
                    # else transfer whole amount
                    my.ind[rece.in + c(0, 1)] = (as.numeric(my.ind[rece.in + c(0, 1)]) + 
                      as.numeric(cons.list[k, 5]))
                  }
                }
                
            }
        }
        
        # Interaccount Transactions
        for (h in 1:pop.size.init) {
            if (runif(1) <= tr.prob.h[h]) {
                # every participants transfers money between own accounts
                
                if (runif(1) <= tr.prob.m[h]) {
                  # multi-input transaction
                  sender = sample(acc.set[[h]], size = 1)
                  second = sample(acc.set[[h]][acc.set[[h]] != sender], size = 1)
                  receiver = resamp(acc.set[[h]][acc.set[[h]] != sender & acc.set[[h]] != 
                    second], size = 1)
                  
                  int.list[h, 1] = paste(my.ind[h, sender])
                  int.list[h, 2] = paste(my.ind[h, second])
                  int.list[h, 3] = paste(my.ind[h, receiver])
                  int.list[h, 4] = 2
                  int.list[h, 5] = paste(round(rlnorm(1, meanlog = log(150), sdlog = log(1.1)), 
                    1))
                  
                  
                  # indices to access the correct account number plus corresponding balance
                  send.in = cbind(h, sender)
                  mult.in = cbind(h, second)
                  rece.in = cbind(h, receiver)
                  
                  # balance check and split amount
                  split = runif(1, 0, min(as.numeric(int.list[h, 5]), as.numeric(my.ind[send.in + 
                    c(0, 1)])))
                  
                  int.list[h, 5] = ifelse(as.numeric(my.ind[mult.in + c(0, 1)]) > as.numeric(int.list[h, 
                    5]) - split, int.list[h, 5], 0)
                  
                  # change balances: add/substract transaction
                  my.ind[send.in + c(0, 1)] = (as.numeric(my.ind[send.in + c(0, 1)]) - split)
                  my.ind[mult.in + c(0, 1)] = (as.numeric(my.ind[mult.in + c(0, 1)]) - (as.numeric(int.list[h, 
                    5]) - split))
                  
                  if (runif(1) <= tr.prob.s & int.list[h, 5] != 0) {
                    change = runif(1, 0, 10)
                    # create shadow address
                    my.ind[h, (7 + acc.vec[h] * 2)] = randomStrings(n = 1, len = 10, digits = T, 
                      upperalpha = F, loweralpha = T, unique = T)
                    
                    # adjust transaction list
                    int.list[h, 4] = paste(my.ind[h, (7 + acc.vec[h] * 2)])
                    
                    # transfer money + change
                    my.ind[h, (7 + acc.vec[h] * 2) + 1] = change
                    
                    my.ind[rece.in + c(0, 1)] = (as.numeric(my.ind[rece.in + c(0, 1)]) + 
                      as.numeric(int.list[h, 5]) - change)
                    
                    # adjust account vector
                    acc.vec[h] = acc.vec[h] + 1
                    
                  } else {
                    # else transfer whole amount
                    my.ind[rece.in + c(0, 1)] = (as.numeric(my.ind[rece.in + c(0, 1)]) + 
                      as.numeric(int.list[h, 5]))
                  }
                } else {
                  sender = sample(acc.set[[h]], size = 1)
                  receiver = sample(acc.set[[h]][acc.set[[h]] != sender], size = 1)
                  
                  int.list[h, 1] = paste(my.ind[h, sender])
                  int.list[h, 2] = 2
                  int.list[h, 3] = paste(my.ind[h, receiver])
                  int.list[h, 4] = 2
                  int.list[h, 5] = paste(round(rlnorm(1, meanlog = log(150), sdlog = log(1.1)), 
                    1))
                  
                  send.in = cbind(h, sender)
                  rece.in = cbind(h, receiver)
                  
                  # check balance
                  int.list[h, 5] = ifelse(as.numeric(int.list[h, 5]) < as.numeric(my.ind[send.in + 
                    c(0, 1)]), int.list[h, 5], 0)
                  
                  # change balances: add/substract transaction
                  my.ind[send.in + c(0, 1)] = (as.numeric(my.ind[send.in + c(0, 1)]) - as.numeric(int.list[h, 
                    5]))
                  
                  if (runif(1) <= tr.prob.s & int.list[h, 5] != 0) {
                    change = runif(1, 0, 10)
                    # create shadow address
                    my.ind[h, (7 + acc.vec[h] * 2)] = randomStrings(n = 1, len = 10, digits = T, 
                      upperalpha = F, loweralpha = T, unique = T)
                    
                    # adjust transaction list
                    int.list[h, 4] = paste(my.ind[h, (7 + acc.vec[h] * 2)])
                    
                    # transfer money + change
                    my.ind[h, (7 + acc.vec[h] * 2) + 1] = change
                    
                    my.ind[rece.in + c(0, 1)] = (as.numeric(my.ind[rece.in + c(0, 1)]) + 
                      as.numeric(int.list[h, 5]) - change)
                    
                    # adjust account vector
                    acc.vec[h] = acc.vec[h] + 1
                    
                  } else {
                    # else transfer whole amount
                    my.ind[rece.in + c(0, 1)] = (as.numeric(my.ind[rece.in + c(0, 1)]) + 
                      as.numeric(int.list[h, 5]))
                  }
                }
                
                
            }
        }
        
        # combine transactions
        edge.list = rbind(acc.list, wage.list, business.list, cons.list, int.list)
        edge.list = edge.list[edge.list[, 5] != 0, ]
        edge.list = edge.list[complete.cases(edge.list), ]
        edge.list = cbind(edge.list, paste(rep(date.vec[i], nrow(edge.list))))
        
        colnames(edge.list) = c("sender", "multi", "receiver", "shadow", "amount", "date")
        
        # save transaction data
        transaction.list[[i]] = edge.list
    }
    
    transactions = do.call(rbind, transaction.list)
    transactions = cbind(idmaker(nrow(transactions)), transactions)  # colnames(trans.id)
    
    write.table(my.ind, file = paste0("my.ind", pop.size.init, ".", scenario, ".txt"))
    write.table(transactions, file = paste0("transactions", pop.size.init, ".", scenario, 
        ".txt"))
    
    print(paste("Processed", pop.size.init, scenario))
    
}

# Parameter Setting
settings = list(consumption = 0.8, business = 0.7, shadow = 0.85, share = 0.25)
params   = expand.grid(size = c(20, 50, 150), scenario = c(1:3))

results = list()
for (i in 1:nrow(params)) {
    results[[i]] = sim(t = 10, pop.size.init = params$size[i], settings = settings, threshold = 0.8, 
        params$scenario[i])
}

```
