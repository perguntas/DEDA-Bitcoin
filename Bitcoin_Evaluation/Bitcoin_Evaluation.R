path = ""
setwd(path)

library(cluster)
library(skmeans)
library(reshape2)

source("matching_function.R")
source("parser.R")
source("helper_functions.R")
source("comparison_function.R")
date.vec = create.dates(10)


settings = expand.grid(j = c(20, 50, 150), i = 1:3, eval = c(1, 2))
for (f in 1:nrow(settings)){
  
  # load transaction data
  assign(paste0("transactions", settings$j[f], ".", settings$i[f]), 
         as.matrix(read.table(paste0("transactions", settings$j[f], ".", settings$i[f], ".txt"))))
  
  # parse transaction data to obtain matched address table
  parsing(data = get(paste0("transactions", settings$j[f], ".", settings$i[f])), 
          scenario = settings$i[f], size = settings$j[f])
  
  # transform transaction data to prepare dissimilarity measure calculation
  assign(paste0("address.tables", settings$j[f], ".", settings$i[f]), 
         prepare.trs(transaction.data = get(paste0("transactions", settings$j[f], ".", settings$i[f])),
                     parser = read.table(paste0("allAccounts", settings$j[f], ".", settings$i[f], ".txt")),
                     dates = date.vec, intervals = c(0, 150, 180, 200, 300, 500, 1000, 2000, 4000, 5600)))
  
  # make difference between inclusion of prior knowledge
  if (settings$eval[f] == 1){
    
    # calculate adjusted distance matrix
    assign(paste0("dist", settings$j[f], ".", settings$i[f], "prior"),
           prior_function(transaction.data = get(paste0("transactions", settings$j[f], ".", settings$i[f])),
                          dates = date.vec, intervals = c(0, 150, 180, 200, 300, 500, 1000, 2000, 4000, 5600),
                          parser = read.table(paste0("allAccounts", settings$j[f], ".", settings$i[f], ".txt")),
                          address.tables = get(paste0("address.tables", settings$j[f], ".", settings$i[f]))))
    
    # calculate HAC fit
    #assign(paste0("hc_fit", settings$j[f], ".", settings$i[f], "prior"), 
    #       hclust(get(paste0("dist", settings$j[f], ".", settings$i[f], "prior"))))
    
    } else {
    
    # calculate distance matrix
    assign(paste0("dist", settings$j[f], ".", settings$i[f]), 
           as.dist(skmeans_xdist(get(paste0("address.tables", settings$j[f], ".", settings$i[f])))))
    
    # calculate HAC fit
    #assign(paste0("hc_fit", settings$j[f], ".", settings$i[f]), 
    #       hclust(get(paste0("dist", settings$j[f], ".", settings$i[f]))))
  }
}

# Decision on k
# par(mfrow=c(3,3))
# for (f in 1:(nrow(settings)/2)){
#   plot(get(paste0("hc_fit", settings$j[f], ".", settings$i[f], "prior")), 
#        xlab = paste("Fit for Size ", settings$j[f], "Scenario", settings$i[f], "Prior"))
# }
# 
# for (f in (nrow(settings)/2 + 1):nrow(settings)){
#   plot(get(paste0("hc_fit", settings$j[f], ".", settings$i[f])),
#        xlab = paste("Fit for Size ", settings$j[f], "Scenario", settings$i[f]))
# }
# dev.off()

k_choice     = rbind(expand.grid(j = 20, i = 1:3, k = c(20:120), eval = c(1, 2)), 
                     expand.grid(j = 50, i = 1:3, k = c(50:150), eval = c(1, 2)),
                     expand.grid(j = 150, i = 1:3, k = c(150:250)), eval = c(1, 2))
      
k_choice$asw = numeric(nrow(k_choice))
k_choice     = k_choice[with(k_choice, order(eval, i, j)), ]


for (m in 1:nrow(k_choice)){
  # find k medoids with highest silhouette coefficient
  if (k_choice$eval[m] == 1){
    k_choice$asw[m] = pam(get(paste0("dist", k_choice$j[m], ".", k_choice$i[m], "prior")), 
                          k = k_choice$k[m], diss = T)$silinfo$avg.width

  } else {
    k_choice$asw[m] = pam(get(paste0("dist", k_choice$j[m], ".", k_choice$i[m])), 
                          k = k_choice$k[m], diss = T)$silinfo$avg.width
  }
}

vec    = c(1:101)
k_best = as.data.frame(matrix(0, 
                              nrow = (nrow(k_choice)/3), 
                              ncol = ncol(k_choice)))
colnames(k_best) = colnames(k_choice)

for (i in 1:nrow(settings)){
  k_best[i, ] = k_choice[vec[which.max(k_choice$asw[vec])], ]
  
  if (k_best$eval[i] == 1){
    assign(paste0("k_fit", k_best$j[i], ".", k_best$i[i], ".", k_best$k[i], "prior"),
          pam(get(paste0("dist", k_best$j[i], ".", k_choice$i[i], "prior")), k = k_best$k[i]))
    
    assign(paste0("results", k_best$j[i], ".", k_best$i[i], ".", k_best$k[i], "prior"),
           eval(c_fit = get(paste0("k_fit", k_best$j[i], ".", k_best$i[i], ".", k_best$k[i], "prior")),
                size = k_best$j[i], scenario = k_best$i[i]))
  } else {
    assign(paste0("k_fit", k_best$j[i], ".", k_best$i[i], ".", k_best$k[i]),
           pam(get(paste0("dist", k_best$j[i], ".", k_choice$i[i])), k = k_best$k[i]))
    
    assign(paste0("results", k_best$j[i], ".", k_best$i[i], ".", k_best$k[i]),
           eval(c_fit = get(paste0("k_fit", k_best$j[i], ".", k_best$i[i], ".", k_best$k[i])),
                size = k_best$j[i], scenario = k_best$i[i]))
  }
  vec = vec + 101
}
