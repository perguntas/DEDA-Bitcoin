# Evaluation = compare the clusters to the original users 

eval = function(c_fit, size, scenario){
  truth  = read.table(paste0("allAccounts", size, ".", scenario, ".txt"), stringsAsFactors = F)
  fit    = c_fit
  helper = data.frame(fit$clustering, c(1:length(fit$clustering)))
  
  truth$cluster = helper[match(truth$ID, helper[,2]), 1]

  subset_origin  = lapply(1:size, function(x) subset(truth, truth$original == x))
  
  eval = lapply(subset_origin,  function(x) x[!duplicated(x$cluster), ])
  eval = table(unlist(lapply(eval, nrow)))
  
  return(as.data.frame.table(eval))
}
