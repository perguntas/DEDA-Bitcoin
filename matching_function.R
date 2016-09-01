prepare.trs = function(transaction.data, parser, dates, intervals){
  data = as.data.frame(transaction.data, stringsAsFactors = F)
  
  # match accounts to their current ID
  data[, grepl("sender|multi|receiver|shadow", colnames(data))] = t(apply(data[, grepl("sender|multi|receiver|shadow", colnames(data))], 1, 
                                                                           function(x) parser[match(x, parser$accounts), 2]))
  data = data[, !grepl("multi", colnames(data))]
  
  # keep all unmatched shadows
  tmp = data[complete.cases(data),]
  tmp = tmp[tmp[, 2] != tmp[, 4],]
  tmp = rbind(tmp[, -4], setNames(tmp[, -3], names(tmp[, -4])))
  
  data = rbind(data[, -4], tmp)
  
  # recode numeric amount into intervals
  data$amount   = as.numeric(data$amount) # date as factor?
  data$interval = findInterval(data$amount, intervals)
  
  # create GAs
  data.list = lapply(1:parser$ID[nrow(parser)], 
                      function(x) subset(data, data$sender == x | data$receiver == x)) 
  
  unifiedGA = lapply(seq_along(data.list), 
                      function(i) as.numeric(ifelse(data.list[[i]]$sender == i, data.list[[i]]$receiver, data.list[[i]]$sender)))
  data.list = lapply(seq_along(unifiedGA), function(x) data.frame(data.list[[x]], GA = unifiedGA[[x]]))
  data.list = lapply(data.list, "[", -c(1:4))
  
  # create factor levels
  GAs.levels = paste(1:length(data.list))
  day.levels = levels(as.factor(dates))
  int.levels = levels(as.factor(data$interval))
  
  # transform everything
  data.list = lapply(data.list, function(x) {x$GA       = factor(x$GA,       levels = GAs.levels); x})
  data.list = lapply(data.list, function(x) {x$date     = factor(x$date,     levels = day.levels); x})
  data.list = lapply(data.list, function(x) {x$interval = factor(x$interval, levels = int.levels); x}) 
  
  data.list = lapply(data.list, function(x) lapply(x, summary))
  data.list = lapply(data.list, function(x) c(unlist(x)))
  
  final.data = do.call(rbind, data.list)
  
  return(final.data)
}


prior_function = function(transaction.data, dates, address.tables, parser, intervals){
  dist = skmeans_xdist(address.tables)
  
  data = as.data.frame(transaction.data, stringsAsFactors = F)
  # match accounts to their current ID
  data[, grepl("sender|multi|receiver|shadow", colnames(data))] = t(apply(data[, grepl("sender|multi|receiver|shadow", colnames(data))], 1, 
                                                                           function(x) parser[match(x, parser$accounts),2]))
  data = data[,!grepl("multi", colnames(data))]
  
  # keep all unmatched shadows
  tmp = data[complete.cases(data),]
  tmp = tmp[tmp[, 2] != tmp[, 4],]
  tmp = rbind(tmp[, -4], setNames(tmp[,-3], names(tmp[,-4])))
  
  data = rbind(data[, -4], tmp)
  
  # recode numeric amount into intervals
  data$amount   = as.numeric(data$amount) 
  data$interval = findInterval(data$amount, intervals)
  
  prior = data[data$date %in% as.character(dates[c(1, 5, 9)]) & data$interval > 6, ]
  print(nrow(prior))
  
  for (i in 1:nrow(prior)){
    dist[prior$sender[i], prior$receiver[i]] = 1
    dist[prior$receiver[i], prior$sender[i]] = 1
  }
  return(as.dist(dist))
}

