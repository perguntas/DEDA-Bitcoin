#======================
# 2. Helper Functions #
#======================

create.dates = function(time){
  month = seq(as.Date("2016/1/1"), by = "month", length.out = ceiling(time/4))
  weeks = list()
  for (j in 1:length(month)){
    weeks[[j]] = seq(month[j], by = "week", length.out = 4)
  }
  dates = do.call("c", weeks)
  return(dates)
}

# Transaction IDs # different format than the account numbers
idmaker = function(x){
  max.val = x * 100
  count   = nchar(as.character(max.val)) 
  size    = paste("%0", count, "d", sep = "")
  lets    = toupper(sample(letters,x , replace = T)) 
  nums    = sprintf(size, sample(1:max.val)[1:x])
  ids     = paste(lets, nums, sep = "")  
  return(ids)
}

# generate random probabilities vectors of 3
gen.prob = function(n) {
  m = matrix(round(runif(3 * n, 0, 1),2), ncol = 3)
  m = round(sweep(m, 1, rowSums(m), FUN= "/"), 2)
  m
}

# sample helper function for vectors of length 1
resamp = function(x,...){
  if(length(x) == 1) x 
  else sample(x,...)
}
