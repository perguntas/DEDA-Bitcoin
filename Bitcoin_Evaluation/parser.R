# Heuristic 1: multi-input transactions are by the same address holder Heuristic 2:
# output addresses which have not appeared before are shadow addresses and belong to the
# same input address holder

parsing = function(data, size, scenario, original) {
    # no of unique addresses
    uniqueAdd = unique(c(data[, 2:5][data[, 2:5] != 2]))
    print.put = length(unique())
    
    # take subset of transaction log with multiple transactions, remove duplicates
    heuristic1 = as.data.frame(subset(data[, 2:3], data[, 3] != 2), stringsAsFactors = F)
    sorted = t(apply(heuristic1, 1, sort))
    heuristic1 = heuristic1[!duplicated(sorted), ]
    print.out = length(unique(unlist(c(heuristic1[, 1:2]))))
    print(paste("Heuristic I:", round(print.out/length(uniqueAdd), 4), "of all addresses matched"))
    
    # keep only those multi output transactions where the address has not appeared before
    heuristic2 = as.data.frame(matrix(nrow = nrow(data), ncol = 2))
    for (i in 1:nrow(data)) {
        if (data[i, 4] != 2 & data[i, 5] != 2) {
            if (sum(data[i, 4:5] %in% data[1:(i - 1), ]) == 1) {
                heuristic2[i, ] = c(data[i, 2], data[i, 4:5][which(!data[i, 4:5] %in% data[1:(i - 
                  1), ])])
            }
        }
    }
    heuristic2 = heuristic2[complete.cases(heuristic2), ]
    print(paste("Heuristic II:", round(nrow(heuristic2)/length(uniqueAdd), 4), "of all addresses matched"))
    
    
    colnames(heuristic1) = colnames(heuristic2) = c("A", "B")
    
    # assign unique ID to each transaction that is copied to any transaction using the
    # account
    heuristics    = rbind(heuristic1, heuristic2)
    heuristics    = unique(heuristics)
    heuristics$ID = 1:nrow(heuristics)
    
    print(paste("Heuristics I + II", round(nrow(heuristics)/length(uniqueAdd), 4), "of all addresses matched"))
    
    for (i in 1:nrow(heuristics)) {
        check = heuristics[i, 1:2]
        heuristics[apply(apply(heuristics, 2, function(x) x %in% check), 1, function(x) any(x) == 
            T), 3] = heuristics[i, 3]
    }
    
    # melt the dataframe to obtain the ID for each account
    heuristics.data = melt(heuristics, id.vars = "ID")
    
    # obtain ordered ID
    heuristics.data    = unique(heuristics.data[, c(1, 3)])
    heuristics.data$ID = as.numeric(as.factor(heuristics.data$ID))
    
    allAccounts    = data.frame(accounts = uniqueAdd, ID = NA, stringsAsFactors = F)
    allAccounts$ID = heuristics.data$ID[match(allAccounts$accounts, heuristics.data$value)]
    allAccounts    = allAccounts[order(allAccounts$ID), ]
    
    allAccounts$ID[is.na(allAccounts$ID)] = seq.int(max(allAccounts$ID, na.rm = T) + 1, 
        max(allAccounts$ID, na.rm = T) + sum(is.na(allAccounts$ID)))
    allAccounts$ID = as.character(allAccounts$ID)
    
    # match to original
    original = read.table(paste0("my.ind", size, ".", scenario, ".txt"), stringsAsFactors = F)
    helper   = apply(original, 2, function(x) match(allAccounts$accounts, x))
    
    allAccounts$original = apply(helper, 1, function(x) x[!is.na(x)])
    
    # use scenario argument to write correct table
    write.table(allAccounts, file = paste0("allAccounts", size, ".", scenario, ".txt"))
    
    
}
