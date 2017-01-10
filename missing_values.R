#----------------------------------------------
#               MISSING VALUES
#----------------------------------------------


get_missing = function(data){
    nas = rep(0,ncol(data))
    sparse_cols = c()
    for (i in 1:ncol(data)){
        nas[i] = sum(is.na(data[,c(i)]))
        if (nas[i] != 0){
            sparse_cols = c(sparse_cols,i)
            cat(colnames(data)[i], ":", nas[i],"\n")
        }
    }
    return(sparse_cols)
}
sparse_idx = get_missing(tab)
tab_full = tab[,-sparse_idx]

fill_mean = function(data, sparse_idx){
    for (i in sparse_idx){
        data[which(is.na(data[,i])),i] = mean(as.matrix(data[-which(is.na(data[,i])),i]))
    }
    return(data)
}

tab_mean_filled = fill_mean(tab, sparse_idx)

fill_median = function(data, sparse_idx){
    for (i in sparse_idx){
        data[which(is.na(data[,i])),i] = median(as.matrix(data[-which(is.na(data[,i])),i]))
    }
    return(data)
}



tab_med_filled = fill_median(tab, sparse_idx)