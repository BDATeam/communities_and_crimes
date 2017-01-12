#----------------------------------------------
#               MISSING VALUES
#----------------------------------------------

# Get number of missing values and indexes of sparse columns
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
tab_sparse = tab[,sparse_idx]


# Imputation ------------------------------------


# Fills sparse columns using nearest neighbours given their indexes
require("kknn")
fill_nn = function(data, sparse_idx){
    for (i in sparse_idx){
        pr_idx = which(is.na(data[,i]))
        y = as.matrix(data[,i]) 
        X = as.matrix(data[,-sparse_idx])
        nn = kknn(y~X, train = data.frame(y,X), test = NULL, k = 5, distance = 2)
        data[pr_idx,i] = nn$fitted.values[pr_idx]
    }
    return(data)
}

tab_knn_filled = fill_nn(tab, sparse_idx)

# Fills sparse columns with their mean given their indexes
fill_mean = function(data, sparse_idx){
    for (i in sparse_idx){
        data[which(is.na(data[,i])),i] = mean(as.matrix(data[,i]), na.rm=TRUE)
    }
    return(data)
}

tab_mean_filled = fill_mean(tab, sparse_idx)

# Fills sparse columns with their median given their indexes
fill_median = function(data, sparse_idx){
    for (i in sparse_idx){
        data[which(is.na(data[,i])),i] = median(as.matrix(data[,i]), na.rm=TRUE)
    }
    return(data)
}

tab_med_filled = fill_median(tab, sparse_idx)
