#----------------------------------------------
#               MISSING VALUES
#----------------------------------------------

# Environment
require("kknn")



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



#________________________________________________
# Imputation


# Fills sparse columns using nearest neighbours given their indexes
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

tab_knn_train = tab_knn_filled[-test_indexes,]
tab_knn_test = tab_knn_filled[test_indexes,]






#_______________________________________________________________________________________________________
# STOP HERE
#_______________________________________________________________________________________________________
#_______________________________________________________________________________________________________
#



require("mclust")
require("mix")

# Fills sparse columns using gaussian mixture modeling
tab_gm_filled = as.data.frame(imputeData(tab, categorical = NULL, seed = 7))


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

#________________________________________________
# Analysis 
library("VIM")

# Global ratio of NAs
global_NAs = sum(is.na(tab))/(sum(is.na(tab))+sum(!is.na(tab)))

# Missing values from Lemas survey
lemas_na = sparse_idx[-1]
tab_lemas = tab[,lemas_na]

aggr(tab_sparse, prop = F, numbers = T, cex.axis = 0.40) 
matrixplot(tab_lemas, interactive = F)

na_idx = is.na(tab[,lemas_na[1]])

matrixplot(tab_lemas[!na_idx,], interactive = F)


hist(tab$LemasSwornFT[!na_idx])

