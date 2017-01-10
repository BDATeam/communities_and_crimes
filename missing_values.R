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