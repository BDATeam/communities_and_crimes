#----------------------------------------------
#           EXTREME GRADIENT BOOSTING
#----------------------------------------------

#   Environment
library(xgboost)

#   Explore data
summary(tab$ViolentCrimesPerPop)

#   Define error functions
rmse = function(pred){return(sqrt(mean((pred-tab$ViolentCrimesPerPop)^2)))}


xgb_cv = function(data,params, n_rounds){
    
    pred = as.matrix(rep(0,nrow(data)))
    #Randomly shuffle the data
    new_indexes = sample(nrow(data))
    data_shuffle<-data[new_indexes,]
    
    #Create 10 equally sized folds
    folds <- cut(seq(1,nrow(data_shuffle)),breaks=10,labels=FALSE)
    
    #Perform 10 fold cross validation
    for(i in 1:10){
        #Segement your data by fold using the which() function 
        testIndexes <- which(folds==i,arr.ind=TRUE)
        real_testIndexes = new_indexes[testIndexes]
        testData <- data_shuffle[testIndexes, ]
        trainData <- data_shuffle[-testIndexes, ]
        
        dtrain <- xgb.DMatrix(data = as.matrix(trainData[,-ncol(trainData)]), label = as.matrix(trainData$ViolentCrimesPerPop))
        dtest <- xgb.DMatrix(data = as.matrix(testData[,-ncol(trainData)]), label = as.matrix(testData$ViolentCrimesPerPop))
        
        #regression
        gbr = xgb.train(params, dtrain, nrounds=n_rounds)
        predictions <- predict(gbr, dtest)
        pred[real_testIndexes] = predictions
    }
    
    error = rmse(pred)
    return(error)
}

param = list(max_depth = 5)

# Results --------------------------------------------

rep_pred = function(data, param, n_rounds){
    err = data.frame(30,1)
    for (k in 1:30){
        set.seed(k)
        err[k,] = xgb_cv(data=data, params = param, n_rounds)
    }
    return(err) 
}

tabErr = rep_pred(tab, param, 20)
tab_fullErr = rep_pred(tab_full, param, 20)
tab_gm_filledErr = rep_pred(tab_gm_filled, param, 20)
tab_knn_filledErr = rep_pred(tab_knn_filled, param, 20)
tab_med_filledErr = rep_pred(tab_med_filled, param, 20)
tab_mean_filledErr = rep_pred(tab_mean_filled, param, 20)


boxplot(tabErr[,1], tab_fullErr[,1], tab_gm_filledErr[,1], tab_knn_filledErr[,1],tab_med_filledErr[,1], tab_mean_filledErr[,1],  xaxt = "n", main = 'RMSE')
axis(1 , cex.axis= 1, mgp=c(1,2,0),  at=1:6, labels=c('All\n columns','Only full\n columns','GM\n imputation','KNN\n imputation','Median\n imputation', 'Mean\n imputation'), padj=0)


mean(tab_fullErr[,1])
mean(tabErr[,1])
mean(tab_gm_filledErr[,1])
mean(tab_knn_filledErr[,1])
mean(tab_med_filledErr[,1])
mean(tab_mean_filledErr[,1])