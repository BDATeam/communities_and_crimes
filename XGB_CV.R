#----------------------------------------------
#               PLAY AROUND
#----------------------------------------------


#   Explore data
summary(tab$OtherPerCap)

#   Define RMSE
rmse = function(pred){return(sqrt(mean((pred-tab$ViolentCrimesPerPop)^2)))}

#   Obligatory Extreme Gradient Boosting
library(xgboost)

xgb_cv = function(data,params, n_rounds){
    
    pred = as.matrix(rep(0,nrow(data)))
    #Randomly shuffle the data
    new_indexes = sample(nrow(data))
    data_shuffle<-data[new_indexes,]
    
    #Create 10 equally size folds
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

xgb_cv(data=tab, params = param, 20)
xgb_cv(data=tab_full, params = param, 20)