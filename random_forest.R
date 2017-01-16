# Environment
library(randomForest)


# take out NA values
tab_ = tab[,colSums(is.na(tab))==0]

# Split Test / Train sets
test_size = 0.2

random_indexes = sample(nrow(tab_))
test_indexes = random_indexes[1:floor(test_size*nrow(tab_))]
tabTest = tab_[test_indexes,]
tabTrain = tab_[-test_indexes,]



# CREATE X and Y TRAIN
x_train = as.matrix(tabTrain[,1:(ncol(tabTrain)-1)])
y_train = as.matrix(tabTrain[,ncol(tabTrain)])
colnames(y_train) = colnames(tabTrain)[ncol(tabTrain)]
# CREATE X and Y TEST
x_test = as.matrix(tabTest[,1:(ncol(tabTest)-1)])
y_test = as.matrix(tabTest[,ncol(tabTest)])
colnames(y_test) = colnames(tabTest)[ncol(tabTest)]


#CROSS VALIDATION

random_forest_cv = function(x,y,ntree){
  
  data = cbind(x,y)
  pred = as.matrix(rep(0,length(y)))
  
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
    
    x_train = scale(trainData[,1:(ncol(trainData)-1)])
    s = attr(x_train, 'scaled:scale')
    m = attr(x_train, 'scaled:center')
    
    y_train = as.matrix(trainData[,ncol(trainData)])
    colnames(y_train) = colnames(y)
    
    x_test = scale(testData[,1:(ncol(testData)-1)], center = m, scale = s)
    
    y_test = as.matrix(testData[,ncol(testData)])
    colnames(y_test) = colnames(y)
    
    
    #regression
    fit <- randomForest(x_train, y_train, ntree=ntree)
    predictions = predict(fit, x_test)
    pred[real_testIndexes] = predictions
  }
  return(pred)
}


# Quick Test
random_forest_cv(x_train,y_train,5)


# Test
x_train_sc = scale(x_train)
s = attr(x_train_sc, 'scaled:scale')
m = attr(x_train_sc, 'scaled:center')
x_test_sc = scale(x_test, center = m, scale = s)
fit <- randomForest(x_train, y_train, ntree=1000)
predictions = predict(fit, x_test)
residuals = predictions - y_test
rmse <- sqrt(mean((residuals)^2))
print(rmse)


# Random Forest -------------


rmse_vector=numeric()
ntree_vector=c(5,10,25,50,75,100,150,200,300,500,700,1000)
#ntree_vector=c(5,10,25,50,75,100,120,150,175,200,250,300,350,400,450,500,550,600,650,700,750,800,850,900,950,1000)

for(ntree in ntree_vector){
  prediction = random_forest_cv(x_train,y_train,ntree)
  residuals = prediction - y_train
  rmse <- sqrt(mean((residuals)^2))
  print(rmse)
  rmse_vector = c(rmse_vector,rmse)
}
index_min = which(rmse_vector==min(rmse_vector),arr.ind=TRUE)
rmse_min = rmse_vector[index_min]
ntree_min = ntree_vector[index_min]
sprintf("Min RMSE of %.4f for ntree = %.0f", rmse_min,ntree_min)

plot(ntree_vector,rmse_vector,xlab="Number of Trees",ylab = "RMSE",main="Random Forest RMSE" )
lines(ntree_vector,rmse_vector, type= "l") 
