#----------------------------------------------
#              FEATURE SELECTION
#----------------------------------------------


# Environment
library(caret)
library(plyr)
library(gridExtra)
library(xtable)
options(xtable.floating = FALSE)
options(xtable.timestamp = "")



# data without missing values
data = tab[,colSums(is.na(tab))==0]
x = as.matrix(data[,1:(ncol(data)-1)])
y = as.matrix(data[,ncol(data)])
colnames(y) = colnames(data)[ncol(data)]


# only complete observables 
data = na.omit(tab)
x = as.matrix(data[,1:(ncol(data)-1)])
y = as.matrix(data[,ncol(data)])
colnames(y) = colnames(data)[ncol(data)]


feature_selection_data = function(data, number_of_features = 40, chosen_method = "lm"){
  
  # FUNCTION THAT RETURNS DATA AFTER A FEATURE SELECTION
  
  # data = input data (x and y together)
  # number of features : nbr features to keep -> 40 is as good as all of the 100 features, but you can try lower
  # chosen method : "lm", "rf", any regression method you want to apply
  
  control <- trainControl(method="repeatedcv", number=10, repeats=3)
  model <- train(ViolentCrimesPerPop~., data = data, method = chosen_method, preProcess="scale", trControl=control)
  importance <- varImp(model, scale=FALSE)

  imp = importance$importance
  colnames(imp) = "ImportanceValue"
  imp$ColumnName = rownames(imp)
  rownames(imp) = c()
  imp_ordered = arrange(imp,desc(ImportanceValue))
  imp_ordered$ImportanceRank = as.numeric(rownames(imp_ordered))
  
  selected_features = imp_ordered$ColumnName[1:number_of_features]
  
  return(data[,c(selected_features,"ViolentCrimesPerPop")])
}










# REMOVE REDUNDANT FEATURES

# calculate correlation matrix
correlationMatrix <- cor(x)
# summarize the correlation matrix
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
# print indexes of highly correlated attributes
print(highlyCorrelated)







# RANK FEATURES BY IMPORTANCE
method_to_train = "lm"

# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=3)
# choose and train the model
model <- train(ViolentCrimesPerPop~., data = data, method = method_to_train, preProcess="scale", trControl=control)
# estimate variable importance
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)

# get importance of uncomplete variables
imp = importance$importance
colnames(imp) = "ImportanceValue"
imp$ColumnName = rownames(imp)
rownames(imp) = c()
imp$ColumnNumber = as.numeric(rownames(imp))
imp_ordered = arrange(imp,desc(ImportanceValue))
imp_ordered$ImportanceRank = as.numeric(rownames(imp_ordered))
imp_final = imp_ordered[,c(2,4,1)]
imp_sparse = imp_final[imp_final$ColumnNumber %in% sparse_idx,]
print(imp_sparse)

# printing results
print(xtable(imp_sparse, digits=c(0,0,0,0,2)), include.rownames = FALSE)
print(xtable(imp_final, digits=c(0,0,0,2)), include.rownames = FALSE)














# FEATURE SELECTION - RECURSIVE FEATURE ELIMINATION (BACKWARD)
max_number_of_features = 5


# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
results <- rfe(x, y, sizes = 1:max_number_of_features,rfeControl=control)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))


