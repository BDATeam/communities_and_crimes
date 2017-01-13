#----------------------------------------------
#              FEATURE SELECTION
#----------------------------------------------


# Environment
library(caret)


# data without missing values
data = tab[,colSums(is.na(tab))==0]
x = as.matrix(data[,1:(ncol(data)-1)])
y = as.matrix(data[,ncol(data)])
colnames(y) = colnames(data)[ncol(data)]







# REMOVE REDUNDANT FEATURES

# calculate correlation matrix
correlationMatrix <- cor(X)
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




# FEATURE SELECTION - RECURSIVE FEATURE ELIMINATION (BACKWARD)
max_number_of_features = 5


# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
results <- rfe(x, y, sizes = c(1:max_number_of_features),rfeControl=control)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))


