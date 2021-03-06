#----------------------------------------------
#                   SETUP
#----------------------------------------------

setwd("~/MSc DSBA/Big Data Analytics/Projects/Final Project/Data")

#   Load data and column names
library(readr)
communities <- read_csv("communities.data", col_names = FALSE, na = "?")
colnames(communities) <- as.vector(read_csv("colnames.csv", col_names = FALSE))

#   Split non predictive columns
non_pred_var <- c(1:5)
non_pred <- communities[,non_pred_var]
tab <- communities[,-non_pred_var]

# Split Test / Train sets
test_size = 0.2

random_indexes = sample(nrow(tab))
test_indexes = random_indexes[1:floor(test_size*nrow(tab))]

tab_test = tab[test_indexes,]
tab_train = tab[-test_indexes,]
