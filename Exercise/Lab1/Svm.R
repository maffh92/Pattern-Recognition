#This file is used to run the svm algorithm

#Linear SVM classifier
library("e1071")

#training set
digit.train <- digit.noise.removed[train.index,]
digit.train$label = as.factor(digit.noise.removed$label)

#test set
digit.test <- digit.noise.removed[-train.index,colnames(digit.train)]
digit.test$label = as.factor(digit.test$label)


# Svm with cost complexity between 1 and 10
set.seed(123456)
digit.svm <- tune.svm(scale(digit.train[, -1]),digit.train$label, kernel="linear",cost=1:10)

# make predictions on test set
svm.pred <- predict(digit.svm, digit.test[, -1])

table(digit.test$label,svm.pred)
sum(diag(table(digit.test$label,svm.pred)))/nrow(digit.test)

# Cost=1
digit.svm <- svm(digit.train[, -1],digit.train$label,cost=5)

# make predictions on test set
svm.pred <- predict(digit.svm, digit.test[, -1])

table(digit.test$label,svm.pred)
sum(diag(table(digit.test$label,svm.pred)))/nrow(digit.test)
