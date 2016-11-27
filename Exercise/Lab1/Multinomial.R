#Multinomial Logit
library(nnet)
digit.ink <- cbind(digit.dat,inkFeature)
#training set
digit.train <- digit.ink[train.index,]
digit.train$label = as.factor(digit.train$label)

#test set
digit.test <- digit.ink[-train.index,colnames(digit.train)]
digit.test$label = as.factor(digit.test$label)

#initialise training and testData
# fit multinomial logistic regression model
digit.multinom <- multinom(label ~ ., data = digit.train, maxit = 100000, MaxNWts = 100000000000000)

# predict class label on training data
digit.multinom.pred <- predict(digit.multinom, digit.train[,-1],type="class")
# make confusion matrix: true label vs. predicted label
table(digit.train$label,digit.multinom.pred)

# predict class label on test data
digit.multinom.test.pred <- predict(digit.multinom, digit.test[,-1],type="class")
table(digit.test$label,digit.multinom.test.pred)

# make confusion matrix for predictions on test data
confmat <- table(digit.test$label,digit.multinom.test.pred)

# use it to compute accuracy on test data
sum(diag(confmat))/sum(confmat)