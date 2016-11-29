#Multinomial Logit
library(nnet)
digit.ink <- cbind(digit.dat,inkFeature)
digit.ink.horizontal <- cbind(digit.ink,horizontalFeature)
#training set
digit.train <- digit.ink[train.index,]
digit.train$label = as.factor(digit.train$label)

#test set
digit.test <- digit.ink[-train.index,colnames(digit.train)]
digit.test$label = as.factor(digit.test$label)


#initialise training and testData
# fit multinomial logistic regression model
set.seed(123456)
digit.multinom <- multinom(label ~ ., data = digit.train, maxit = 100000, MaxNWts = 100000000000000)

# predict class label on training data
digit.multinom.pred <- predict(digit.multinom, digit.test[,-1],type="class")
# make confusion matrix: true label vs. predicted label
table(digit.train$label,digit.multinom.pred)

# predict class label on test data
digit.multinom.single.test.pred <- predict(digit.multinom, digit.test[,-1],type="class")
table(digit.test$label,digit.multinom.single.test.pred)

# make confusion matrix for predictions on test data
multimodal.confmat.single <- table(digit.test$label,digit.multinom.single.test.pred)

# use it to compute accuracy on test data
sum(diag(multimodal.confmat.single))/sum(multimodal.confmat.single)


#2 features
#training set
digit.train.horizontal <- digit.ink.horizontal[train.index,]
digit.train.horizontal$label = as.factor(digit.train.horizontal$label)

#test set
digit.test.horizontal <- digit.ink.horizontal[-train.index,colnames(digit.train.horizontal)]
digit.test.horizontal$label = as.factor(digit.test.horizontal$label)


# predict class label on training data
set.seed(123456)
digit.multinom.horizontal <- multinom(label ~ ., data = digit.train.horizontal, maxit = 100000, MaxNWts = 100000000000000)

digit.multinom.pred.horizontal <- predict(digit.multinom.horizontal, digit.test.horizontal[,-1],type="class")
# make confusion matrix: true label vs. predicted label
table(digit.test.horizontal$label,digit.multinom.pred.horizontal)

# make confusion matrix for predictions on test data
multimodal.confmat.horizontal <- table(digit.test.horizontal$label,digit.multinom.pred.horizontal)

# use it to compute accuracy on test data
sum(diag(multimodal.confmat.horizontal))/sum(multimodal.confmat.horizontal)