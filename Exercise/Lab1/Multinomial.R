#Multinomial Logit
library(nnet)
digit.ink.horizontal <- cbind(digit.ink,horizontalFeatures)
colnames(digit.ink)[1] <- "label"
colnames(digit.ink)[2] <- "inkFeature"

#training set
digit.train <- as.data.frame(digit.ink[train.index,])
digit.train$label = as.factor(digit.train$label)

#test set
digit.test <- as.data.frame(digit.ink[-train.index,])
digit.test$label = as.factor(digit.test$label)

# all pixel features + ink feature (all scaled)
#initialise training and testData
# fit multinomial logistic regression model
set.seed(123456)
digit.multinom <- multinom(label ~ ., data = digit.train, maxit = 10000000, MaxNWts = 100000000000000)

# predict class label on training data
digit.multinom.pred <- predict(digit.multinom, digit.test,type="class")
# make confusion matrix: true label vs. predicted label
multimodal.confmat = table(digit.test$label,digit.multinom.pred)
table(digit.test$label,digit.multinom.pred)
sum(diag(multimodal.confmat))/sum(multimodal.confmat)

#1    2    3    4    5    6    7    8    9   10
#1  1738   71  992  773   35    0   83  355    0    0
#2     6 3672   10   97   20    0   22  754    0    0
#3   916  249  920  982   67    0  119  812    0    0
#4   747  376  850  991   79    0  132 1062    0    0
#5   212  748  443  871   80    0  170 1448    0    0
#6   403  616  537  832   72    0  121 1115    0    0
#7   589  412  783  949   85    0  157 1053    0    0
#8   140 1085  363  808   97    0  166 1637    0    0
#9   899  164  898 1013   75    0  124  808    0    0
#10  236  682  469  867  117    0  157 1569    0    0
# 0.2242683

######################################

#2 features
#training set
digit.train.all.features <- digit.all.features[train.index,]
digit.train.all.features$label = as.factor(digit.train.all.features$label)

#test set
digit.test.all.features <- digit.all.features[-train.index,colnames(digit.train.all.features)]
digit.test.all.features$label = as.factor(digit.test.all.features$label)

# ink_density + all line features (scaled)
# predict class label on training data
set.seed(123456)
digit.multinom.all.features <- multinom(label ~ ., data = digit.train.all.features, maxit = 100000, MaxNWts = 100000000000000)

digit.multinom.pred.all.features <- predict(digit.multinom.all.features, digit.test.all.features[,-1],type="class")
# make confusion matrix: true label vs. predicted label
table(digit.test.all.features$label,digit.multinom.pred.all.features)

# make confusion matrix for predictions on test data
multimodal.confmat.all.features <- table(digit.test.all.features$label,digit.multinom.pred.all.features)

# use it to compute accuracy on test data
sum(diag(multimodal.confmat.all.features))/sum(multimodal.confmat.all.features)