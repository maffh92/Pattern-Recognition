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

############## REsults: #########
# 1    2    3    4    5    6    7    8    9   10
# 1  3099  120  148   18   46  364   65   76   91   20
# 2    45 4056   40   46   37  141   29  102   77    8
# 3   126  106 2680  280   99  314  260   60   56   84
# 4    91  157  162 2785   28  447   44  187  157  179
# 5   121  165   62   30 2845   81  101   29  104  434
# 6   183   82  294  464  133 1726  149  197  335  133
# 7    59   86  375   21   32   66 3341    1   35   12
# 8    19   66   18   72   57  263    4 3251   85  461
# 9   170  261   84  263  110  483  101  188 2211  110
# 10   17   34    5   73  578  199    3  256  130 2802
# 
# [1] 0.7023415
