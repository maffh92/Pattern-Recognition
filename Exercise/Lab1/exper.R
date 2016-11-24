
#     [,1]      [,2]      [,3]      [,4]      [,5]      [,6]      [,7]      [,8]      [,9]     [,10]
#[1,] 34632.408 15188.466 29871.099 28320.188 24232.722 25835.920 27734.917 22931.244 30184.148 24553.750
#[2,]  8462.916  4409.932  7653.922  7574.975  6375.416  7527.595  7531.413  6169.042  7778.354  6466.003
# This shows that the second digit (which is "1") can probably be distinguished from the rest with good accuracy based solely on this feature:
# how much ink a digit costs. It is also probably possible to distinguish 0 from 7 using this feature. It is strange, however, that 0 costs more
# ink than 6, 8 or 9. (Check the conclusions by calcutating classification accuracy).

# As a second feature, it probably makes sense to check the amount of ink used in the center of a picture. For digit "0" this should be the smallest.

# It probably really makes sense to reduce the amount of data by quantizing.

#loading the data
digit.dat <- read.csv("dataset/mnist.csv", header = TRUE, sep = ",")
#preprocessing the data
digit.dat$label = as.factor(digit.dat$label)

#remove all zero colls
digit.dat <- digit.dat[, colSums(digit.dat != 0) > 0]

#checking stats. the result shows that a lot of pixels remain white for ALL 42000 samples. They can be clearly eliminated.
summary(digit.dat)

inkDensity <- apply(digit.dat[,-1],1,sum)
inkMeanStats <- tapply(inkDensity,digit.dat$label,mean)
inkSdStats <- tapply(inkDensity,digit.dat$label,sd)

#scale features
ScaledinkSdStats <- scale(inkSdStats)
names(ScaledinkSdStats) <- 0:9

#combine features
#assign to each label to correct ink feature
inkFeature <- 1:nrow(digit.dat)
for(i in 1:nrow(digit.dat)){
  inkFeature[i] <- ScaledinkSdStats[digit.dat[i,]$label]
}


#combine the digit with the ink feature
digit.ink <- cbind(digit.dat,inkFeature)


#Multinomial Logit
library(nnet)
train.index <- c(1:1000)

#initialise training and testData
#training set
digit.train <- digit.ink[train.index,]
digit.train$label = as.factor(digit.train$label)

#test set
digit.test <- digit.ink[-train.index,]
digit.test$label = as.factor(digit.test$label)

# fit multinomial logistic regression model
digit.multinom <- multinom(label ~ ., data = digit.train, maxit = 1000, MaxNWts = 100000000000000)

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

#Multinomial Logit with LASSO. This one already determines the best cross validation
library(glmnet)
digit.lasso.cv <- cv.glmnet(as.matrix(digit.train[,-1]), digit.train$label,family="multinomial", type.measure="class")
plot(digit.lasso.cv)

# predict class label on test set using the best cv model
digit.lasso.cv.pred <- predict(digit.lasso.cv, as.matrix(digit.test[,-1]),type="class")

# make the confusion matrix
digit.lasso.cv.confmat <- table(digit.test$label,digit.lasso.cv.pred)
digit.lasso.cv.confmat
# digit.lasso.cv.pred
# 0    1    2    3    4    5    6    7    8    9
# 0 3934    0   10    2    0   34   15    0   30    0
# 1    0 4588    0    0    0    0    0    0    0    0
# 2   29    0 3596  135   29   25  104   19   93   23
# 3    5    0  204 3390    1  285   80   61  188   47
# 4    0    0   19    9 3520   54   18   40    4  306
# 5   34    0   94  160   39 3010  125   32  142   70
# 6   31    0  135    4   29  204 3572    0   65    0
# 7    0    0   79   20  136    4    1 3772    0  284
# 8   25    0  115  159    2  237   37    7 3354   34
# 9    0    0   34   56  285   26    3  317    4 3366
# compute the accuracy on the test set
sum(diag(digit.lasso.cv.confmat))/sum(digit.lasso.cv.confmat)
#0.8805366


#k-nearest neigbhours
library(class)
#one neighbor <- still need to use cross validation on this method.
knn.pred <- knn(digit.train[,-1],digit.test[,-1],digit.train$label, k = 1)
confmat <- table( digit.test$label, knn.pred)
# use it to compute accuracy on test data
sum(diag(confmat))/sum(confmat)

#one neighbor results:
# confmat <- table( digit.test$label, knn.pred)
# confmat
# knn.pred
# 0    1    2    3    4    5    6    7    8    9
# 0 3928    3   13    6    2   18   37    2    7    9
# 1    0 4547   11   10    4    0    5    6    1    4
# 2   81  150 3502   73   26    9   23  115   51   23
# 3   19   62   85 3583    9  245   24   60   93   81
# 4    4  128    9    1 3189    0   50   78    8  503
# 5   52   35    3  130   17 3175   73   13  113   95
# 6   73   28    6    0   21   30 3858    5   14    5
# 7    6  120   42    9   46    6    1 3869    1  196
# 8   41  152   37  194   43  184   48   33 3100  138
# 9   17   42   22   30  315   33    3  238   16 3375
# > sum(diag(confmat))/sum(confmat)
# [1] 0.881122

#Linear SVM classifier
library("e1071")

#training set
digit.train <- digit.dat[train.index,]
digit.train <- digit.train[, colSums(digit.train != 0) > 0]
digit.train$label = as.factor(digit.train$label)

#test set
digit.test <- digit.dat[-train.index,]
digit.test$label = as.factor(digit.test$label)



set.seed(10111)
# Cost=1
digit.svm <- svm(digit.train[, -1],digit.train$label)

# make predictions on test set
svm.pred <- predict(digit.svm, digit.test[, -1])

table(digit.test$label,svm.pred)
sum(diag(table(digit.test$label,svm.pred)))/nrow(digit.test)

#With cross validation
# tune cost parameter with cross-validation
digit.svm.tune <- tune.svm(digit.train[, -c(1, 40,65)],
                                 digit.train[,65],cost=1:10)

# V57 is almost always zero, let's remove it
digit.svm.tune <- tune.svm(digit.train[, -c(1, 40,57,65)],
                                 digit.train$label,cost=1:10)

# show performance for each value of the cost parameter and choose lowest
digit.svm.tune$performances


# We fit the SVM with this parameter value on the whole training set:
digit.svm.tuned <- svm(digit.train[, -c(1, 40,57,65)],
                             digit.train$label,cost=5)

# Generate predictions on the test set
svm.tuned.pred <- predict(digit.svm.tuned,
                            digit.test[, -c(1, 40,57,65)])

sum(diag(table(digit.test$label,svm.tuned.pred)))/nrow(digit.test)
