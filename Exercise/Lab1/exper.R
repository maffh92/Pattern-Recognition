
#     [,1]      [,2]      [,3]      [,4]      [,5]      [,6]      [,7]      [,8]      [,9]     [,10]
#[1,] 34632.408 15188.466 29871.099 28320.188 24232.722 25835.920 27734.917 22931.244 30184.148 24553.750
#[2,]  8462.916  4409.932  7653.922  7574.975  6375.416  7527.595  7531.413  6169.042  7778.354  6466.003
# This shows that the second digit (which is "1") can probably be distinguished from the rest with good accuracy based solely on this feature:
# how much ink a digit costs. It is also probably possible to distinguish 0 from 7 using this feature. It is strange, however, that 0 costs more
# ink than 6, 8 or 9. (Check the conclusions by calcutating classification accuracy).

# As a second feature, it probably makes sense to check the amount of ink used in the center of a picture. For digit "0" this should be the smallest.

# It probably really makes sense to reduce the amount of data by quantizing.

b <- c(8,12,16,20)
for(i in b){
  h <- i
}

horizontalLines <- c(8,12,14,16,20)
verticalLines <- c(10,12,14,16,20)

horizontalFeatures <- generalLineFeatures(horizontalLines,digit.dat,horizontalFrame)
verticalFeatures <- generalLineFeatures(verticalLines,digit.dat,verticalFrame)

horizontalFrame <- data.frame("horizontal-8"=1:42000,"horizontal-12"=1:42000,"horizontal-14"=1:42000,"horizontal-16"=1:42000,"horizontal-20"=1:42000)
verticalFrame <- data.frame("vertical-10"=1:42000,"horizontal-12"=1:42000,"horizontal-14"=1:42000,"horizontal-16"=1:42000,"horizontal-20"=1:42000)

generalLineFeatures <- function(lines,data,featureFrame){
  i <- 0
  for(elem in lines){
    i <- i + 1
    featureFrame[,i] <- horizontal_line(elem,data)
  }
  return(featureFrame)
}



horizontal_line <- function (b,data){
  line = 1:42000
    for(i in 1:42000){
      line[i] = sum(data[i, ((b-1)*28+2) : ((b-1)*28 + 29)]);
    }
  line
} # to use the function, type digit.dat = cbind(digit.dat, horizontal_line(b, digit$dat)

vertical_line <- function (a, data){
  line = 1:42000
  for(i in 1:42000){
    sumv = 0;
    for(j in 0:27){
      sumv = sumv + data[i, a + 28*j + 1];
    }
    line[i] = sumv;
  }
  line
} # to use the function, type digit.dat = cbind(digit.dat, vertical_line(a, digit$dat)

vecticalFeature <- vertical_line(8,digit.dat)
horizontalFeature <- horizontal_line(8,digit.dat)

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
