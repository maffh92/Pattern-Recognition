#k-nearest neigbhours
library(class)
#training set

#training set
digit.train <- digit.all.features[train.index,]
digit.train$label = as.factor(digit.train$label)

#test set
digit.test <- digit.all.features[-train.index,colnames(digit.train)]
digit.test$label = as.factor(digit.test$label)

#training set
digit.train <- digit.noise.removed.train
digit.train$label = as.factor(digit.noise.removed.train$label)

#test set
digit.test <- digit.noise.removed.test
digit.test$label = as.factor(digit.test$label)


#run 10 fold  and determine the k with the best accuracy
neighboursInformation <- run.analyse(digit.train,100)
neighboursInformation[max(neighboursInformation$accuracy) == neighboursInformation$accuracy,]
#one neighbor <- still need to use cross validation on this method.
set.seed(123456)
knn.pred <- knn(digit.train[,-1],digit.test[,-1],digit.train$label, k = 3)
knn.table <- table( digit.test$label, knn.pred)
# knn.pred
# 0    1    2    3    4    5    6    7    8    9
# 0 3865    4    9    3    6   47   72   17    6   18
# 1    0 4547   12    4    3    0    4    5    1    5
# 2   43  156 3424   52   32   22   52  188   41   55
# 3   14   95   48 3688    4  185   24   80   41   58
# 4    3   99    7    2 3331    4   68   52    0  406
# 5   18   46    6  150   39 3185   91   41   14  106
# 6   43   24    2    1   11   48 3888    8    0    3
# 7    3  185   22    3   51    3    2 3942    0   85
# 8   33  217   39  298   46  192   56   68 2863  169
# 9   19   55    2   46  179   26   13  265    9 3483
# use it to compute accuracy on test data
sum(diag(knn.table))/sum(knn.table)
# [1] 0.8833171


#run.analyse is used as the main function to run the analyse
#classifyInformation is used to store the parameters with the corresponding number of errors.
run.analyse <- function(data,k){
  #get a random of size 200, first attribute of the data variable is the nr
  classifyInformation = data.frame(k = c(), accuracy = c())
  for(i in 1 : k){ # nmin and minleaf may have different range
      value <- ten.fold.split(data,i) # total error for 10-fold validation
      classifyInformation = rbind(classifyInformation, c(i, value))
  }
  
  names(classifyInformation) <- c("k","accuracy")
  
 return(classifyInformation)
}






#ten.fold.split first divides the groups in equal divided groups of 20. The data is already random, so we do not make it random here.
# Then it will go over all of these groups and classifies them to the other 180 elements and sum the total errors.
ten.fold.split <- function(data,k){ # returns total error for 10-fold validation
  rowsToDivide <- 1 : nrow(data)
  
  #split the data set into equal divided groups
  equalDividedGroups <- split(rowsToDivide, ceiling(seq_along(rowsToDivide)/100)) #
  confmat <- data.frame("0"=rep(0,10),"1"=rep(0,10),"2"=rep(0,10),"3"=rep(0,10),"4"=rep(0,10),"5"=rep(0,10),"6"=rep(0,10),"7"=rep(0,10),"8"=rep(0,10),"9"=rep(0,10))
  colnames(confmat) <- 0:9

  for(i in 1:NROW(equalDividedGroups)){
    #split the training set and testset
    trainingSet <- data[listToVectorExcept(equalDividedGroups,i),]
    testSet <- data[equalDividedGroups[[i]],]
    set.seed(123456)
    knn.pred <- knn(trainingSet[,-1],testSet[,-1],trainingSet[,1], k = k)
    confmat <- confmat + table(testSet[,1], knn.pred)
    # use it to compute accuracy on test data
  }
  #need to convert it to numeric, for some reason it is not numeric anymore
  confmat <- apply(as.data.frame(confmat), c(1,2) ,as.numeric)
  return(sum(diag(confmat))/sum(confmat))
}



# listToVectorExpr is used to exclude one group of a list.
# The data parameter is a list of 10 groups and except is the number of the group to be excluded from tr. set
listToVectorExcept <- function(data, except){ 
  value <- c()
  for(i in 1:NROW(data)){
    if(i != except){
      value <- append(value, data[[i]])
    }
  }
  return(value)
}