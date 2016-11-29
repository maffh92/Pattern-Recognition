#k-nearest neigbhours
library(class)
#training set

#training set
digit.train <- digit.all.features[train.index,]
digit.train$label = as.factor(digit.train$label)

#test set
digit.test <- digit.all.features[-train.index,colnames(digit.train)]
digit.test$label = as.factor(digit.test$label)

#run 10 fold  and determine the k with the best accuracy
neighboursInformation <- run.analyse(digit.dat[train.index,],10)

#one neighbor <- still need to use cross validation on this method.
set.seed(123456)
knn.pred <- knn(digit.train[,-1],digit.test[,-1],digit.train$label, k = 1)
knn.table <- table( digit.test$label, knn.pred)
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