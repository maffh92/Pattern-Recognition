#This file is used to run the svm algorithm

#Linear SVM classifier
library("e1071")

#training set
digit.svn.train <- digit.noise.removed.train
digit.svn.train$label = as.factor(digit.noise.removed$label)

#test set
digit.svn.test <- digit.noise.removed.test
digit.svn.test$label = as.factor(digit.svn.test$label)


# Svm with cost complexity between 1 and 10
set.seed(123456)
digit.tune.svm <- tune.svm(digit.train[, -1],digit.train$label, cost=1:10)

# digit.tune.svm$performances
# cost error dispersion
# 1     1 0.089 0.02766867
# 2     2 0.084 0.03134042
# 3     3 0.084 0.03204164
# 4     4 0.084 0.03204164
# 5     5 0.084 0.03204164
# 6     6 0.084 0.03204164
# 7     7 0.084 0.03204164
# 8     8 0.084 0.03204164
# 9     9 0.084 0.03204164
# 10   10 0.084 0.03204164

# Cost=1
digit.svm <- svm(digit.train[, -1],digit.train$label,cost=2)

# make predictions on test set
svm.pred <- predict(digit.svm, digit.test[,-1])

svm.pred.table <- table(digit.test$label,svm.pred)
# svm.pred.table 
0    1    2    3    4    5    6    7    8    9
0 3843    0   20    2    8   84   57    2   29    2
1    0 4501   35    3    6   13    7    5    9    2
2   23   18 3725   44   57   12   56   70   51    9
3   10   48  104 3655    4  234   31   54   61   36
4    4   24   34    0 3769    2   29    5    2  103
5   12    8   21   61   34 3429   62   10   24   35
6   23    6   14    0   14   47 3916    0    7    1
7    8   46   44    8   89   13    3 3953    6  126
8   16   79   59  107   43  136   32   26 3395   88
9   23   28    4   45  206   24    2   82   24 3659

sum(diag(svm.pred.table))/sum(svm.pred.table)
# [1] 0.9230488