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
digit.tune.svm <- tune.svm(digit.svn.train[, -1],digit.svn.train$label, cost=1:10)

# cost error dispersion
# 1     1 0.083 0.02983287
# 2     2 0.081 0.02923088
# 3     3 0.081 0.02806738
# 4     4 0.080 0.02867442
# 5     5 0.080 0.02867442
# 6     6 0.080 0.02867442
# 7     7 0.080 0.02867442
# 8     8 0.080 0.02867442
# 9     9 0.080 0.02867442
# 10   10 0.080 0.02867442

# Cost=1
digit.svm <- svm(digit.svn.train[, -1],digit.svn.train$label,cost=4)

# make predictions on test set
svm.pred <- predict(digit.svm, digit.svn.test[,-1])

svm.pred.table <- table(digit.svn.test$label,svm.pred)
svm.pred
#      0    1    2    3    4    5    6    7    8    9
# 0 3879    0    7   23    6   30   59    3   28    3
# 1    0 4509   13   10   12   16    7   10    4    8
# 2   43   10 3710   49   79   14   46   56   69    8
# 3   18   17   69 3867   10  102   17   38   75   26
# 4    5    7   11    2 3694    1   20   26    3  202
# 5   40   11    9  221   36 3229   44    9   64   42
# 6   40   10    7    5   33   59 3862    0   24    0
# 7    9   22   28    4   52    1    3 4038    5  122
# 8   32   35   15  180   35   93   22   17 3513   32
# 9   33    9    4   70  131   10   10  136   19 3654
sum(diag(svm.pred.table))/nrow(digit.svn.test)
# 0.9257317