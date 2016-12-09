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
digit.svm <- svm(digit.train[, -1],digit.train$label,cost=3)

# make predictions on test set
svm.pred <- predict(digit.svm, digit.test[,-1])

svm.pred.table <- table(digit.test$label,svm.pred)
# svm.pred.table 
# 0    1    2    3    4    5    6    7    8    9
# 0 3848    0   19    2    7   82   57    2   28    2
# 1    0 4501   38    2    6   11    7    6    8    2
# 2   23   13 3729   44   51   13   58   72   53    9
# 3   10   50  105 3659    3  229   29   53   62   37
# 4    4   25   34    0 3774    2   30    5    3   95
# 5   13    7   22   63   33 3425   66   10   23   34
# 6   23    6   13    0   14   44 3920    0    7    1
# 7    8   44   44    7   81   14    2 3969    6  121
# 8   16   74   61  116   40  130   33   28 3393   90
# 9   23   25    4   37  214   24    2   94   21 3653

sum(diag(svm.pred.table))/sum(svm.pred.table)
# [1] 0.9236829