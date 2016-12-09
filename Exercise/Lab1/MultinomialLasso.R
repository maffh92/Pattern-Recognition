#Multinomial Logit with LASSO. This one already determines the best cross validation
library(glmnet)

#training set
digit.train <- digit.all.features.rawPixel[train.index,c("label",subset.selection.varnames)]
digit.train$label = as.factor(digit.train$label)

#test set
digit.test <- digit.all.features.rawPixel[-train.index,colnames(digit.train)]
digit.test$label = as.factor(digit.test$label)

set.seed(123456)
digit.lasso.cv <- cv.glmnet(as.matrix(digit.train[,-1]), digit.train$label,family="multinomial", type.measure="class")
plot(digit.lasso.cv)


# predict class label on test set using the best cv model
digit.lasso.cv.pred <- predict(digit.lasso.cv, as.matrix(digit.test[,-1]),type="class")

# make the confusion matrix
digit.lasso.cv.confmat <- table(digit.test$label,digit.lasso.cv.pred)
# digit.lasso.cv.confmat
# 0    1    2    3    4    5    6    7    8    9
# 0 3655    0   26   13   16  182   73   10   67    5
# 1    1 4416   39   44    0   23   17   12   28    1
# 2   33   65 3337  133  119   21  191   74   71   21
# 3   17   82  106 3333   14  342   64   58   74  147
# 4   15   60   55    5 3539   25   35   12   34  192
# 5   75   76   75  237   63 2762   89  114   90  115
# 6   46   18  142    6   58  108 3601    2   46    1
# 7   20  107   53   22   85   14    0 3801   14  180
# 8   23  175   78  261   74  202   47   41 2900  180
# 9   33   68    7   61  287   75    1  135   29 3401

# compute the accuracy on the test set
sum(diag(digit.lasso.cv.confmat))/sum(digit.lasso.cv.confmat)
[1] 0.847439
 