#Multinomial Logit with LASSO. This one already determines the best cross validation
library(glmnet)

#training set
digit.train <- digit.all.features[train.index,]
digit.train$label = as.factor(digit.train$label)

#test set
digit.test <- digit.all.features[-train.index,colnames(digit.train)]
digit.test$label = as.factor(digit.test$label)

set.seed(123456)
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
#0.8872927