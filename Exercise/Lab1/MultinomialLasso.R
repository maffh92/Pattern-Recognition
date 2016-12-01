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
0    1    2    3    4    5    6    7    8    9
0 3970    0    0    5    0   22   12    0   29    0
1    0 4587    1    0    0    0    0    1    0    0
2   18    0 3445  135   44   81  156   32  164    9
3    5    0  104 3644    5  230   64   31  128   28
4    0    0    7    5 3448   14    9   33    3  452
5   23    0   64  290   30 2926  122   12  168   70
6   16    0   66   18   49   89 3710    1   91    0
7    0    0   21   22  133    3    1 3785    0  319
8   17    0   57  295    5  154   36    3 3385   22
9    0    0    8   77  188   17    1  311    1 3473
# compute the accuracy on the test set
sum(diag(digit.lasso.cv.confmat))/sum(digit.lasso.cv.confmat)
 