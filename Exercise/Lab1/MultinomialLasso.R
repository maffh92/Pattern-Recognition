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
digit.lasso.cv.confmat
digit.lasso.cv.pred
0    1    2    3    4    5    6    7    8    9
0 3628    0   18   18   16  220   71   15   55    6
1    1 4408   38   43    0   29   17   15   29    1
2   29   58 3328  123  111   19  190   97   81   29
3   15   67  104 3361   16  359   57   51   72  135
4   17   58   51    7 3542   18   43   11   33  192
5   75   70   71  219   59 2817   95  101   88  101
6   40   16  148    6   51   99 3622    7   36    3
7   20   94   65   22   80   15    1 3811   14  174
8   19  166   72  238   76  212   54   35 2926  183
9   26   64   12   61  279   78    2  136   26 3413
# compute the accuracy on the test set
sum(diag(digit.lasso.cv.confmat))/sum(digit.lasso.cv.confmat)
 