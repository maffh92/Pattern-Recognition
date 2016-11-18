#hPrice
hprice.dat <- read.table("Dataset/hprice.txt",header=TRUE)

# get some descriptive statistics of the data
# what percentage of the houses has a basement?
summary(hprice.dat)


# fit a linear regression model to predict "sale.price" from "lot.size" and "desire.loc"
hprice.lm1 <- lm(sale.price ~ lot.size + desire.loc,data=hprice.dat)


# give summary information about the fitted model
# the least squares estimate of the coefficient of lot.size is 5.977
# for each coefficient a test is performed whether it is significantly different from zero
# the p-value of this test is given in the last column, which is labeled "Pr(>|t|)"
# the smaller this value, the "more significant" the coefficient
# Also, the R-squared of the fitted model is returned: what percentage of variation in sales price
# is explained by variation in lot size and location?
summary(hprice.lm1)

# fit a linear regression model to predict "sale.price" from all remaining variables in the data frame (indicated by ".")
hprice.lm2 <- lm(sale.price ~ .,data=hprice.dat)
summary(hprice.lm2)


# first you need to load the MASS package (you can also do this by selecting Load Package from the Packages menu).
library(MASS)
hprice.step <- stepAIC(hprice.lm2,scope=list(lower = ~1,upper = ~.^2),k=log(546))

# give a summary of the search process
hprice.step$anova

# give summary information on the final model
summary(hprice.step)
