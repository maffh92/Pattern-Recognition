set.seed(123456)
train.index <- sample(c(1:nrow(digit.dat)),1000)

#loading the data
digit.dat <- read.csv("dataset/mnist.csv", header = TRUE, sep = ",")
digit.dat$label = as.factor(digit.dat$label)

#remove all zero colls
digit.dat <- digit.dat[, colSums(digit.dat != 0) > 0]


