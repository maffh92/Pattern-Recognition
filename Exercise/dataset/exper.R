#loading the data
digit.dat = read.csv("mnist.csv", header = TRUE, sep = ",")
#preprocessing the data
digit.dat$label = as.factor(digit.dat$label)
#checking stats. the result shows that a lot of pixels remain white for ALL 42000 samples. They can be clearly eliminated.
summary(digit.dat)
d.ink = c(1:42000)
for(i in 1:42000){
  d.ink[i] = sum(digit.dat[i,-1])
}
digit.dat = cbind(digit.dat, d.ink)
ink_stats = c(0:19)
for(i in 0:9){
  ink_stats[i+1] = mean(digit.dat[digit.dat$label == i, 786])
  ink_stats[i+11] = sd(digit.dat[digit.dat$label == i, 786])
}
ink_stats = matrix(ink_stats, nrow = 2, ncol = 10, byrow = TRUE)

#[,1]      [,2]      [,3]      [,4]      [,5]      [,6]      [,7]      [,8]      [,9]     [,10]
#[1,] 34632.408 15188.466 29871.099 28320.188 24232.722 25835.920 27734.917 22931.244 30184.148 24553.750
#[2,]  8462.916  4409.932  7653.922  7574.975  6375.416  7527.595  7531.413  6169.042  7778.354  6466.003
# This shows that the second digit (which is "1") can probably be distinguished from the rest with good accuracy based solely on this feature:
# how much ink a digit costs. It is also probably possible to distinguish 0 from 7 using this feature. It is strange, however, that 0 costs more
# ink than 6, 8 or 9. (Check the conclusions by calcutating classification accuracy).

# As a second feature, it probably makes sense to check the amount of ink used in the center of a picture. For digit "0" this should be the smallest.

# It probably really makes sense to reduce the amount of data by quantizing.