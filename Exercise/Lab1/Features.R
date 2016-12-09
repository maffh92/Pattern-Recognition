#All (?) the features
inkDensity <- apply(digit.dat[,-1],1,sum)
inkMeanStats <- tapply(inkDensity,digit.dat$label,mean) # just to look at the new feature
inkSdStats <- tapply(inkDensity,digit.dat$label,sd) # just to look at the new feature
inkDensityMean <- apply(digit.dat[,-1],1,mean)

inkFeature = scale(inkDensityMean)
# digit.ink <- cbind(digit.dat,inkFeature)
digit.ink <- as.data.frame(cbind(digit.dat$label,inkFeature))

#combine all the features
# digit.all.features <- cbind(digit.dat,inkFeature,scaledHorizontalFeatures,scaledVerticalFeatures)

#remove all the columns below some fixed size of zero values
# threshold <- 100
# x <- digit.dat[train.index,-1]
# tmp <- apply(x,2,function(a){if(length(a[a!=0]) > threshold) a}) # what is this?
#if(class(tmp) == "list"){
#  print("remove all null cases from the list")
#  tmp <- tmp[!sapply(tmp, is.null)]
#}
#tmp <- as.data.frame(tmp)

#digit.noise.removed.train <- cbind(digit.dat[train.index,1],tmp,inkFeature[train.index],verticalFeatures[train.index,],horizontalFeatures[train.index,])
#digit.noise.removed.test <- cbind(digit.dat[-train.index,1],digit.dat[-train.index,colnames(tmp)],inkFeature[-train.index],verticalFeatures[-train.index,],horizontalFeatures[-train.index,])

#colnames(digit.noise.removed.train)[1] <- "label"
#colnames(digit.noise.removed.test) <- colnames(digit.noise.removed.train)

###############
###line features
###############


horizontalLines <- c(8,12,14,16,20)
verticalLines <- c(10,12,14,16,20)

horizontalFrame <- data.frame("horizontal-8"=1:42000,"horizontal-12"=1:42000,"horizontal-14"=1:42000,"horizontal-16"=1:42000,"horizontal-20"=1:42000)
verticalFrame <- data.frame("vertical-10"=1:42000,"vertical-12"=1:42000,"vertical-14"=1:42000,"vertical-16"=1:42000,"vertical-20"=1:42000)

# TAKES TIME
horizontalFeatures <- generalLineFeatures(horizontalLines,digit.dat,horizontalFrame)
verticalFeatures <- generalLineFeatures(verticalLines,digit.dat,verticalFrame)
scaledHorizontalFeatures <- scale(horizontalFeatures)
scaledVerticalFeatures <- scale(verticalFeatures)

# line feature stats
h8MeanStats = tapply(digit.all.features$horizontal.8,digit.dat$label,mean)
h12MeanStats = tapply(digit.all.features$horizontal.12,digit.dat$label,mean)
h14MeanStats = tapply(digit.all.features$horizontal.14,digit.dat$label,mean)
h16MeanStats = tapply(digit.all.features$horizontal.16,digit.dat$label,mean)
h20MeanStats = tapply(digit.all.features$horizontal.20,digit.dat$label,mean)

v10MeanStats = tapply(digit.all.features$vertical.10,digit.dat$label,mean)
v12MeanStats = tapply(digit.all.features$vertical.12,digit.dat$label,mean)
v14MeanStats = tapply(digit.all.features$vertical.14,digit.dat$label,mean)
v16MeanStats = tapply(digit.all.features$vertical.16,digit.dat$label,mean)
v20MeanStats = tapply(digit.all.features$vertical.20,digit.dat$label,mean)


#combine all the new features
digit.all.features = as.data.frame(cbind(digit.dat$label, inkFeature, scaledHorizontalFeatures, scaledVerticalFeatures))
colnames(digit.all.features)[1] <- "label"
colnames(digit.all.features)[2] <- "inkFeature"

#######################################################
# line feature stats:
# > h8MeanStats
# 0           1           2           3           4           5           6           7           8           9 
# 0.72785863 -0.81598241  0.20224507  0.47862695 -0.58172859  0.38962264 -0.74985953 -0.13593079  0.54338027  0.06449222 
# > h12MeanStats
# 0           1           2           3           4           5           6           7           8           9 
# 0.72839992 -0.72644183 -0.68353264 -0.07657331  0.16561755 -0.20099501 -0.30899468  0.37983170  0.47223190  0.32415621 
# > h14MeanStats
# 0           1           2           3           4           5           6           7           8           9 
# 0.21488928 -0.88135051 -0.67994264  0.30057793  0.46811025  0.06547935  0.46765014 -0.58862839  0.30675524  0.48412807 
# > h16MeanStats
# 0           1           2           3           4           5           6           7           8           9 
# 0.03932602 -0.85655591  0.03553227 -0.27673997  1.15893522 -0.36016381  0.70643752 -0.64258783 -0.06175186  0.40814764 
# > h20MeanStats
# 0           1           2           3           4           5           6           7           8           9 
# 0.74798181 -0.60320178  1.27166761 -0.20580518 -0.62766150 -0.20758612  1.01221323 -0.59612239 -0.02290225 -0.67069573 
# > v10MeanStats
# 0           1           2           3           4           5           6           7           8           9 
# 0.73667003 -0.75065102 -0.39290543 -0.35177719 -0.19777363 -0.05596683 -0.71694733  1.32572200  0.18684360  0.24688842 
# > v12MeanStats
# 0           1           2           3           4           5           6           7           8           9 
# 0.72839992 -0.72644183 -0.68353264 -0.07657331  0.16561755 -0.20099501 -0.30899468  0.37983170  0.47223190  0.32415621 
# > v14MeanStats
# 0           1           2           3           4           5           6           7           8           9 
# 0.21488928 -0.88135051 -0.67994264  0.30057793  0.46811025  0.06547935  0.46765014 -0.58862839  0.30675524  0.48412807 
# > v16MeanStats
# 0           1           2           3           4           5           6           7           8           9 
# 0.03932602 -0.85655591  0.03553227 -0.27673997  1.15893522 -0.36016381  0.70643752 -0.64258783 -0.06175186  0.40814764 
# > v20MeanStats
# 0           1           2           3           4           5           6           7           8           9 
# 0.74798181 -0.60320178  1.27166761 -0.20580518 -0.62766150 -0.20758612  1.01221323 -0.59612239 -0.02290225 -0.67069573
####################################################

generalLineFeatures <- function(lines,data,featureFrame){
  i <- 0
  for(elem in lines){
    i <- i + 1
    featureFrame[,i] <- horizontal_line(elem,data)
  }
  return(featureFrame)
}

horizontal_line <- function (b,data){
  line = 1:42000
  for(i in 1:42000){
    line[i] = sum(data[i, ((b-1)*28+2) : ((b-1)*28 + 29)]);
  }
  line
} # to use the function, type digit.dat = cbind(digit.dat, horizontal_line(b, digit$dat)

vertical_line <- function (a, data){
  line = 1:42000
  for(i in 1:42000){
    sumv = 0;
    for(j in 0:27){
      sumv = sumv + data[i, a + 28*j + 1];
    }
    line[i] = sumv;
  }
  line
} # to use the function, type digit.dat = cbind(digit.dat, vertical_line(a, digit$dat)