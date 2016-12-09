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


horizontalLines <- c(3:26) # c(8,12,14,16,20)
verticalLines <- c(3:26) # c(10,12,14,16,20)

horizontalFrame <- data.frame("horizontal-3"=1:42000,"horizontal-4"=1:42000,"horizontal-5"=1:42000,"horizontal-6"=1:42000,"horizontal-7"=1:42000,
                              "horizontal-8"=1:42000,"horizontal-9"=1:42000,"horizontal-10"=1:42000,"horizontal-11"=1:42000,
                              "horizontal-12"=1:42000,"horizontal-13"=1:42000,"horizontal-14"=1:42000,"horizontal-15"=1:42000,
                              "horizontal-16"=1:42000,"horizontal-17"=1:42000,"horizontal-18"=1:42000,"horizontal-19"=1:42000,
                              "horizontal-20"=1:42000, "horizontal-21"=1:42000,"horizontal-22"=1:42000,"horizontal-23"=1:42000,
                              "horizontal-24"=1:42000,"horizontal-25"=1:42000,"horizontal-26"=1:42000)
verticalFrame <- data.frame("vertical-3"=1:42000,"vertical-4"=1:42000,"vertical-5"=1:42000,"vertical-6"=1:42000,"vertical-7"=1:42000,
                            "vertical-8"=1:42000,"vertical-9"=1:42000,"vertical-10"=1:42000,"vertical-11"=1:42000,
                            "vertical-12"=1:42000,"vertical-13"=1:42000,"vertical-14"=1:42000,"vertical-15"=1:42000,
                            "vertical-16"=1:42000,"vertical-17"=1:42000,"vertical-18"=1:42000,"vertical-19"=1:42000,
                            "vertical-20"=1:42000, "vertical-21"=1:42000,"vertical-22"=1:42000,"vertical-23"=1:42000,
                            "vertical-24"=1:42000,"vertical-25"=1:42000,"vertical-26"=1:42000)

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
digit.all.features.rawPixel = as.data.frame(cbind(digit.dat, inkFeature, scaledHorizontalFeatures, scaledVerticalFeatures))

colnames(digit.all.features)[1] <- "label"
colnames(digit.all.features)[2] <- "inkFeature"

generalLineFeatures <- function(lines,data,featureFrame){
  i <- 0
  for(elem in lines){
    i <- i + 1
    featureFrame[,i] <- vertical_line(elem,data)
    print(i)
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


##################################################
set.seed(123456)
# load the library
library(mlbench)
library(caret)
library(e1071)
library(randomForest)
# load the data
data(PimaIndiansDiabetes)
# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
results <- rfe(digit.train[,-1], digit.train$label, sizes=c(10,350), rfeControl=control)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))


subset.selection.varnames <- predictors(results)[1:350]