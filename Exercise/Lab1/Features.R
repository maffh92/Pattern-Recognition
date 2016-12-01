#combine all the features
digit.all.features <- cbind(digit.dat,inkFeature,scaledHorizontalFeatures,scaledVerticalFeatures)

#All the features
inkDensity <- apply(digit.dat[,-1],1,sum)
inkMeanStats <- tapply(inkDensity,digit.dat$label,mean)
inkSdStats <- tapply(inkDensity,digit.dat$label,sd)

#scale features
ScaledinkSdStats <- scale(inkSdStats)
names(ScaledinkSdStats) <- 0:9
digit.ink <- cbind(digit.dat,inkFeature)

#combine features
#assign to each label to correct ink feature
inkFeature <- 1:nrow(digit.dat)
for(i in 1:nrow(digit.dat)){
  inkFeature[i] <- ScaledinkSdStats[digit.dat[i,]$label]
}


#Change all values to either 0 or 1. This could potentially remove noise
tmp.without.label <- digit.dat[,-1]
tmp.without.label[tmp.without.label<150] <- 0
tmp.without.label[tmp.without.label>=150] <- 1


#remove all the columns below some fixed size of zero values
threshold <- 100
x <- digit.dat[train.index,-1]

tmp <- apply(x,2,function(a){if(length(a[a!=0]) > threshold) a})
if(class(tmp) == "list"){
  print("remove all null cases from the list")
  tmp <- tmp[!sapply(tmp, is.null)]
}
tmp <- as.data.frame(tmp)


digit.noise.removed.train <- cbind(digit.dat[train.index,1],tmp,inkFeature[train.index],verticalFeatures[train.index,],horizontalFeatures[train.index,])
digit.noise.removed.test <- cbind(digit.dat[-train.index,1],digit.dat[-train.index,colnames(tmp)],inkFeature[-train.index],verticalFeatures[-train.index,],horizontalFeatures[-train.index,])


colnames(digit.noise.removed.train)[1] <- "label"
colnames(digit.noise.removed.test) <- colnames(digit.noise.removed.train)


###############
###lines features
###############


horizontalLines <- c(8,12,14,16,20)
verticalLines <- c(10,12,14,16,20)

horizontalFeatures <- generalLineFeatures(horizontalLines,digit.dat,horizontalFrame)
verticalFeatures <- generalLineFeatures(verticalLines,digit.dat,verticalFrame)
scaledHorizontalFeatures <- scale(horizontalFeatures)
scaledVerticalFeatures <- scale(verticalFeatures)

horizontalFrame <- data.frame("horizontal-8"=1:42000,"horizontal-12"=1:42000,"horizontal-14"=1:42000,"horizontal-16"=1:42000,"horizontal-20"=1:42000)
verticalFrame <- data.frame("vertical-10"=1:42000,"horizontal-12"=1:42000,"horizontal-14"=1:42000,"horizontal-16"=1:42000,"horizontal-20"=1:42000)

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