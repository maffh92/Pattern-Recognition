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
threshold <- 300
x <- digit.dat[,-1]
tmp <- apply(x,2,function(a){if(length(a[a!=0]) > threshold) a})
if(class(tmp) == "list"){
  print("remove all null cases from the list")
  tmp <- tmp[!sapply(tmp, is.null)]
}
tmp <- as.data.frame(tmp)
digit.noise.removed <- cbind(digit.train[,1],tmp)
colnames(digit.noise.removed)[1] <- "label"