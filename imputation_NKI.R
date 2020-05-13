#NAs by imputation

# Use raw data with NAs < 6%
data<- raw_no_NA

# Look at the missing data
library(mice)
md.pattern(data)

library(VIM)
aggr_plot <- aggr(data, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

marginplot(data[c(1,2)])

# do imputation
tempData <- mice(data,m=5,maxit=50,meth='pmm',seed=500, print = F)
summary(tempData)

#check questions to see the imputation. Change the lasst bit to view outher questions.
tempData$imp$Image3

# Inspect imputed data
#plot questions against each other.
#yhe shape of the imputed (purple) points should match yhe shape of the observerd (blue) points.
xyplot(tempData,Image1 ~ Value4+Expect2+ServQ2,pch=18,cex=1)

#Inspect via density curves
densityplot(tempData)

#inspect distribution of variables as individual points.
stripplot(tempData, pch = 20, cex = 1.2)


# Extract all imputed matrices and take the mean
raw1<-complete(tempData,1)
raw2<-complete(tempData,2)
raw3<-complete(tempData,3)
raw4<-complete(tempData,4)
raw5<-complete(tempData,5)

raw_mean<-(raw1+raw2+raw3+raw4+raw5)/5

#finish this script and clean up
raw<- raw_mean
rm(pMiss,aggr_plot,data,modelFit1,modelFit2,poolmodelFit2,raw_mean,tempData,tempData2)
rm(raw1,raw2,raw3,raw4,raw5)