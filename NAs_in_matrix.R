# Deal with NAs in the matrix


library(xlsx)  
raw <- read.xlsx("C:/Users/habe/Documents/NKI_clustering/2019_Raw_Data_NKI_scale10.xlsx",1, header = T)

# If a question has more than 5% NAs it's not very reliable. Calculate that.

pMiss <- function(x){sum(is.na(x))/length(x)*100}
plot(apply(raw,2,pMiss), xlab = "quesition index", ylab = "Percent of NAs")
plot(apply(raw,1,pMiss), xlab = "customer index",ylab = "Percent of NAs")

# subset to a data matrix with columns with less than 5% NAs
#a <- apply(raw,2,pMiss)>6
#raw_quest5 <-subset(raw,select = -which(a, arr.ind = T, useNames = T))
#rm(a)


# subset to a data matrix with rows with less than 5% NAs
raw_cust5 <-subset(raw,subset = apply(raw,1,pMiss)<6)

# Check the plot again to see if the thresholds needs to be 
# changed a little to allow more customers.
plot(apply(raw_cust5,1,pMiss), xlab = "quesition index", ylab = "Percent of NAs")

#Check the plot to see if any question needs to be removed.
plot(apply(raw_cust5,2,pMiss), xlab = "quesition index", ylab = "Percent of NAs")

#remove questions with too many NAs

raw_cust5 <-subset(raw_cust5,select = -c(17,38))

customer_lables = read.xlsx(file = "NKI_legends.xlsx",1)
customer_lables = subset(customer_lables, subset = apply(raw,1,pMiss)<6) 


raw<-raw_cust5

raw_t<-raw
for (r in (1:nrow(raw))){
  for (c in (1:ncol(raw))){
    if (is.na(raw[r,c])){
      raw_t[r,c]<-round(rowMeans(raw, na.rm = T))[r]
      }
  }
}


raw<-raw_t
rm(raw_t,c,r)

#



