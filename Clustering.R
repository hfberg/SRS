# 
# # Create a correlation matrix from NKI data
# 
# #Originally from website: https://towardsdatascience.com/market-segmentation-with-r-pca-k-means-clustering-part-1-d2c338b1dd0b
# # And website: https://www.r-bloggers.com/pca-and-k-means-clustering-of-delta-aircraft/
# 
# # Modified by Hanna Berg 23-04-2020
# 
# #Library for formatting the code.
# library(bannerCommenter)
# 
# banner("Import and check data", emph = TRUE)
# 
# #Load library to import excel file
# library(xlsx)
# 
# # Give the full path to the data file to import, and which work sheet to use. 
# # Using windows be sure to change backslash from \ to / in path.
# raw <- read.xlsx("C:/Users/habe/Documents/NKI_clustering/2019_Raw_Data_NKI_scale10.xlsx",1, header = T)
# sum_raw<-as.data.frame(summary(raw))
# #optional, to check data
# #str(raw)
# #head(raw)
# 
# #########################################
# 
# # banner("Check and remove matrix for NAs", emph = TRUE)
# 
# # NAs_in_raw<-as.data.frame(colSums(is.na(raw))) #Checking NAs 
# # 
# # # Remove all columns containing at least one NA 
# # # (temporary solution, some answers could maybe be approximated later on.)
# # 
# # rm_cols<-vector()
# # 
# # for (a in (1:NROW(NAs_in_raw))){
# #   if(NAs_in_raw[a,] !=0) {
# #     rm_cols <- append(rm_cols, a)
# #   }
# # }
# # 
# # raw[,rm_cols]<- NULL
# # 
# # # Remove troublesome columns, adapted especially for NKI data, 
# # # disable or customize for other data matrices.
# # raw[,1:2]<-NULL
# # raw[,78]<- NULL
# # 
# # #remove unncessary variables, cuz we like a clean environment.
# # rm(a,rm_cols)
# 
# ############################################
# 
# banner("replace NAs in matrix", emph =T)
# raw_t<-raw
# for (r in (1:nrow(raw))){
#   for (c in (1:ncol(raw))){
#     if (is.na(raw[r,c])){
#       raw_t[r,c]<-round(rowMeans(raw, na.rm = T))[r]
#     }
#   }
# }
# 
# View(raw_t) #inspect the new matrix
# 
# raw<-raw_t
# rm(raw_t,c,r)
# 
# # Scaling is needed if: 
# #     - the units are different between columns
# #     - the means are very different between columns
# #     - the standard deviations are very different between columns
# 
# # Here is code to calculate and plot mean and sd for each column.
# banner("Mean and standard deviation", emph = TRUE)
# 
# mean_in_raw <- c()
# sd_in_raw <-c()
# 
# for ( i in raw[,1:ncol(raw)]){
#   
#   mean_in_raw <- append(mean_in_raw,(mean(i)))
#   sd_in_raw <- append(sd_in_raw, (sd(i)))
# }
# 
# plot(mean_in_raw,xlab= "question index")
# plot(sd_in_raw, xlab= "Question index")
# 
#  ######################### Start here for ready made data

banner("Correlation matrix", emph = TRUE)

library(corrplot)
cormat <- round(cor(raw), 2)
corrplot(cormat, type = c("lower"), tl.cex = 0.3)

banner("PCA", emph = TRUE)
pr_out <- prcomp(raw, center = T, scale = T) #PCA step. Scaling data before PCA is usually advisable! 
#summary(pr_out)

# Screeplot
pr_var <-  pr_out$sdev ^ 2
pve <- pr_var / sum(pr_var)
plot(pve, xlab = "Principal Component", ylab = "Proportion of Variance Explained", ylim = c(0,1), type = 'b')

# Cumulative PVE plot
plot(cumsum(pve), xlab = "Principal Component", ylab = "Cumulative Proportion of Variance Explained", ylim =c(0,1), type = 'b')

# Interpreting results. what does each PC mean?
# Rotate loadings
rot_loading <- varimax(pr_out$rotation[, 1:5])
rot_loading

# Variable quality on PC
variable_Q<-rot_loading[["loadings"]]^2

# Apply threshold to plot only the most important variables
rm_rows = 0
thr = 0.09
for (i in 1:nrow(variable_Q)){
  if (sum(variable_Q[i,])<thr){
    rm_rows<-append(rm_rows,i)
  }
}

#remove rows with a sum below the threshold
variable_Q<-variable_Q[-rm_rows,]
variable_Q<-variable_Q[order(variable_Q[,1],decreasing=T),]
rm(rm_rows, thr)

# Plot most important question for each PC.
library("corrplot")
corrplot(variable_Q,is.corr = F)

# Decide what every axis means based on the previous quality plot.
PC1_axis = "PC1: Image"
PC2_axis = "PC2: Social reponsibility"
PC3_axis = "PC3: Lojalitet"
PC4_axis = "PC4: Varde/peng" 
PC5_axis = "PC5: Installning/kannedom" 

#Select number of PCs
no_of_PCs<- 5
PCAs<-data.frame(pr_out$x[,1:no_of_PCs])
colnames(PCAs)<-c(PC1_axis,PC2_axis,PC3_axis, PC4_axis, PC5_axis)


banner("Clustering with K-means", emph = TRUE)
# Determine number of clusters
predict_no_clust <- (nrow(raw)-1)*sum(apply(raw,2,var))
for (i in 2:15) predict_no_clust[i] <- sum(kmeans(raw,
                                     centers=i)$withinss)
plot(1:15, predict_no_clust, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

# K-means clustering
# Determine number of clusters from scree plot. 
# Where does the elbow occur?
# Apply k-means with k=elbow number
k <- kmeans(PCAs, 12, nstart=25, iter.max=1000)
library(RColorBrewer)
library(scales)
palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot(PCAs, col=k$clust, pch=16)

# Plot in 3D and save to disk
library("rgl")


plot3d(PCAs, col=k$clust, pch=16)

# # Generates a non interactive plot called "plot"
# snapshot3d("plot.png")
# #postscript("C:/Users/habe/Documents/NKI_clustering/plot")
# 
# 
# # Generates a website with an interactive plot.
# writeWebGL(dir = "webGL", filename = file.path("webGL/index.html"))
# browseURL(
#   paste("file://", writeWebGL(dir=file.path(tempdir(), "webGL"), 
#                               width=500), sep="")
# )
# 
# 
# 
# banner("Lable customers", emph = TRUE)
# 
# #Load lables
# customer_lables = read.xlsx(file = "NKI_legends.xlsx",1)
# 
# plot(PCAs[,1:2], col = customer_lables[,4], pch=16)
# legend("topright",legend = unique(customer_lables[,4]),  fill = c("blue", "red") )
# 
# ## With ggplot
# PCAs_t3<-cbind(PCAs_t,customer_lables[,3])
# 
# # A basic scatterplot with color depending on Species
# ggplot(PCAs_t3[,1:2], aes(x=PCAs_t3[,1], y=PCAs_t3[,2], color=PCAs_t3[,3])) + geom_point(size=1) 
